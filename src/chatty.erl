-module(chatty).

-export([discussion_object_new/1]).
-export([comment/4, comment/5, update_comment/5, update_comment/6]).
-export([update_story/5, update_story/6]).
-export([comments/1]).
-export([comment_uid/1]).
-export([comment_tree_map/2]).
-export([upvote/4, upvote/5]).
-export([downvote/4, downvote/5]).
-export([upvote_story/3, downvote_story/3]).
-export([top_n_hot/2, top_n_confidence/2, top_n_controversy/2]).

-export([score_confidence/2]).

-export([md/1]).  % used by cache to auto-md things

-define(BUCKET, <<"chatty">>).

% this is: calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(ERLANG_GS_EPOCH_OFFSET, 62167219200).

%%%----------------------------------------------------------------------
%%% New Discussions (basically a new discussion board entirely)
%%%----------------------------------------------------------------------
discussion_object_new(Owner) ->
  cghost:object_create(Owner, Owner),
  Owner.

%%%----------------------------------------------------------------------
%%% Write Comment
%%%----------------------------------------------------------------------
% Expire the tree cache after this comment posts
comment({RootTreeId, ParentId}, CommentId, UserId, CommentText) ->
  comment(ParentId, CommentId, UserId, CommentText, now()),
  chatty_cache:comment_tree_expire(ParentId),
  chatty_cache:comment_tree_expire(RootTreeId);
% Just add the comment without expiring the tree cache
comment(ParentId, CommentId, UserId, CommentText) ->
  comment(ParentId, CommentId, UserId, CommentText, now()).

comment(ParentId, CommentId, UserId, CommentText, TS) ->
  comment(ParentId, CommentId, UserId, CommentText, TS, []).

comment(ParentId, CommentId, UserId, CommentText, TS, ExtraMetadata)
    when is_list(ExtraMetadata) ->
  PreObj = riakc_obj:new(?BUCKET, CommentId, CommentText),
  MetaProplist = [{"uid", UserId},
                  {"ts", iso8601:format(TS)} | ExtraMetadata],
  Metadata = dict:from_list([{<<"X-Riak-Meta">>, MetaProplist}]),
  Obj = riakc_obj:update_metadata(PreObj, Metadata),
  riak_pool:put(chatty, Obj),
  cghost:object_parent(ParentId, CommentId),
  CommentId.

%%%----------------------------------------------------------------------
%%% Update/Override Comment
%%%----------------------------------------------------------------------
% Expire the tree cache after this comment updates
update_comment({RootTreeId, ParentId}, CommentId,
    UserId, CommentText, OldCommentId) ->
  update_comment(ParentId, CommentId, UserId, CommentText, OldCommentId, now()),
  chatty_cache:comment_tree_expire(ParentId),
  chatty_cache:comment_tree_expire(RootTreeId);
% Just update the comment without expiring the tree cache
update_comment(ParentId, CommentId, UserId, CommentText, OldCommentId) ->
  update_comment(ParentId, CommentId, UserId, CommentText, OldCommentId, now()).

update_comment(ParentId, CommentId, UserId, CommentText, OldCommentId, TS) ->
  AdditionalMetadata = [{"replaced", OldCommentId}],
  comment(ParentId, CommentId, UserId, CommentText, TS, AdditionalMetadata),
  cghost:object_rename(ParentId, OldCommentId, CommentId),
  er:zrem(redis_chatty, key_rank_confidence(ParentId), OldCommentId),
  er:zrem(redis_chatty, key_rank_controversy(ParentId), OldCommentId),
  update_rank_controversy(ParentId, CommentId),
  update_rank_confidence(ParentId, CommentId),
  chatty_cache:comment_expire(OldCommentId),
  CommentId.

update_story(BoardId, StoryId, UserId, CommentText, OldCommentId) ->
  update_story(BoardId, StoryId, UserId, CommentText, OldCommentId, now()).

update_story(BoardId, StoryId, UserId, CommentText, OldCommentId, TS) ->
  U = update_comment(BoardId, StoryId, UserId, CommentText, OldCommentId, TS),
  er:zrem(redis_chatty, key_rank_hot(BoardId), OldCommentId),
  update_rank_hot(BoardId, StoryId),
  U.

% TODO:
%  • union comment threads
%  • add comment -> list of parents map (for relocatable comments)

%%%----------------------------------------------------------------------
%%% Reformaters
%%%----------------------------------------------------------------------
md(Text) ->
  run_if_pid(sundown, mochiweb_html:escape(Text)).

-compile({inline, [{run_if_pid, 2}]}).
run_if_pid(Name, Content) when is_atom(Name) ->
  case whereis(Name) of
    N when is_pid(N) -> stdinout:send(N, Content);
                   _ -> Content
  end.

%%%----------------------------------------------------------------------
%%% Read Comments
%%%----------------------------------------------------------------------
comments(ParentId) ->
  chatty_cache:comment_tree_json(ParentId).

comment_tree_map(Tree, NodeFun) when is_function(NodeFun) ->
  comment_tree_map(Tree, NodeFun, []).

comment_tree_map([{Key, VoteCount, Children} | T], Fun, Accum)
    when is_function(Fun) andalso is_list(Accum) ->
  case chatty_cache:comment(Key) of
    notfound -> comment_tree_map(T, Fun, [null | Accum]);
    {Uid, TS, ReplacedBy, CommentText} ->
      Childrens = case Children of
                    cycle -> [];
                       [] -> [];
                        _ -> comment_tree_map(Children, Fun)
                  end,
      Transformed = Fun({Key, Uid, TS, ReplacedBy,
                         VoteCount, md(CommentText), Childrens}),
      comment_tree_map(T, Fun, [Transformed | Accum])
  end;
comment_tree_map([], _, Accum) when is_list(Accum) ->
  lists:reverse(Accum).

comment_uid(CommentId) ->
  case chatty_cache:comment(CommentId) of
    {Uid, _, _, _} -> Uid;
          notfound -> none
  end.

%%%----------------------------------------------------------------------
%%% Voting
%%%----------------------------------------------------------------------
upvote(RootId, ParentId, CommentId, UserId) ->
  upvote(RootId, ParentId, CommentId, UserId, 1).

upvote(RootId, ParentId, CommentId, UserId, Weight) ->
  % do any checks for banned from upvoting?
  U = cghost:vote(up, Weight, ParentId, CommentId, UserId),
  comment_vote_common(RootId, ParentId, CommentId),
  U.

downvote(RootId, ParentId, CommentId, UserId) ->
  downvote(RootId, ParentId, CommentId, UserId, 1).

downvote(RootId, ParentId, CommentId, UserId, Weight) ->
  % do any checks for banned from downvoting?
  D = cghost:vote(down, Weight, ParentId, CommentId, UserId),
  comment_vote_common(RootId, ParentId, CommentId),
  D.

comment_vote_common(RootId, ParentId, CommentId) ->
  update_rank_confidence(ParentId, CommentId),
  update_rank_controversy(ParentId, CommentId),
  case RootId of
          [] -> ok;
    ParentId -> ok; % expired below
           _ -> chatty_cache:comment_tree_expire(RootId)
  end,
  chatty_cache:comment_tree_expire(ParentId),
  chatty_cache:comment_expire(CommentId).

upvote_story(BoardId, StoryId, UserId) ->
  U = upvote(BoardId, BoardId, StoryId, UserId),
  story_vote_common(BoardId, StoryId),
  U.

downvote_story(BoardId, StoryId, UserId) ->
  D = downvote(BoardId, BoardId, StoryId, UserId),
  story_vote_common(BoardId, StoryId),
  D.

story_vote_common(BoardId, StoryId) ->
  update_rank_hot(BoardId, StoryId).

update_rank_hot(BoardId, StoryId) ->
  {_, TS, _, _} = chatty_cache:comment(StoryId),
  DateTime = iso8601:parse(TS),
  DateTimeAsGS = calendar:datetime_to_gregorian_seconds(DateTime),
  % cal:datetime gives us seconds since year 0, not since 1970.  Recompute here:
  DateTimeAsEpoch = DateTimeAsGS - ?ERLANG_GS_EPOCH_OFFSET,
  Hotness = comment_score_hot(BoardId, StoryId, DateTimeAsEpoch),
  zadd(key_rank_hot(BoardId), Hotness, StoryId).

update_rank_confidence(ParentId, CommentId) ->
  Confidence = comment_score_confidence(ParentId, CommentId),
  zadd(key_rank_confidence(ParentId), Confidence, CommentId).

update_rank_controversy(ParentId, CommentId) ->
  Confidence = comment_score_controversy(ParentId, CommentId),
  zadd(key_rank_controversy(ParentId), Confidence, CommentId).

%%%----------------------------------------------------------------------
%%% Local Redis Keys
%%%----------------------------------------------------------------------
key_rank(ParentId, What) ->
  eru:er_key(chatty, board, ParentId, What).

key_rank_controversy(ParentId) ->
  key_rank(ParentId, controversy).

key_rank_confidence(ParentId) ->
  key_rank(ParentId, confidence).

key_rank_hot(BoardId) ->
  key_rank(BoardId, hot).

%%%----------------------------------------------------------------------
%%% Local Redis Zset Adding
%%%----------------------------------------------------------------------
zadd(RedisKey, Score, CommentId) when is_number(Score) ->
  UseScore = mochinum:digits(Score),
  er:zadd(redis_chatty, RedisKey, UseScore, CommentId).

%%%----------------------------------------------------------------------
%%% Local Redis Zset Reading
%%%----------------------------------------------------------------------
top_n(RedisKey, N) ->
  er:zrevrange(redis_chatty, RedisKey, 0, N+1).

top_n_hot(BoardId, N) ->
  top_n(key_rank_hot(BoardId), N).

top_n_controversy(BoardId, N) ->
  top_n(key_rank_controversy(BoardId), N).

top_n_confidence(BoardId, N) ->
  top_n(key_rank_confidence(BoardId), N).

%%%----------------------------------------------------------------------
%%% Scoring Comments and Posts
%%%----------------------------------------------------------------------
to_integer(nil) -> 0;
to_integer(N) when is_binary(N) ->
  to_integer(binary_to_list(N));
to_integer(N) when is_list(N) ->
  list_to_integer(N);
to_integer(N) when is_integer(N) -> N.

updown(ParentId, CommentId) ->
  {Ups, Downs} = cghost:votes_updown(ParentId, CommentId),
  {to_integer(Ups), to_integer(Downs)}.

score(Ups, Downs) when is_integer(Ups) andalso is_integer(Downs) ->
  Ups - Downs.

comment_score_hot(ParentId, CommentId, PostedEpoch) ->
  {Ups, Downs} = updown(ParentId, CommentId),
  score_hot(Ups, Downs, PostedEpoch).

epoch({Megasecs, Secs, _}) ->
  (Megasecs * 1000000) + Secs;
epoch(X) when is_integer(X) -> X;
epoch(X) when is_list(X) -> to_integer(X);
epoch(X) when is_binary(X) -> to_integer(X).

posted_epoch_minus_base_epoch(PostedEpoch) ->
  BaseEpoch = 1343779200, % August 1, 2012.
  epoch(PostedEpoch) - BaseEpoch.

score_hot(Ups, Downs, PostedEpoch) ->
  Score = score(Ups, Downs),
  Order = math:log10(max(abs(Score), 1)),
  Sign = if
            Score > 0 ->  1;
            Score < 0 -> -1;
           Score == 0 ->  0
         end,
  Seconds = posted_epoch_minus_base_epoch(PostedEpoch),
  Order + (Sign * Seconds / 50400). % 50400 = 14 hours in seconds

comment_score_controversy(ParentId, CommentId) ->
  {Ups, Downs} = updown(ParentId, CommentId),
  score_controversy(Ups, Downs).

score_controversy(Ups, Downs) when is_number(Ups) andalso is_number(Downs) ->
  (Ups + Downs) / max(abs(score(Ups, Downs)), 1).

comment_score_confidence(ParentId, CommentId) ->
  {Ups, Downs} = updown(ParentId, CommentId),
  score_confidence(Ups, Downs).  % use the pre-built cache

% NB: the confidence score *only* depends on Upvote and Downvote counts
% So, if it ever made sense, we could cache a lot of the values up front.
% But right now, it takes 5-7us to run this calculation and 6us to retrieve
% a value from ets.  It makes more sense to keep things simple and just do
% some match each time.
score_confidence(Ups, Downs) ->
  N = Ups + Downs,
  case N of
    0 -> -Downs;  % downvotes push the object further down
    _ -> Confidence = 1.281551565545, % 80% Confidence  (1.64485 = 95%),
         Z = Confidence,
         P = Ups / N,
         Left = P + Z*Z/(2*N),
         Right = Z * math:sqrt((P * (1 - P)/N) + (Z*Z/(4*N*N))),
         Under = 1+Z*Z/N,
         (Left - Right) / Under
  end.
