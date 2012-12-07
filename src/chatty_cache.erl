-module(chatty_cache).

-export([resolve_tree/1]).
-export([resolve_tree_json/1]).
-export([comment/1, comment_expire/1, comment_fetch/1]).
-export([json_hot/2, json_controversy/2, json_confidence/2]).

-export([comment_tree_json/1, comment_tree_raw/1, comment_tree_expire/1]).

% used by inline exported funs
-export([node_to_map/1]).

-define(BUCKET, <<"chatty">>).

% ALTERNATIVE CACHING OPTIONS:
%  • Store each top level sub-tree in ets; assemble the tree on each request
%    • This would allow us to expire sub-trees when a new comment is made
%      instead of re-rendering the *entire* comment tree.
%  • If generating the comment tree is a bottleneck, have a background process
%    pull it, generate it, and stuff it into the cache every X interval.
%
%%%----------------------------------------------------------------------
%%% Resolve a Comment Tree
%%%----------------------------------------------------------------------
comment_tree_json({TreeId, N}) when is_list(TreeId) ->
  comment_tree_json({list_to_binary(TreeId), N});
comment_tree_json(TreeId) when is_list(TreeId) ->
  comment_tree_json(list_to_binary(TreeId));
comment_tree_json({TreeId, N}) when is_binary(TreeId) ->
  resolve_tree_json({TreeId, N});
%  ecache:get(chatty_tree_json, {TreeId, N});
comment_tree_json(TreeId) when is_binary(TreeId) ->
  resolve_tree_json(TreeId).
%  ecache:get(chatty_tree_json, TreeId).

comment_tree_raw({TreeId, N}) when is_list(TreeId) ->
  comment_tree_raw({list_to_binary(TreeId), N});
comment_tree_raw(TreeId) when is_list(TreeId) ->
  comment_tree_raw(list_to_binary(TreeId));
comment_tree_raw({TreeId, N}) when is_binary(TreeId) ->
%  ecache:get(chatty_tree, {TreeId, N});
  resolve_tree({TreeId, N});
comment_tree_raw(TreeId) when is_binary(TreeId) ->
  resolve_tree(TreeId).
%  ecache:get(chatty_tree, TreeId).

comment_tree_expire(TreeId) when is_list(TreeId) ->
  comment_tree_expire(list_to_binary(TreeId));
comment_tree_expire(TreeId) when is_binary(TreeId) ->
  ecache:dirty(chatty_tree_json, TreeId),
  ecache:dirty(chatty_tree_json, {TreeId, 1}),  % only board entries
  ecache:dirty(chatty_tree, TreeId),
  ecache:dirty(chatty_tree, {TreeId, 1}).       % only board entries

resolve_tree({TreeId, DepthLimit}) when is_binary(TreeId) ->
%  io:format("Resolving tree: ~p~n", [{TreeId, DepthLimit}]),
  PreTree = cghost:object_resolve_to_depth(TreeId, 2500, DepthLimit),
  chatty:comment_tree_map(PreTree, fun(X) -> X end);
resolve_tree(TreeId) ->
  resolve_tree({TreeId, 1000}).

resolve_tree_json({TreeId, DepthLimit}) when is_binary(TreeId) ->
%  io:format("Resolving tree JSON: ~p~n", [{TreeId, DepthLimit}]),
  PreTree = cghost:object_resolve_to_depth(TreeId, 2500, DepthLimit),
  JsonMapTree = chatty:comment_tree_map(PreTree, fun node_to_map/1),
  Encoder = mochijson2:encoder([{utf8, true}]),
  iolist_to_binary(Encoder(JsonMapTree));
resolve_tree_json(TreeId) ->
  resolve_tree_json({TreeId, 1000}).

node_to_map({Key, Uid, TS, Replaced, VoteCount,
             CommentText, TotalChildren, Children}) ->
  [{id, Key}, {uid, Uid}, {ts, TS}, {vc, VoteCount}, {text, CommentText},
   {replaced, Replaced}, {tc, TotalChildren}, {children, Children}].

%%%----------------------------------------------------------------------
%%% Board Jsons
%%%----------------------------------------------------------------------
json_hot(BoardId, N) ->
  top_n_json(BoardId, N, fun chatty:top_n_hot/2).

json_controversy(BoardId, N) ->
  top_n_json(BoardId, N, fun chatty:top_n_controversy/2).

json_confidence(BoardId, N) ->
  top_n_json(BoardId, N, fun chatty:top_n_confidence/2).

top_n_json(BoardId, N, Fun) ->
  CommentIds = Fun(BoardId, N),
  Comments = [{Id, chatty_cache:comment(Id)} || Id <- CommentIds],
  % Vote Counts aren't available here -- we'd have to look them up again,
  % but we don't really need them, do we?
  FormattedComments =[[{uid, Uid}, {ts, TS},
                       {text, chatty:md(Text)}, {id, Id}] ||
                      {Id, {Uid, TS, _, Text}} <- Comments],
  Encoder = mochijson2:encoder([{utf8, true}]),
  iolist_to_binary(Encoder(FormattedComments)).

%%%----------------------------------------------------------------------
%%% Cache of Individual Comments
%%%----------------------------------------------------------------------
-compile({inline, [quickfetch/2]}).
quickfetch(Proplist, Key) when is_list(Proplist) ->
  case lists:keyfind(Key, 1, Proplist) of
    {Key, Val} -> list_to_binary(Val);
         false -> error
  end.

comment(Key) ->
  ecache:get(chatty_comment, Key).

comment_expire(Key) ->
  ecache:dirty(chatty_comment, Key).

comment_fetch(Key) when is_binary(Key) ->
  case riak_pool:get(chatty, ?BUCKET, Key) of
    {ok, RiakObj} -> MD = riakc_obj:get_metadata(RiakObj),
                     MetaProplist = dict:fetch(<<"X-Riak-Meta">>, MD),
                     Uid = quickfetch(MetaProplist, "uid"),
                     TS = quickfetch(MetaProplist, "ts"),
                     Replaced = case quickfetch(MetaProplist, "replaced") of
                                  error -> [];
                                      V -> V
                                end,
                     {Uid, TS, Replaced, riakc_obj:get_value(RiakObj)};
    {error, notfound} -> notfound
  end.

%%%----------------------------------------------------------------------
%%% Cache of Comment Tree and Sub-Trees
%%%----------------------------------------------------------------------

% this would be inefficient because we can't share cached sub-trees with
% the cached primary tree.  We'd have everything duplicated many times.


