-module(chatty_tests).

-include_lib("eunit/include/eunit.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
chatty_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
     {"Create new Question (1) on Course A",
       fun create_question_1/0},
     {"Reply to Question 1 on Course A",
       fun create_reply_to_question_1/0},
     {"Replace text of Question 1 on Course A",
       fun replace_question_1_text/0},
     {"Upvote Question 1 on Course A",
       fun upvote_question_1/0},
     {"Replace text on Question 1 on Course A again",
       fun replace_question_1_again/0},
     {"Upvote Question 1 on Course A again",
       fun upvote_question_1_again/0},
     {"Reply to Reply 1 of Question 1 on Course A",
       fun reply_to_reply_1/0},
     {"Upvote Reply 1 of Question 1 on Course A",
       fun upvote_reply_to_reply_to_question_1/0},
     {"Check Comment Tree",
       fun comment_tree/0},
     {"Create Question 2",
       fun create_question_2/0},
     {"Upvote Question 2 on Course A",
       fun upvote_question_2/0},
     {"Check Question Hotness Ranks",
       fun question_ranks_hotness/0},
     {"Check Question Controversy Ranks",
       fun question_ranks_controversy/0},
     {"Check Question Confidence Ranks",
       fun question_ranks_confidence/0},
     {"Downvote Question 2 on Course A",
       fun downvote_question_2/0},
     {"Check Question Hotness Ranks After Downvote",
       fun question_ranks_after_downvote_hotness/0},
     {"Check Question Controversy Ranks After Downvote",
       fun question_ranks_after_downvote_controversy/0},
     {"Check Question Confidence Ranks After Downvote",
       fun question_ranks_after_downvote_confidence/0}
    ]
  }.

-define(TS,  {1344,439344,604328}).
-define(TS2, {1344,439944,604328}). % a few minutes later
%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
create_question_1() ->
  % courseA -> 1
  CommentId = chatty:comment(<<"courseA">>, <<"1">>,
                             <<"userID">>, <<"commentTextQuestion1">>, ?TS),
  ?assertEqual(<<"1">>, CommentId).

create_reply_to_question_1() ->
  % courseA -> 1 -> 2
  CommentId =
    chatty:comment(<<"1">>, <<"2">>, <<"userID2">>, 
                   <<"replyToQuestion1">>, ?TS),
  ?assertEqual(<<"2">>, CommentId).

replace_question_1_text() ->
  % courseA -> 3 -> 2
  % Here, (courseA, 1) ceases to exist and is replaced by (courseA, 3)
  CommentId =
    chatty:update_story(<<"courseA">>, <<"3">>,
                        <<"userID">>, <<"replacedCommentTextQuestion1">>,
                        <<"1">>, ?TS),
  ?assertEqual(<<"3">>, CommentId).

upvote_question_1() ->
  % courseA -> 3 -> 2
  OriginalScore = rghost:vote_total(<<"courseA">>, <<"3">>),
  NewScore = chatty:upvote_story(<<"courseA">>, <<"3">>, <<"userID">>),
  ?assertEqual(<<"0">>, OriginalScore),
  ?assertEqual(1, NewScore).

replace_question_1_again() ->
  % courseA -> 4 -> 2
  % Here, (courseA, 3) ceases to exist and is replaced by (courseA, 4)
  CommentId =
    chatty:update_story(<<"courseA">>, <<"4">>,
                        <<"userID">>, <<"replacedCommentTextQuestion1">>,
                        <<"3">>, ?TS),
  ?assertEqual(<<"4">>, CommentId).

upvote_question_1_again() ->
  % courseA -> 4 -> 2
  OriginalScore = rghost:vote_total(<<"courseA">>, <<"4">>),
  NewScore = chatty:upvote_story(<<"courseA">>, <<"4">>, <<"userID">>),
  ?assertEqual(<<"1">>, OriginalScore),
  ?assertEqual(2, NewScore).

reply_to_reply_1() ->
  % courseA -> 4 -> 2 -> 5
  CommentId =
    chatty:comment(<<"2">>, <<"5">>, <<"userID2">>,
                   <<"repToRplToQuestion1">>, ?TS),
  ?assertEqual(<<"5">>, CommentId).

upvote_reply_to_reply_to_question_1() ->
  % courseA -> 4 -> 2 -> 5
  OriginalScore = rghost:vote_total(<<"2">>, <<"5">>),
  NewScore = chatty:upvote(<<"2">>, <<"5">>, <<"userID">>),
  ?assertEqual(<<"0">>, OriginalScore),
  ?assertEqual(1, NewScore).

comment_tree() ->
  % courseA -> 4 -> 2 -> 5
  CommentTreeJson = chatty:comments(courseA),
  CommentTreeExpected = <<"[{\"id\":\"4\",\"uid\":\"userID\",\"ts\":\"2012-08-08T15:22:24Z\",\"vc\":\"2\",\"text\":\"replacedCommentTextQuestion1\",\"replaced\":\"3\",\"children\":[{\"id\":\"2\",\"uid\":\"userID2\",\"ts\":\"2012-08-08T15:22:24Z\",\"vc\":\"0\",\"text\":\"replyToQuestion1\",\"replaced\":[],\"children\":[{\"id\":\"5\",\"uid\":\"userID2\",\"ts\":\"2012-08-08T15:22:24Z\",\"vc\":\"1\",\"text\":\"repToRplToQuestion1\",\"replaced\":[],\"children\":[]}]}]}]">>,
  ?assertEqual(CommentTreeExpected, CommentTreeJson).

create_question_2() ->
  % courseA -> {1, question2}
  CommentId = chatty:comment(<<"courseA">>, <<"question2">>,
                             <<"userID">>, <<"commentTextQuestion2222">>, ?TS2),
  ?assertEqual(<<"question2">>, CommentId).

upvote_question_2() ->
  % courseA -> question2
  OriginalScore = rghost:vote_total(<<"courseA">>, <<"question2">>),
  NewScore = chatty:upvote_story(<<"courseA">>, <<"question2">>, <<"userID">>),
  NewerScore = chatty:upvote_story(<<"courseA">>, <<"question2">>, <<"userID">>),
  ?assertEqual(<<"0">>, OriginalScore),
  ?assertEqual(1, NewScore),
  ?assertEqual(2, NewerScore).

question_ranks_hotness() ->
  % courseA -> {question2, 4}
  Hotness = chatty:top_n_hot(<<"courseA">>, 32),
  ?assertEqual([<<"question2">>, <<"4">>], Hotness).

question_ranks_controversy() ->
  % courseA -> {question2, 4}
  Hotness = chatty:top_n_controversy(<<"courseA">>, 32),
  ?assertEqual([<<"question2">>, <<"4">>], Hotness).

question_ranks_confidence() ->
  % courseA -> {question2, 4}
  Hotness = chatty:top_n_confidence(<<"courseA">>, 32),
  ?assertEqual([<<"question2">>, <<"4">>], Hotness).

downvote_question_2() ->
  % courseA -> question2
  OriginalScore = rghost:vote_total(<<"courseA">>, <<"question2">>),
  NewScore = chatty:downvote_story(<<"courseA">>, <<"question2">>, <<"userID">>),
  NewerScore = chatty:downvote_story(<<"courseA">>, <<"question2">>, <<"userID">>),
  NewestScore = chatty:downvote_story(<<"courseA">>, <<"question2">>, <<"userID">>),
  ?assertEqual(<<"2">>, OriginalScore),
  ?assertEqual(1, NewScore),
  ?assertEqual(0, NewerScore),
  ?assertEqual(-1, NewestScore).

question_ranks_after_downvote_hotness() ->
  % courseA -> {question2, 4}
  Hotness = chatty:top_n_hot(<<"courseA">>, 32),
  ?assertEqual([<<"4">>, <<"question2">>], Hotness).

question_ranks_after_downvote_controversy() ->
  % courseA -> {question2, 4}
  Hotness = chatty:top_n_controversy(<<"courseA">>, 32),
  ?assertEqual([<<"question2">>, <<"4">>], Hotness).

question_ranks_after_downvote_confidence() ->
  % courseA -> {question2, 4}
  Hotness = chatty:top_n_confidence(<<"courseA">>, 32),
  ?assertEqual([<<"4">>, <<"question2">>], Hotness).

%%%----------------------------------------------------------------------
%%% Set it up, tear it down
%%%----------------------------------------------------------------------
setup() ->
  application:start(er),
  er_pool:start_link(redis_ghost, "127.0.0.1", 6389),
  er:flushall(redis_ghost),
  application:start(riak_pool),
  poolboy:start_link([{name, {local, chatty}},
                      {worker_module, riak_pool_worker},
                      {pool_size, 10},
                      {max_overflow, 20},
                      {host, "127.0.0.1"},
                      {port, 8087}]),
  % I hope you don't want anything in your 'chatty' bucket...
  {ok, Keys} = riak_pool:list_keys(chatty, <<"chatty">>),
  [riak_pool:delete(chatty, <<"chatty">>, K) || K <- Keys],
  application:start(chatty).

teardown(_) ->
  application:stop(chatty),
  application:stop(riak_pool),
  application:stop(er).
