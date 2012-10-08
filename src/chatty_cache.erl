-module(chatty_cache).

-export([resolve_tree/1]).
-export([resolve_tree_json/1]).
-export([comment/1, comment_expire/1, comment_fetch/1]).

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
comment_tree_json(TreeId) ->
  ecache:get(chatty_tree_json, TreeId).

comment_tree_raw(TreeId) ->
  ecache:get(chatty_tree, TreeId).

comment_tree_expire(TreeId) ->
  ecache:dirty(chatty_tree_json, TreeId),
  ecache:dirty(chatty_tree, TreeId).

resolve_tree(TreeId) ->
  PreTree = rghost:object_resolve_to_height(TreeId, 1000),
  chatty:comment_tree_map(PreTree, fun(X) -> X end).

resolve_tree_json(TreeId) ->
  PreTree = rghost:object_resolve_to_height(TreeId, 1000),
  JsonMapTree = chatty:comment_tree_map(PreTree, fun node_to_map/1),
  Encoder = mochijson2:encoder([{utf8, true}]),
  iolist_to_binary(Encoder(JsonMapTree)).

node_to_map({Key, Uid, TS, Replaced, VoteCount, CommentText, Children}) ->
  [{id, Key}, {uid, Uid}, {ts, TS}, {vc, VoteCount}, {text, CommentText},
   {replaced, Replaced}, {children, Children}].

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


