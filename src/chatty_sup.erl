-module(chatty_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

supname(Name) ->
  list_to_atom(atom_to_list(Name) ++ "_sup").

er_sup(Name, IP, Port) ->
  SupName = supname(Name),
  {SupName,
   {er_pool, start_link, [Name, IP, Port]},
    permanent, 5000, worker, [er_pool]}.

cache_if_no_cache(Name, FunName) ->
  case whereis(Name) of
    undefined -> ecache:cache_sup(Name, chatty_cache, FunName, 16);
            _ -> []
  end.

init([]) ->
  TreeCache = cache_if_no_cache(chatty_tree, resolve_tree),
  TreeJsonCache = cache_if_no_cache(chatty_tree_json, resolve_tree_json),
  CommentCache = cache_if_no_cache(chatty_comment, comment_fetch),
  RedisChatty = er_sup(redis_chatty, "127.0.0.1", 6383),

  Processes = [TreeCache,
               TreeJsonCache,
               CommentCache,
               RedisChatty],

  Strategy = {one_for_one, 10, 10},
  {ok,
   {Strategy, lists:flatten(Processes)}}.
