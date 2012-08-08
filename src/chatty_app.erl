-module(chatty_app).

-behaviour(application).
-export([start/2,stop/1]).

-spec start(any(), any()) -> any().
start(_Type, _StartArgs) ->
  application:start(crypto),
  application:start(riak_pool),
  chatty_sup:start_link().

-spec stop(any()) -> any().
stop(_State) ->
  ok.
