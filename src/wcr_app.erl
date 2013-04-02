%%%------------------------------------------------------------------------
%%% @doc Defines start point for application, also starts dependencies
%%% Created: 2013-04-02 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(wcr_app).
-behaviour(application).

%% Application callbacks
-export([ start/0
        , start/2
        , stop/1
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  wcr:ensure_started(sasl),
  wcr:ensure_started(gproc),
  lager:start(),
  application:start(webcrutches).

start(_StartType, _StartArgs) ->
  start_web(),
  wcr_sup:start_link().

stop(_State) ->
  ok.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
