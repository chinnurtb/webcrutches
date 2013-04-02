%%%------------------------------------------------------------------------
%%% @doc WebCrutches Chain call support
%%% Created: 2013-04-02 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(wcr_chain).

-export([ chain_run/3
        , chain_success/1, chain_success/2
        , chain_fail/1, chain_fail/2
        ]).

-include("wcr_internal.hrl").

%%%------------------------------------------------------------------------
-type handler_fun_t() :: fun((cowboy_req:req(), wcr:handler_state()) ->
                                ssweb:chain_return()).

-spec chain_run(FunList :: [handler_fun_t()],
                Req :: cowboy_req:req(),
                State :: wcr:handler_state()) ->
                   {ok, cowboy_req:req(), wcr:handler_state()} |
                   {error, cowboy_req:req(), wcr:handler_state()}.

%% @doc Runs list of functions passing opaque state through them and stopping
%% if any of functions returns error.
chain_run([], Req, State) -> {ok, Req, State};
chain_run([F | Tail], Req, State) ->
  case F(Req, State) of
    {chain_ok, Req2, State2}   -> chain_run(Tail, Req2, State2);
    {chain_fail, Req3, State3} -> {error, Req3, State3}
  end.

chain_success(Req, State = #wcr_handler_state{}) -> {chain_ok, Req, State}.
chain_success({Req, State = #wcr_handler_state{}}) -> {chain_ok, Req, State}.

chain_fail(Req, State = #wcr_handler_state{}) -> {chain_fail, Req, State}.
chain_fail({Req, State = #wcr_handler_state{}}) -> {chain_fail, Req, State}.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
