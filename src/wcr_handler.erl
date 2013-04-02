%%%------------------------------------------------------------------------
%%% @doc WebCrutches Chain call support
%%% Created: 2013-04-02 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(wcr_handler).

-export([ init/3
        , handle/3
        ]).

-include("wcr_internal.hrl").

%%%------------------------------------------------------------------------
init({tcp, http}, Req, [Func]) ->
  State0 = wcr:'#new-'(wcr_handler_state),
  State = wcr:'#set-'({function, Func}, State0),
  {ok, Req, State}.

%%%------------------------------------------------------------------------
-spec handle(Module :: atom(),
             Req :: cowboy_req:req(),
             State :: wcr:handler_state()) ->
                {ok, cowboy_req:req(), wcr:handler_state()}.

%% @doc A handler entry point for all web resources. Checks and parses POST
%% fields, checks user cookie and extracts user from session storage, checks
%% offline flag, calculates and calls handler for the resource being processed,
%% catches errors
handle(Module, Req0, State0) ->
  try
    {Method, Req1} = cowboy_req:method(Req0),

    %% parse request body as multipart, this will not work for POST urlencoded
    %% {Req2, State1} = case is_POST_and_multipart(Req1) of
    %%                    {true, true} ->
    %%                      parse_multipart_form_data(Req1, State0);
    %%                    {true, false} ->
    %%                      parse_body_qs(Req1, State0);
    %%                    {false, _} ->
    %%                      {Req1, State0}
    %%                  end,

    %% {Req3a, State2} = get_user_save_to_state(Req2, State1),
    Req3 = log_access(Method, Req1, State0),

    %% site offline flag
    %% TODO: cache site config in memory or in state
    %% #mcb_site_config{ offline=SiteOffline } = macaba_board:get_site_config(),
    %% State3 = State2#mcb_html_state{ site_offline = SiteOffline },
    %% State4 = state_set_var(site_offline, SiteOffline, State3),

    FnName = list_to_existing_atom("crutch_" ++ atom_to_list(test)),
    {Req4, State3} = apply(Module, FnName, [Method, Req3, State0]),
    {ok, Req4, State3}
  catch
    E ->
      T = lists:flatten(io_lib:format("handle error: ~p ~p",
                                      [E, erlang:get_stacktrace()])),
      lager:error(E),
      {ReqE, StateE} = wcr:response_text(500, T, Req0, State0),
      {ok, ReqE, StateE}
  end.

%%%------------------------------------------------------------------------
log_access(Method, Req0, #wcr_handler_state{ user=U }) ->
  {{IP1, IP2, IP3, IP4}, Req1} = cowboy_req:peer_addr(Req0),
  IP = iolist_to_binary([integer_to_list(IP1), $., integer_to_list(IP2), $.,
                         integer_to_list(IP3), $., integer_to_list(IP4)]),
  {Path, Req2} = cowboy_req:path(Req1),
  lager:info("[~s] Role=~p ~s ~s", [IP, U#wcr_user.roles, Method, Path]),
  Req2.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
