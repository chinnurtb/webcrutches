%%%------------------------------------------------------------------------
%%% @doc WebCrutches Chain call support
%%% Created: 2013-04-02 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(wcr).

-export([ render_page/3
        , render_page/4
        , response_text/4
        , response_json/4
        , redirect/3
        , state_set_var/3
        , state_get_var/2
        , get_user_save_to_state/2
        , get_user/1
        , create_session_for/3
        , ses_cookie_name/0
        , clear_session_cookie/1
        , record_to_proplist/1
        , record_to_proplist/2
        ]).

-include("wcr_internal.hrl").

-compile({parse_transform, exprecs}).
-export_records([wcr_handler_state, wcr_user, wcr_session]).

-opaque handler_state() :: #wcr_handler_state{}.
-type handler_return() :: {cowboy_req:req(), #wcr_handler_state{}}.
-type chain_return() :: {chain_ok|chain_error
                         , cowboy_req:req(), #wcr_handler_state{}}.

-export_type([ handler_state/0
             , handler_return/0
             , chain_return/0
             ]).

%%%-----------------------------------------------------------------------------
%% @doc Renders HTML page for response
-spec render_page(TemplateName :: string(),
                  Req0 :: cowboy_req:req(),
                  State :: wcr:handler_state()) ->
                     wcr:handler_return().

render_page(TemplateName, Req0, State0) ->
  render_page(200, TemplateName, Req0, State0).

%% @doc Renders HTML page for response
-spec render_page(HttpStatus :: integer(),
                  TemplateName :: string(),
                  Req0 :: cowboy_req:req(),
                  State :: wcr:handler_state()) ->
                     wcr:handler_return().

render_page(HttpStatus, TemplateName, Req0,
            State=#wcr_handler_state{
              page_vars=PageVars,
              already_rendered=false
             }) ->
  %% lager:debug("Before render: vars=~p", [PageVars]),
  Body = ?MODULE:render(TemplateName, PageVars),
  Headers = [ {<<"Content-Type">>, <<"text/html">>}
            , {<<"Expires">>, <<"0">>}
            ],
  {ok, Req} = cowboy_req:reply(HttpStatus, Headers, Body, Req0),
  {Req, State#wcr_handler_state{already_rendered=true}};

render_page(_, _, Req, State=#wcr_handler_state{already_rendered=true}) ->
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Does text/plain response
-spec response_text(HttpStatus :: integer(),
                    Body :: iolist() | binary(),
                    Req0 :: cowboy_req:req(),
                    State :: wcr:handler_state()) ->
                       wcr:handler_return().

response_text(HttpStatus, Body, Req0, State=#wcr_handler_state{}) ->
  Headers = [ {<<"Content-Type">>, <<"text/plain">>}
            , {<<"Expires">>, <<"0">>}
            ],
  {ok, Req} = cowboy_req:reply(HttpStatus, Headers, Body, Req0),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Does application/json response
-spec response_json(HttpStatus :: integer(),
                    J :: jsx:json_term(),
                    Req0 :: cowboy_req:req(),
                    State :: wcr:handler_state()) ->
                       wcr:handler_return().

response_json(HttpStatus, J, Req0, State=#wcr_handler_state{}) ->
  Headers = [ {<<"Content-Type">>, <<"application/json">>}
            , {<<"Expires">>, <<"0">>}
            ],
  Body = jsx:encode(J),
  {ok, Req} = cowboy_req:reply(HttpStatus, Headers, Body, Req0),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Redirects user
-spec redirect(URL :: binary()|string(),
               Req0 :: cowboy_req:req(),
               State :: wcr:handler_state()) ->
                  wcr:handler_return().

redirect(URL, Req0, State=#wcr_handler_state{}) ->
  {ok, Req} = cowboy_req:reply(
                301, [ {<<"Location">>, macaba:as_binary(URL)}
                     , {<<"Expires">>, <<"0">>}
                     ],
                <<>>, Req0),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Sets page_vars for rendering template
-spec state_set_var(K :: atom(),
                    V :: any(),
                    State :: wcr:handler_state()) -> wcr:handler_state().

state_set_var(K, V, State = #wcr_handler_state{ page_vars=P0 }) ->
  P = orddict:store(K, V, P0),
  State#wcr_handler_state{ page_vars = P }.

%%%-----------------------------------------------------------------------------
%% @doc Retrieves value of some page_vars element
-spec state_get_var(K :: atom(), State :: mcweb:html_state()) -> any().

state_get_var(K, #wcr_handler_state{ page_vars=PV }) ->
  orddict:fetch(K, PV).

%%%-----------------------------------------------------------------------------
-spec get_user_save_to_state(Req :: cowboy_req:req(),
                             State :: wcr:handler_state()) ->
                                wcr:handler_return().
%% @doc Attempts to extract cookie and find session with that cookie, else
%% returns anonymous user. Saves result state or clears cookie in request
get_user_save_to_state(Req0, State0) ->
  {User, Req} = get_user(Req0),
  State = state_set_var(user, record_to_proplist(User), State0),
  {Req, State#wcr_handler_state{user=User}}.

%%%-----------------------------------------------------------------------------
-spec get_user(Req :: cowboy_req:req()) -> {#ssweb_user{}, cowboy_req:req()}.
%% @doc Extracts cookie from request, find user and return user and slightly
%% modified request
get_user(Req0) ->
  {SesId, Req1} = cowboy_req:cookie(ses_cookie_name(), Req0),
  case SesId of
    undefined ->
      {#wcr_user{}, Req1};
    _ ->
      case wcr_ses:get(SesId) of
        {error, not_found} ->
          %% lager:debug("web:get_user ses '~s' not found", [SesId]),
          Req = clear_session_cookie(Req1),
          {#wcr_user{}, Req};
        {ok, Pid} ->
          U = gen_server:call(Pid, get_user),
          {U, Req1}
      end
  end.

%% @doc Creates session process, sets response cookie, and sets user field
%% in state
create_session_for(U=#wcr_user{}, Req0, State0) ->
  {RemoteAddr, _} = cowboy_req:peer(Req0),
  Opts = [ {remote_addr, RemoteAddr}
         , {user, U}
         ],
  {SesId, _SesPid} = wcr_ses:new(Opts),
  %% lager:debug("set resp cookie ~p=~p", [ses_cookie_name(), SesId]),
  Req = cowboy_req:set_resp_cookie(
          ses_cookie_name(), SesId, [{path, <<"/">>}], Req0),
  State = State0#core_web_state{ user=U },
  {Req, State}.

%% @doc Gets ses cookie name from config
ses_cookie_name() ->
  {ok, CookieName} = macaba_conf:get([<<"board">>, <<"session_cookie_name">>]),
  CookieName.

%%%------------------------------------------------------------------------
%% @doc Attempts to reset session cookie to empty value
-spec clear_session_cookie(Req :: cowboy_req:req()) -> cowboy_req:req().
clear_session_cookie(Req0) ->
  Coo = ?MODULE:ses_cookie_name(),
  cowboy_req:set_resp_cookie(Coo, <<>>, [{path, <<"/">>}, {max_age, 0}], Req0).

%%%------------------------------------------------------------------------
record_to_proplist(Rec) -> record_to_proplist(wcr, Rec).
record_to_proplist(Module, Rec) ->
  [{F, Module:'#get-'(F, Rec)} || F <- Module:'#info-'(Rec, fields)].

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
