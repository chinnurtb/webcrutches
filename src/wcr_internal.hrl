-ifndef(WEBCRUTCHES_INTERNAL_HRL_INCLUDED).
-define(WEBCRUTCHES_INTERNAL_HRL_INCLUDED, 1).

-record(wcr_user, {
          login = <<>> :: binary(),
          roles = 0    :: integer()
         }).

-record(wcr_handler_state, {
          function  :: atom(),
          user      :: #wcr_user{},
          page_vars :: orddict:orddict(),
          already_rendered = false :: boolean()
         }).

-record(wcr_session, {
          %% this like, allows ipv6 too, but will we ever support that?
          sesid = <<>> :: binary(),
          remote_addr = {0,0,0,0} :: {byte(),byte(),byte(),byte()},
          user :: #wcr_user{}
         }).

-endif. % WEBCRUTCHES_INTERNAL_HRL_INCLUDED
