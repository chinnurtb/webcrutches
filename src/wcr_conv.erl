-module(wcr_conv).

-export([ as_atom/1
        , as_string/1
        , as_bool/1
        , as_binary/1
        , as_integer/1
        , as_integer/2
        , as_ipv4/1
        , as_existing_atom/1
        ]).

%%-----------------------------------------------------------------------------
as_atom(A) when is_atom(A) -> A;
as_atom(A) when is_list(A) -> list_to_existing_atom(A);
as_atom(A) when is_binary(A) -> binary_to_existing_atom(A, latin1).

%%-----------------------------------------------------------------------------
as_string(X) when is_atom(X) -> atom_to_list(X);
as_string(X) when is_list(X) -> X;
as_string(X) when is_integer(X) -> integer_to_list(X);
as_string(X) when is_binary(X) -> binary_to_list(X);
as_string(X) when is_boolean(X) ->
    case X of true -> "true"; false -> "false" end.

%%-----------------------------------------------------------------------------
%% @doc Converts string, integer() binary() to bool 'true'|'false'
-spec as_bool(X :: boolean() | binary() | list() | integer()) -> boolean().
as_bool(X) when is_boolean(X) -> X;
as_bool(0) -> false;
as_bool(X) when is_integer(X) -> true;
as_bool(X) when is_binary(X) -> as_bool(binary_to_list(X));
as_bool("1") -> true;
as_bool("on") -> true;
as_bool("true") -> true;
as_bool("0") -> false;
as_bool("") -> false;
as_bool("false") -> false.

%%--------------------------------------------------------------------
-spec as_binary(X :: atom() | integer() | binary() | list()) -> binary().
as_binary(X) when is_atom(X) -> atom_to_binary(X, latin1);
as_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
as_binary(X) when is_binary(X) -> X;
as_binary(X) when is_list(X) -> list_to_binary(X);
as_binary(X) -> as_binary(io_lib:format("~p", [X])).

%%-----------------------------------------------------------------------------
%% @doc Converts string or binary() to ipv4 {X,X,X,X}
-spec as_ipv4(Bin :: binary() | list()) -> {byte(),byte(),byte(),byte()}.

as_ipv4(A) when is_atom(A) ->
  Str = atom_to_list(A),
  {ok, Ip} = inet_parse:ipv4_address(Str),
  Ip;

as_ipv4(Bin) when is_binary(Bin) ->
  Str = binary_to_list(Bin),
  {ok, Ip} = inet_parse:ipv4_address(Str),
  Ip;

as_ipv4(Str) when is_list(Str) ->
  {ok, Ip} = inet_parse:ipv4_address(Str),
  Ip.

%%-----------------------------------------------------------------------------
%% @doc Converts integer, list or binary to integer
as_integer(Value, Default) ->
  case as_integer(Value) of
    undefined ->
      Default;
    V ->
      V
  end.

-spec as_integer(Value :: binary() | string() | integer()) ->
                    integer() | {error, undefined}.
as_integer(Value)
  when is_integer(Value) ->
  Value;
as_integer(Value)
  when is_binary(Value) ->
  {Num,_} = string:to_integer(binary_to_list(Value)),
  Num;
as_integer(Value)
  when is_list(Value) ->
  {Num,_} = string:to_integer(Value),
  Num;
as_integer(_) -> undefined.

%%-----------------------------------------------------------------------------
%% @doc Converts string or binary() to existing atom, or undefined is returned
-spec as_existing_atom(A :: binary() | list() | atom()) -> atom().
as_existing_atom(A) when is_atom(A) -> A;
as_existing_atom(A) when is_binary(A) ->
  Str = binary_to_list(A),
  try list_to_existing_atom(Str)
  catch error:badarg -> undefined end;
as_existing_atom(Str) when is_list(Str) ->
  try list_to_existing_atom(Str)
  catch error:badarg -> undefined end.


%% hex(N) when N < 10 ->
%%   $0+N;
%% hex(N) when N >= 10, N < 16 ->
%%   $a+(N-10).

int(C) when $0 =< C, C =< $9 -> C - $0;
int(C) when $A =< C, C =< $F -> C - $A + 10;
int(C) when $a =< C, C =< $f -> C - $a + 10.

%% to_hex(N) when N < 256 ->
%%   [hex(N div 16), hex(N rem 16)].

%% list_to_hexstr([]) -> 
%%   [];
%% list_to_hexstr([H|T]) ->
%%   to_hex(H) ++ list_to_hexstr(T).

%% bin_to_hexstr(Bin) ->
%%   list_to_hexstr(binary_to_list(Bin)).

hexstr_to_bin(S) ->
  list_to_binary(hexstr_to_list(S)).

hexstr_to_list([X,Y|T]) ->
  [int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
  [].
