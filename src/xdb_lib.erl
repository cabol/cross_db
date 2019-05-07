%%%-------------------------------------------------------------------
%%% @doc
%%% Common utilities.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_lib).

-export([
  to_bin/1,
  to_atom/1,
  to_kvlist/1,
  to_map/1,
  is_datetime/1,
  stringify/2,
  keyfetch/2,
  keyfind/2,
  keyfind/3,
  keymerge/2,
  kv_keys/1,
  kv_values/1,
  reduce_while/3,
  raise/1,
  raise/2,
  raise/3
]).

%%%===================================================================
%%% Common Types
%%%===================================================================

-type kwentry() :: {atom(), any()}.
-type keyword() :: [kwentry()].
-type kw_map()  :: #{atom()  => any()}.
-type kv()      :: keyword() | kw_map().
-type kvlist()  :: [{any(), any()}].

-export_type([
  kwentry/0,
  keyword/0,
  kw_map/0,
  kv/0,
  kvlist/0
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec to_bin(binary() | atom() | integer() | float() | list()) -> binary().
to_bin(Data) when is_binary(Data) ->
  Data;
to_bin(Data) when is_atom(Data) ->
  atom_to_binary(Data, utf8);
to_bin(Data) when is_integer(Data) ->
  integer_to_binary(Data);
to_bin(Data) when is_float(Data) ->
  list_to_binary(io_lib:format("~w", [Data]));
to_bin(Data) when is_list(Data) ->
  iolist_to_binary(Data).

-spec to_atom(atom() | binary() | list()) -> atom().
to_atom(Data) when is_atom(Data) ->
  Data;
to_atom(Data) when is_binary(Data) ->
  binary_to_atom(Data, utf8);
to_atom(Data) when is_list(Data) ->
  list_to_atom(Data).

-spec to_kvlist(kvlist() | map()) -> kvlist().
to_kvlist(Data) when is_list(Data) ->
  Data;
to_kvlist(Data) when is_map(Data) ->
  maps:to_list(Data).

-spec to_map(kvlist() | map()) -> map().
to_map(Data) when is_map(Data) ->
  Data;
to_map(Data) when is_list(Data) ->
  maps:from_list(Data).

-spec is_datetime(any()) -> boolean().
is_datetime({_, _, _} = Date) ->
  calendar:valid_date(Date);
is_datetime({{_, _, _} = Date, {H, M, S}}) ->
  calendar:valid_date(Date) and
    (H >= 0 andalso H =< 23) and
    (M >= 0 andalso M =< 59) and
    (S >= 0 andalso S =< 59);
is_datetime(_) ->
  false.

-spec stringify(string(), [any()]) -> string().
stringify(Msg, Args) ->
  lists:flatten(io_lib:format(Msg, Args)).

-spec keyfetch(any(), kvlist()) -> any() | no_return().
keyfetch(Key, KVList) ->
  case lists:keyfind(Key, 1, KVList) of
    {Key, Value} -> Value;
    false        -> error({badkey, Key})
  end.

-spec keyfind(any(), kvlist()) -> any() | undefined.
keyfind(Key, KVList) ->
  keyfind(Key, KVList, undefined).

-spec keyfind(any(), kvlist(), any()) -> any().
keyfind(Key, KVList, Default) ->
  case lists:keyfind(Key, 1, KVList) of
    {Key, Value} -> Value;
    false        -> Default
  end.

-spec keymerge(kvlist(), kvlist()) -> kvlist().
keymerge(KV1, KV2) ->
  lists:foldl(fun({K, V}, Acc) ->
    lists:keystore(K, 1, Acc, {K, V})
  end, KV1, KV2).

-spec kv_keys(kvlist()) -> [any()].
kv_keys(KV) ->
  {Keys, _Values} = lists:unzip(KV),
  Keys.

-spec kv_values(kvlist()) -> [any()].
kv_values(KV) ->
  {_Keys, Values} = lists:unzip(KV),
  Values.

-spec reduce_while(Fun, AccIn, List) -> Result when
  Fun    :: fun((Elem :: any(), Acc :: any()) -> FunRes),
  FunRes :: {cont | halt, AccOut :: any()},
  AccIn  :: any(),
  List   :: [any()],
  Result :: any().
reduce_while(Fun, AccIn, List) when is_function(Fun, 2) ->
  try
    lists:foldl(fun(Elem, Acc) ->
      case Fun(Elem, Acc) of
        {cont, AccOut} -> AccOut;
        {halt, AccOut} -> throw({halt, AccOut})
      end
    end, AccIn, List)
  catch
    throw:{halt, AccOut} ->
      AccOut;
    Kind:Reason ->
      erlang:raise(Kind, Reason, erlang:get_stacktrace())
  end.

-spec raise(any()) -> no_return().
raise(Reason) ->
  {_, Trace} = erlang:process_info(self(), current_stacktrace),
  erlang:raise(error, Reason, Trace).

-spec raise(atom(), any()) -> no_return().
raise(Error, Reason) when is_atom(Error) ->
  {_, Trace} = erlang:process_info(self(), current_stacktrace),
  erlang:raise(error, {Error, Reason}, Trace).

-spec raise(atom(), string(), [any()]) -> no_return().
raise(Error, Text, Args) when is_atom(Error) ->
  Reason = stringify(Text, Args),
  {_, Trace} = erlang:process_info(self(), current_stacktrace),
  erlang:raise(error, {Error, Reason}, Trace).
