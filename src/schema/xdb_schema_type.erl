%%%-------------------------------------------------------------------
%%% @doc
%%% Based on SumoDB `sumo_type'.
%%%
%%% @reference See
%%% <a href="https://github.com/inaka/sumo_db/blob/master/src/utils/sumo_type.erl">SumoDB</a>
%%% @end
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_schema_type).

-export([
  cast/2,
  cast_field_name/1
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec cast(atom(), term()) -> {ok, term()} | {error, {invalid, term()}}.
cast(_, undefined) ->
  {ok, undefined};
cast(Type, Data) when is_binary(Data) andalso (Type == integer orelse Type == float) ->
  cast(Type, binary_to_list(Data));
cast(float, Data) when is_integer(Data) ->
  {ok, Data + 0.0};
cast(float, Data) when is_list(Data) ->
  cast_float(Data);
cast(integer, Data) when is_float(Data) ->
  {ok, trunc(Data)};
cast(integer, Data) when is_list(Data) ->
  cast_integer(Data);
cast(Type, Data) when is_list(Data), Type /= binary, Type /= custom ->
  case io_lib:printable_list(Data) of
    true  -> cast(Type, list_to_binary(Data));
    false -> {error, {invalid, Data}}
  end;
cast(string, Data) when is_binary(Data); is_atom(Data); is_number(Data) ->
  {ok, xdb_lib:to_bin(Data)};
cast(boolean, Data) when is_binary(Data) ->
  case lists:member(Data, [<<"true">>, <<"1">>]) of
    true ->
      {ok, true};
    false ->
      case lists:member(Data, [<<"false">>, <<"0">>]) of
        true  -> {ok, false};
        false -> {error, {invalid, Data}}
      end
  end;
cast(Type, Data) when is_binary(Data) andalso (Type == date orelse Type == datetime) ->
  try
    {ok, iso8601:parse(Data)}
  catch
    _:_ -> {error, {invalid, Data}}
  end;
cast(Type, Data) ->
  Fun = maps:get(Type, primitives(), fun(_) -> false end),
  case Fun(Data) of
    true  -> {ok, Data};
    false -> {error, {invalid, Data}}
  end.

-spec cast_field_name(atom() | binary()) -> atom().
cast_field_name(Data) when is_atom(Data) ->
  Data;
cast_field_name(Data) when is_binary(Data) ->
  binary_to_atom(Data, utf8).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
primitives() ->
  #{
    string   => fun erlang:is_binary/1,
    integer  => fun erlang:is_integer/1,
    float    => fun erlang:is_float/1,
    boolean  => fun erlang:is_boolean/1,
    date     => fun xdb_lib:is_datetime/1,
    datetime => fun xdb_lib:is_datetime/1,
    binary   => fun erlang:is_binary/1,
    custom   => fun(_) -> true end
  }.

%% @private
cast_float(Data) ->
  case string:to_float(Data) of
    {error, no_float} ->
      case cast_integer(Data) of
        {ok, Integer} -> {ok, Integer + 0.0};
        Error         -> Error
      end;
    {Float, _Rest} ->
      {ok, Float}
  end.

%% @private
cast_integer(Data) ->
  case string:to_integer(Data) of
    {error, no_integer} -> {error, {invalid, Data}};
    {Integer, _Rest}    -> {ok, Integer}
  end.
