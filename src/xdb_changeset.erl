%%%-------------------------------------------------------------------
%%% @doc
%%% Based on SumoDB Changeset and `Ecto.Changeset'.
%%%
%%% Changesets allow filtering, casting, validation and definition of
%%% constraints when manipulating schemas.
%%%
%%% @reference See
%%% <a href="https://github.com/inaka/sumo_db/blob/master/src/utils/sumo_changeset.erl">SumoDB</a>
%%% <a href="https://hexdocs.pm/ecto/Ecto.Changeset.html">Ecto.Changeset</a>
%%% @end
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_changeset).

%% Properties
-export([
  schema/1,
  data/1,
  params/1,
  errors/1,
  changes/1,
  is_valid/1,
  types/1,
  required/1,
  repo/1,
  action/1,
  filters/1
]).

%% API
-export([
  add_error/3,
  add_error/4,
  apply_changes/1,
  cast/3,
  change/2,
  get_field/2,
  get_field/3,
  put_change/3,
  get_change/2,
  get_change/3,
  delete_change/2,
  validate_change/3,
  validate_required/2,
  validate_inclusion/3,
  validate_number/3,
  validate_length/3,
  validate_format/3
]).

-import(xdb_schema_type, [cast_field_name/1]).

%%%=============================================================================
%%% Types
%%%=============================================================================

%% General types
-type error()  :: {binary(), xdb_lib:keyword()}.
-type errors() :: [{atom(), error()}].
-type action() :: undefined | insert | update | delete | replace | ignore.

%% Changeset definition
-type t() :: #{
  schema   => module() | undefined,
  data     => xdb_schema:t() | undefined,
  params   => xdb_schema:fields() | undefined,
  errors   => errors(),
  changes  => xdb_lib:kw_map(),
  is_valid => boolean(),
  types    => xdb_lib:kw_map() | undefined,
  required => [atom()],
  repo     => atom() | undefined,
  action   => action(),
  filters  => xdb_lib:kw_map()
}.

%% Exported types
-export_type([
  t/0
]).

%%%=============================================================================
%%% Properties
%%%=============================================================================

-spec schema(t()) -> module() | undefined.
schema(#{schema := Value}) ->
  Value.

-spec data(t()) -> xdb_schema:t() | undefined.
data(#{data := Value}) ->
  Value.

-spec params(t()) -> xdb_schema:fields() | undefined.
params(#{params := Value}) ->
  Value.

-spec errors(t()) -> errors().
errors(#{errors := Value}) ->
  Value.

-spec changes(t()) -> xdb_lib:kw_map().
changes(#{changes := Value}) ->
  Value.

-spec is_valid(t()) -> boolean().
is_valid(#{is_valid := Value}) ->
  Value.

-spec types(t()) -> xdb_lib:kw_map() | undefined.
types(#{types := Value}) ->
  Value.

-spec required(t()) -> [atom()].
required(#{required := Value}) ->
  Value.

-spec repo(t()) -> atom() | undefined.
repo(#{repo := Value}) ->
  Value.

-spec action(t()) -> atom() | undefined.
action(#{action := Value}) ->
  Value.

-spec filters(t()) -> xdb_lib:kw_map().
filters(#{filters := Value}) ->
  Value.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec add_error(t(), atom(), binary()) -> t().
add_error(Changeset, Key, Message) ->
  add_error(Changeset, Key, Message, []).

-spec add_error(t(), atom(), binary(), xdb_lib:keyword()) -> t().
add_error(#{errors := Errors} = Changeset, Key, Message, Keys) ->
  Changeset#{errors := [{Key, {Message, Keys}} | Errors], is_valid := false}.

-spec apply_changes(t()) -> xdb_schema:t().
apply_changes(#{changes := Changes, data := Data}) when map_size(Changes) == 0 ->
  Data;
apply_changes(#{changes := Changes, data := Data}) ->
  xdb_schema:set_fields(Data, Changes).

-spec cast(Data, Params, Permitted) -> Res when
  Data      :: xdb_schema:t() | t(),
  Params    :: xdb_schema:fields(),
  Permitted :: [atom()],
  Res       :: t().
cast(#{schema := Schema, data := Data, types := Types} = CS, Params, Permitted) ->
  NewChangeset = do_cast({Schema, Data, Types}, Params, Permitted),
  cast_merge(CS, NewChangeset);
cast(Data, Params, Permitted) ->
  Metadata = get_metadata(Data),
  do_cast(Metadata, Params, Permitted).

%% @private
do_cast({SchemaName, Data, Types}, Params, Permitted) ->
  NewParams = xdb_schema:normalize_keys(Params),
  FilteredParams = maps:with(Permitted, NewParams),

  {Changes, Errors, IsValid} =
    maps:fold(fun(ParamKey, ParamVal, Acc) ->
      process_param(ParamKey, ParamVal, Types, Acc)
    end, {#{}, [], true}, FilteredParams),

  (changeset())#{
    schema   := SchemaName,
    data     := Data,
    params   := FilteredParams,
    changes  := Changes,
    errors   := Errors,
    is_valid := IsValid,
    types    := Types
  }.

-spec change(Data, Changes) -> Res when
  Data    :: xdb_schema:t() | t(),
  Changes :: xdb_lib:kw_map(),
  Res     :: t().
change(#{changes := _, types := _} = Changeset, Changes) ->
  NewChanges = changes(get_changed(Changeset, Changes)),
  Changeset#{changes := NewChanges};
change(Data, Changes) ->
  {SchemaName, Data, Types} = get_metadata(Data),
  Changeset = (changeset())#{
    schema   := SchemaName,
    data     := Data,
    types    := Types
  },
  NewChanges = changes(get_changed(Changeset, Changes)),
  Changeset#{changes  := NewChanges}.

%% @private
get_changed(Changeset, NewChanges) ->
  maps:fold(fun(K, V, Acc) ->
    put_change(Acc, K, V)
  end, Changeset, NewChanges).

-spec get_field(t(), atom()) -> term().
get_field(Changeset, Key) ->
  get_field(Changeset, Key, undefined).

-spec get_field(t(), atom(), term()) -> term().
get_field(#{changes := Changes, data := Data}, Key, Default) ->
  case maps:find(Key, Changes) of
    {ok, Value} ->
      Value;
    error ->
      case maps:find(Key, Data) of
        {ok, Value} -> Value;
        error       -> Default
      end
  end.

-spec put_change(t(), atom(), term()) -> t().
put_change(#{changes := Changes, data := Data} = Changeset, Key, Value) ->
  NewChanges =
    case maps:find(Key, Data) of
      {ok, V} when V /= Value ->
        maps:put(Key, Value, Changes);
      _ ->
        case maps:is_key(Key, Changes) of
          true  -> maps:remove(Key, Changes);
          false -> Changes
        end
    end,

  Changeset#{changes := NewChanges}.

-spec get_change(t(), atom()) -> term().
get_change(Changeset, Key) ->
  get_change(Changeset, Key, undefined).

-spec get_change(t(), atom(), term()) -> term().
get_change(#{changes := Changes}, Key, Default) ->
  maps:get(Key, Changes, Default).

-spec delete_change(t(), atom()) -> t().
delete_change(#{changes := Changes} = Changeset, Key) ->
  NewChanges = maps:remove(Key, Changes),
  Changeset#{changes := NewChanges}.

-spec validate_change(t(), atom(), fun((atom(), term()) -> [error()])) -> t().
validate_change(#{changes := Changes, errors := Errors} = Changeset, Field, Validator) ->
  _ = ensure_field_exists(Changeset, Field),

  Value = fetch(Field, Changes),
  NewErrors1 =
    case is_nil(Value) of
      true  -> [];
      false -> Validator(Field, Value)
    end,

  NewErrors2 =
    [begin
      case Error of
        {K, V} when is_atom(K), is_binary(V) ->
          {K, {V, []}};
        {K, {V, Opts}} when is_atom(K), is_binary(V), is_list(Opts) ->
          {K, {V, Opts}}
      end
    end || Error <- NewErrors1],

  case NewErrors2 of
    []      -> Changeset;
    [_ | _] -> Changeset#{errors := NewErrors2 ++ Errors, is_valid := false}
  end.

-spec validate_required(t(), [atom()]) -> t().
validate_required(#{required := Required, errors := Errors} = CS, Fields) ->
  NewErrors = [
    {F, {<<"can't be blank">>, [{validation, required}]}}
    || F <- Fields, is_missing(CS, F), ensure_field_exists(CS, F), is_nil(fetch(F, Errors))
  ],

  case NewErrors of
    [] -> CS#{required := Fields ++ Required};
    _  -> CS#{required := Fields ++ Required, errors := NewErrors ++ Errors, is_valid := false}
  end.

-spec validate_inclusion(t(), atom(), [term()]) -> t().
validate_inclusion(Changeset, Field, Enum) ->
  validate_change(Changeset, Field, fun(_, Value) ->
    case lists:member(Value, Enum) of
      true  -> [];
      false -> [{Field, {<<"is invalid">>, [{validation, inclusion}]}}]
    end
  end).

-spec validate_number(t(), atom(), xdb_lib:keyword()) -> t().
validate_number(Changeset, Field, Opts) ->
  validate_change(Changeset, Field, fun(TargetField, Value) ->
    hd([begin
      case maps:find(SpecKey, number_validators(TargetValue)) of
        {ok, {SpecFun, Message}} ->
          validate_number(TargetField, Value, Message, SpecFun, TargetValue);
        error ->
          error({badarg, SpecKey})
      end
    end || {SpecKey, TargetValue} <- Opts])
  end).

%% @private
validate_number(Field, Value, Message, SpecFun, TargetValue) ->
  case SpecFun(Value, TargetValue) of
    true  -> [];
    false -> [{Field, {Message, [{validation, number}]}}]
  end.

-spec validate_length(t(), atom(), xdb_lib:keyword()) -> t().
validate_length(Changeset, Field, Opts) ->
  validate_change(Changeset, Field, fun(_, Value) ->
    case do_validate_length(length_validators(), Opts, byte_size(Value), undefined) of
      undefined -> [];
      Message   -> [{Field, {Message, [{validation, length}]}}]
    end
  end).

%% @private
do_validate_length([], _, _, Acc) ->
  Acc;
do_validate_length([{Opt, Validator} | T], Opts, Length, Acc) ->
  case fetch(Opt, Opts) of
    undefined ->
      do_validate_length(T, Opts, Length, Acc);
    Value ->
      case Validator(Length, Value) of
        undefined ->
          do_validate_length(T, Opts, Length, Acc);
        Message ->
          Message
      end
  end.

%% @private
wrong_length(Value, Value) ->
  undefined;
wrong_length(_Length, Value) ->
  text("should be ~p character(s)", [Value]).

%% @private
too_short(Length, Value) when Length >= Value ->
  undefined;
too_short(_Length, Value) ->
  text("should be at least ~p character(s)", [Value]).

%% @private
too_long(Length, Value) when Length =< Value ->
  undefined;
too_long(_Length, Value) ->
  text("should be at most ~p character(s)", [Value]).

-spec validate_format(t(), atom(), binary()) -> t().
validate_format(Changeset, Field, Format) ->
  validate_change(Changeset, Field, fun(_, Value) ->
    case re:run(Value, Format) of
      nomatch -> [{Field, {<<"has invalid format">>, [{validation, format}]}}];
      _       -> []
    end
  end).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private
changeset() ->
  #{
    schema   => undefined,
    data     => undefined,
    params   => undefined,
    errors   => [],
    changes  => #{},
    is_valid => true,
    types    => undefined,
    required => [],
    repo     => undefined,
    action   => undefined,
    filters  => #{}
  }.

%% @private
get_metadata(Data) ->
  SchemaMod = xdb_schema:module(Data),
  SchemaSpec = SchemaMod:schema_spec(),
  Types = xdb_schema_spec:field_types(SchemaSpec),
  {SchemaMod, Data, Types}.

%% @private
process_param(ParamKey, ParamValue, Types, {Changes, Errors, IsValid}) ->
  Key = cast_field_name(ParamKey),
  case cast_field(Key, ParamValue, Types) of
    {ok, CastValue} ->
      {maps:put(Key, CastValue, Changes), Errors, IsValid};
    {invalid, Type} ->
      {Changes, [{Key, {<<"is invalid">>, [{type, Type}, {validation, cast}]}} | Errors], false};
    missing ->
      {Changes, Errors, IsValid}
  end.

%% @private
cast_field(Key, Value, Types) ->
  case maps:get(Key, Types, error) of
    error ->
      missing;
    Type ->
      case xdb_schema_type:cast(Type, Value) of
        {ok, _} = Ok -> Ok;
        {error, _}   -> {invalid, Type}
      end
  end.

%% @private
cast_merge(CS1, CS2) ->
  NewChanges = maps:merge(changes(CS1), changes(CS2)),
  NewErrors = lists:usort(errors(CS1) ++ errors(CS2)),
  NewIsValid = is_valid(CS1) and is_valid(CS2),
  NewTypes = types(CS1),
  NewRequired = lists:usort(required(CS1) ++ required(CS2)),
  NewParams = maps:merge(cs_params(CS1), cs_params(CS2)),

  CS1#{
    params   := NewParams,
    changes  := NewChanges,
    errors   := NewErrors,
    is_valid := NewIsValid,
    types    := NewTypes,
    required := NewRequired
  }.

%% @private
cs_params(#{params := Params}) ->
  case Params of
    undefined -> #{};
    _         -> Params
  end.

%% @private
is_missing(Changeset, Field) ->
  case get_field(Changeset, Field) of
    undefined -> true;
    _         -> false
  end.

%% @private
ensure_field_exists(#{types := Types}, Field) ->
  case maps:is_key(Field, Types) of
    true  -> true;
    false -> error({badarg, Field})
  end.

%% @private
is_nil(undefined) -> true;
is_nil(_)         -> false.

%% @private
fetch(Key, Keyword) when is_list(Keyword) ->
  case lists:keyfind(Key, 1, Keyword) of
    {Key, Value} -> Value;
    _            -> undefined
  end;
fetch(Key, Map) when is_map(Map) ->
  maps:get(Key, Map, undefined).

%% @private
number_validators(N) ->
  Text = fun(T) -> text("must be ~s ~p", [T, N]) end,
  #{
    less_than                => {fun(X, Y) -> X < Y end,  Text("less than")},
    greater_than             => {fun(X, Y) -> X > Y end,  Text("greater than")},
    less_than_or_equal_to    => {fun(X, Y) -> X =< Y end, Text("less than or equal to")},
    greater_than_or_equal_to => {fun(X, Y) -> X >= Y end, Text("greater than or equal to")},
    equal_to                 => {fun(X, Y) -> X == Y end, Text("equal to")}
  }.

%% @private
length_validators() ->
  [{is, fun wrong_length/2}, {min, fun too_short/2}, {max, fun too_long/2}].

%% @private
text(Msg, Args) ->
  iolist_to_binary(io_lib:format(Msg, Args)).
