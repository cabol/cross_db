%%%-------------------------------------------------------------------
%%% @doc
%%% Defines a schema.
%%%
%%% A schema is used to map any data source into an Erlang map.
%%% One of such use cases is to map data coming from a repository,
%%% usually a table, into Erlang maps.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_schema).

%% API
-export([
  new/2,
  module/1,
  source/1,
  metadata/1,
  fields/1,
  pk_fields/1,
  get_pk_values/2,
  set_fields/2,
  set_field/3,
  get_fields/2,
  get_field/2,
  normalize_keys/1
]).

-import(xdb_schema_type, [cast_field_name/1]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Schema Metadata
-type metadata() :: #{
  schema := module(),
  source := atom(),
  state  := built | loaded | deleted
}.

%% Schema fields definition
-type fields() :: #{
  atom() | binary() => any()
}.

%% Schema definition
-type t() :: #{
  '__meta__' := metadata(),
  atom()     => any() | undefined
}.

%% Exported types
-export_type([
  metadata/0,
  fields/0,
  t/0
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(module(), fields()) -> t().
new(Module, Fields) when is_atom(Module), is_map(Fields) ->
  set_fields(init_schema(Module), Fields).

-spec module(t()) -> module().
module(#{'__meta__' := #{schema := Value}}) ->
  Value.

-spec source(t()) -> atom().
source(#{'__meta__' := #{source := Value}}) ->
  Value.

-spec metadata(t()) -> metadata().
metadata(#{'__meta__' := Value}) ->
  Value.

-spec fields(t()) -> fields().
fields(#{'__meta__' := #{schema := _}} = Schema) ->
  maps:remove('__meta__', Schema).

-spec pk_fields(t()) -> fields() | no_return().
pk_fields(#{'__meta__' := #{schema := Mod}} = Schema) ->
  PkFieldNames = xdb_schema_spec:pk_field_names(Mod:schema_spec()),
  Fields = fields(Schema),

  lists:foldl(fun(PK, Acc) ->
    try
      Acc#{PK => fetch_key(PK, Fields)}
    catch
      error:{badkey, K} ->
        xdb_exception:no_primary_key_value_error(K, Fields)
    end
  end, #{}, PkFieldNames).

-spec get_pk_values([atom()], fields()) -> [any()] | no_return().
get_pk_values(PKs, Fields) ->
  [begin
    try
      fetch_key(PK, Fields)
    catch
      error:{badkey, K} ->
        xdb_exception:no_primary_key_value_error(K, Fields)
    end
  end || PK <- PKs].

-spec set_fields(t(), fields()) -> t().
set_fields(#{'__meta__' := #{schema := Mod}} = Schema, Fields) ->
  FieldSpecs = xdb_schema_spec:fields(Mod:schema_spec()),
  maps:fold(fun(FieldName, FieldValue, Acc) ->
    NormalizedFieldName = cast_field_name(FieldName),
    case lists:keyfind(NormalizedFieldName, 1, FieldSpecs) of
      {NormalizedFieldName, Type, _} ->
        Acc#{NormalizedFieldName => cast_field(Type, FieldName, FieldValue)};
      false ->
        Acc
    end
  end, Schema, Fields).

%% @equiv set_fields(Schema, Fields)
set_field(Schema, FieldName, FieldValue) ->
  set_fields(Schema, #{cast_field_name(FieldName) => FieldValue}).

-spec get_fields(t(), [atom() | binary()]) -> fields().
get_fields(#{'__meta__' := _} = Schema, FieldList) when is_list(FieldList) ->
  maps:with([cast_field_name(F) || F <- FieldList], Schema).

-spec get_field(t(), atom()) -> any() | undefined.
get_field(#{'__meta__' := _} = Schema, Field) when is_atom(Field) ->
  maps:get(Field, Schema, undefined).

-spec normalize_keys(fields()) -> #{atom() => any()}.
normalize_keys(Fields) when is_map(Fields) ->
  maps:fold(fun
    (K, V, Acc) when is_binary(K) -> Acc#{cast_field_name(K) => V};
    (K, _, Acc) when is_atom(K)   -> Acc
  end, Fields, Fields).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
init_schema(Module) ->
  SchemaSpec = Module:schema_spec(),
  Source = xdb_schema_spec:name(SchemaSpec),
  FieldNames = xdb_schema_spec:field_names(SchemaSpec),
  Meta = new_metadata(Module, Source),

  lists:foldl(fun(FieldName, Acc) ->
    Acc#{FieldName => undefined}
  end, #{'__meta__' => Meta}, FieldNames).

%% @private
new_metadata(Schema, Source) ->
  #{schema => Schema, source => Source, state => built}.

%% @private
cast_field(Type, Name, Value) ->
  case xdb_schema_type:cast(Type, Value) of
    {ok, NewValue} -> NewValue;
    {error, _}     -> error({bad_type, {Name, Type, Value}})
  end.

%% @private
fetch_key(K, Map) ->
  case maps:get(K, Map) of
    undefined -> error({badkey, K});
    Value     -> Value
  end.
