%%%-------------------------------------------------------------------
%%% @doc
%%% Defines a schema specification
%%%
%%% The `xdb_schema_spec' defines the struct/map to keep the
%%% information about the schema, such as: field names, types
%%% and maybe options.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_schema_spec).

%% API
-export([
  new/2,
  new_field/2,
  new_field/3,
  name/1,
  fields/1,
  field_names/1,
  field_types/1,
  pk_fields/1,
  pk_field_names/1
]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Field Types
-type field_type() :: string
                    | integer
                    | float
                    | boolean
                    | date
                    | datetime
                    | binary
                    | custom.

%% Common field options
-type field_opt()  :: primary_key.
-type field_opts() :: [field_opt() | any()].

%% Field spec
-type field() :: {
  Name :: atom(),
  Type :: field_type(),
  Opts :: [field_opt() | any()]
}.

%% Schema fields definition
-type fields() :: [field()].

%% Schema spec
-type t() :: #{
  name   := atom(),
  fields := fields()
}.

%% Exported types
-export_type([
  field/0,
  fields/0,
  t/0
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(atom(), fields()) -> t().
new(Name, Fields) when is_atom(Name), is_list(Fields) ->
  #{name => Name, fields => Fields}.

%% @equiv new_field(Name, Type, [])
new_field(Name, Type) ->
  new_field(Name, Type, []).

-spec new_field(atom(), field_type(), field_opts()) -> field().
new_field(Name, Type, Opts) when is_atom(Name), is_atom(Type), is_list(Opts) ->
  {Name, Type, Opts}.

-spec name(t()) -> atom().
name(#{name := Name}) ->
  Name.

-spec fields(t()) -> fields().
fields(#{fields := Fields}) ->
  Fields.

-spec field_names(t()) -> [atom()].
field_names(#{fields := Fields}) ->
  [K || {K, _, _} <- Fields].

-spec field_types(t()) -> #{atom() => field_type()}.
field_types(#{fields := Fields}) ->
  lists:foldl(fun({FieldName, FieldType, _}, Acc) ->
    Acc#{FieldName => FieldType}
  end, #{}, Fields).

-spec pk_fields(t()) -> fields().
pk_fields(#{fields := Fields}) ->
  [F || F = {_, _, Opts} <- Fields, lists:member(primary_key, Opts)].

-spec pk_field_names(t()) -> [atom()].
pk_field_names(#{fields := Fields}) ->
  [K || {K, _, Opts} <- Fields, lists:member(primary_key, Opts)].
