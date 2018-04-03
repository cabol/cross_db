-module(xdb_schema_SUITE).

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-export([
  t_schema/1,
  t_get_set_fields/1
]).

%% Test Cases
-include_lib("mixer/include/mixer.hrl").
-mixin([xdb_schema_test]).

-import(xdb_ct, [assert_error/2]).

-define(EXCLUDED_FUNS, [
  module_info,
  all,
  init_per_suite,
  end_per_suite
]).

%%%===================================================================
%%% CT
%%%===================================================================

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(xdb_ct:config()) -> xdb_ct:config().
init_per_suite(Config) ->
  [{schema, person} | Config].

-spec end_per_suite(xdb_ct:config()) -> ok.
end_per_suite(_Config) ->
  ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec t_schema(xdb_ct:config()) -> ok.
t_schema(_Config) ->
  Schema = xdb_schema:new(person, person:new(<<"John">>, <<"Doe">>)),

  person = xdb_schema:module(Schema),
  people = xdb_schema:source(Schema),
  #{schema := _} = xdb_schema:metadata(Schema),
  #{
    first_name  := <<"John">>,
    last_name   := <<"Doe">>,
    birthdate   := {_, _, _},
    created_at  := {{_, _, _}, {_, _, _}},
    description := undefined
  } = xdb_schema:fields(Schema),

  #{id := 1} = xdb_schema:pk_fields(Schema#{id => 1}),
  ok = assert_error(fun() -> xdb_schema:pk_fields(Schema) end, no_primary_key_value_error),

  [1] = xdb_schema:get_pk_values([id], Schema#{id => 1}),
  ok = assert_error(fun() ->
    xdb_schema:get_pk_values([id], Schema)
  end, no_primary_key_value_error),

  NormalizedFields = xdb_schema:normalize_keys(#{
    first_name      => <<"John">>,
    <<"last_name">> => <<"Doe">>
  }),
  #{
    first_name  := <<"John">>,
    last_name   := <<"Doe">>
  } = NormalizedFields,
  ok.

-spec t_get_set_fields(xdb_ct:config()) -> ok.
t_get_set_fields(_Config) ->
  Schema = xdb_schema:new(person, person:new(<<"John">>, <<"Doe">>)),

  Schema1 = xdb_schema:set_fields(Schema, #{
    first_name      => <<"other">>,
    <<"last_name">> => <<"other">>
  }),

  #{
    first_name  := <<"other">>,
    last_name   := <<"other">>
  } = xdb_schema:fields(Schema1),

  Schema2 = xdb_schema:set_field(Schema, description, <<"hello">>),
  <<"hello">> = xdb_schema:get_field(Schema2, description),
  #{
    first_name  := <<"John">>,
    last_name   := <<"Doe">>
  } = xdb_schema:get_fields(Schema2, [first_name, <<"last_name">>]),

  ok = xdb_ct:assert_error(fun() ->
    xdb_schema:set_field(Schema, id, <<"hello">>)
  end, {bad_type, {id, integer, <<"hello">>}}).
