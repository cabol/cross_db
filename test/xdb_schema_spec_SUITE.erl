-module(xdb_schema_spec_SUITE).

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-export([
  t_schema/1,
  t_getters/1,
  t_setters/1,
  t_disabled_getters_setters/1
]).

-define(EXCLUDED_FUNS, [
  module_info,
  all,
  init_per_suite,
  end_per_suite
]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% CT
%%%===================================================================

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(xdb_ct:config()) -> xdb_ct:config().
init_per_suite(Config) ->
  [{schema, new_schema()} | Config].

-spec end_per_suite(xdb_ct:config()) -> ok.
end_per_suite(_) ->
  ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec t_schema(xdb_ct:config()) -> ok.
t_schema(Config) ->
  Schema = ?config(schema, Config),

  test = xdb_schema_spec:name(Schema),
  FieldNames = lists:usort(xdb_schema_spec:field_names(Schema)),
  FieldNames = lists:usort([id, username, password, is_active, created_at]),

  [
    {id, integer, [primary_key]},
    {username, string, []},
    {password, string, []},
    {is_active, boolean, []},
    {created_at, datetime, []}
  ] = xdb_schema_spec:fields(Schema),

  #{
    id         := integer,
    username   := string,
    password   := string,
    is_active  := boolean,
    created_at := datetime
  } = xdb_schema_spec:field_types(Schema),
  [{id, integer, [primary_key]}] = xdb_schema_spec:pk_fields(Schema),
  [id] = xdb_schema_spec:pk_field_names(Schema),
  ok.

-spec t_getters(xdb_ct:config()) -> ok.
t_getters(_Config) ->
  Person = person:schema(person:new(<<"John">>, <<"Doe">>)),
  <<"John">> = person:first_name(Person),
  <<"Doe">> = person:last_name(Person),
  undefined = person:age(Person),
  ok.

-spec t_setters(xdb_ct:config()) -> ok.
t_setters(_Config) ->
  Person = person:schema(person:new(<<"John">>, <<"Doe">>)),
  <<"John">> = person:first_name(Person),
  <<"Doe">> = person:last_name(Person),
  undefined = person:age(Person),
  33 = person:age(person:age(Person, 33)),
  <<"Other">> = person:last_name(person:last_name(Person, <<"Other">>)),
  ok.

-spec t_disabled_getters_setters(xdb_ct:config()) -> ok.
t_disabled_getters_setters(_Config) ->
  Person = person:schema(person:new(<<"John">>, <<"Doe">>)),

  % this is for dialyzer (skyp error undefined_function_calls)
  Mod = person,

  ok = xdb_ct:assert_error(fun() ->
    Mod:profile_image(Person)
  end, undef),

  ok = xdb_ct:assert_error(fun() ->
    Mod:created_at(Person, calendar:universal_time())
  end, undef).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
new_schema() ->
  xdb_schema_spec:new(test, [
    xdb_schema_spec:new_field(id, integer, [primary_key]),
    xdb_schema_spec:new_field(username, string),
    xdb_schema_spec:new_field(password, string),
    xdb_schema_spec:new_field(is_active, boolean),
    xdb_schema_spec:new_field(created_at, datetime)
  ]).
