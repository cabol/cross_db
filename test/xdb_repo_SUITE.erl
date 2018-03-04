-module(xdb_repo_SUITE).

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-export([
  t_insert/1,
  t_insert_all/1,
  t_update/1,
  t_delete/1,
  t_all/1,
  t_get/1,
  t_get_by/1
]).

-import(xdb_ct, [assert_error/2]).

-define(EXCLUDED_FUNS, [
  module_info,
  all,
  init_per_suite,
  end_per_suite
]).

-define(REPO, xdb_test_repo).
-define(ADAPTER, xdb_test_adapter).

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
  {ok, _} = application:ensure_all_started(cross_db),
  Config.

-spec end_per_suite(xdb_ct:config()) -> ok.
end_per_suite(_) ->
  ok = application:stop(cross_db).

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec t_insert(xdb_ct:config()) -> ok.
t_insert(_Config) ->
  {ok, #{id := 1}} =
    [pipe](
      #{id => 1},
      person:schema(_),
      ?REPO:insert(_)),

  {ok, #{id := 1}} =
    [pipe](
      #{id => 1},
      person:schema(_),
      xdb_changeset:change(_, #{first_name => <<"Joe">>}),
      ?REPO:insert(_)),

  {error, CS} =
    [pipe](
      #{id => 1},
      person:schema(_),
      xdb_changeset:change(_, #{first_name => <<"Joe">>}),
      xdb_changeset:add_error(_, first_name, <<"Invalid">>),
      ?REPO:insert(_)),

  {error, [{constraint, invalid_key}]} =
    [pipe](
      #{id => -2},
      person:schema(_),
      ?REPO:insert(_)),

  ok = assert_error(fun() ->
    [pipe](
      CS,
      ?REPO:update(_))
  end, badarg),

  ok = assert_error(fun() ->
    [pipe](
      #{id => -1},
      person:schema(_),
      ?REPO:insert(_))
  end, my_error),

  ok = assert_error(fun() ->
    [pipe](
      #{id => -11},
      person:schema(_),
      ?REPO:insert(_))
  end, badarg).

-spec t_insert_all(xdb_ct:config()) -> ok.
t_insert_all(_Config) ->
  {1, [#{id := 1}]} = ?REPO:insert_all(person, [#{id => 1}]),
  {2, [#{id := 1}, #{id := 2}]} = ?REPO:insert_all(person, [#{id => 1}, #{id => 2}]),
  ok.

-spec t_update(xdb_ct:config()) -> ok.
t_update(_Config) ->
  ok = assert_error(fun() ->
    [pipe](
      #{id => 1},
      person:schema(_),
      ?REPO:update(_))
  end, function_clause),

  {ok, #{id := 1}} =
    [pipe](
      #{id => 1},
      person:schema(_),
      xdb_changeset:change(_, #{first_name => <<"Joe">>}),
      ?REPO:update(_)),

  {error, #{}} =
    [pipe](
      #{id => 1},
      person:schema(_),
      xdb_changeset:change(_, #{first_name => <<"Joe">>}),
      xdb_changeset:add_error(_, first_name, <<"Invalid">>),
      ?REPO:update(_)),

  ok = assert_error(fun() ->
    [pipe](
      #{id => -1},
      person:schema(_),
      xdb_changeset:change(_, #{id => -2}),
      ?REPO:update(_))
  end, stale_entry_error).

-spec t_delete(xdb_ct:config()) -> ok.
t_delete(_Config) ->
  {ok, #{id := 1}} =
    [pipe](
      #{id => 1},
      person:schema(_),
      ?REPO:delete(_)),

  {ok, #{id := 1}} =
    [pipe](
      #{id => 1},
      person:schema(_),
      xdb_changeset:change(_, #{first_name => <<"Joe">>}),
      ?REPO:delete(_)),

  {error, #{}} =
    [pipe](
      #{id => 1},
      person:schema(_),
      xdb_changeset:change(_, #{first_name => <<"Joe">>}),
      xdb_changeset:add_error(_, first_name, <<"Invalid">>),
      ?REPO:delete(_)),

  ok = assert_error(fun() ->
    [pipe](
      #{id => -1},
      person:schema(_),
      ?REPO:delete(_))
  end, stale_entry_error),

  ok = assert_error(fun() ->
    [pipe](
      #{},
      person:schema(_),
      ?REPO:delete(_))
  end, no_primary_key_value_error).

-spec t_all(xdb_ct:config()) -> ok.
t_all(_Config) ->
  [#{'__meta__' := _, id := 1}] = ?REPO:all(person),

  Query = xdb_query:from(person, [{where, [{id, 1}]}]),
  [#{'__meta__' := _, id := 1}] = ?REPO:all(Query),
  ok.

-spec t_get(xdb_ct:config()) -> ok.
t_get(_Config) ->
  #{'__meta__' := _, id := 1} = ?REPO:get(person, 1),
  undefined = ?REPO:get(person, -1),

  ok = assert_error(fun() ->
    ?REPO:get(person, -11)
  end, multiple_results_error),

  ok = assert_error(fun() ->
    ?REPO:get(account, 1)
  end, badarg).

-spec t_get_by(xdb_ct:config()) -> ok.
t_get_by(_Config) ->
  #{'__meta__' := _, id := 1} = ?REPO:get_by(person, [{id, 1}]),
  ok.
