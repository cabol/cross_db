-module(xdb_mnesia_adapter_SUITE).

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-export([
  t_all_with_different_table_types/1,
  t_raw_query/1,
  t_boot_repo_options/1
]).

%% Test Cases
-include_lib("mixer/include/mixer.hrl").
-mixin([
  {xdb_repo_basic_test, [
    % CT
    init_per_testcase/2,
    end_per_testcase/2,

    % Test Cases
    t_insert/1,
    t_insert_errors/1,
    t_insert_on_conflict/1,
    t_insert_all/1,
    t_insert_all_on_conflict/1,
    t_update/1,
    t_delete/1,
    t_get/1,
    t_get_by/1,
    t_all/1,
    t_all_with_pagination/1,
    t_delete_all/1,
    t_delete_all_with_conditions/1,
    t_update_all/1,
    t_update_all_with_conditions/1
  ]}
]).

-define(EXCLUDED_FUNS, [
  module_info,
  all,
  init_per_suite,
  end_per_suite,
  init_per_testcase,
  end_per_testcase
]).

-include_lib("stdlib/include/ms_transform.hrl").

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
  [{repo, xdb_test_mnesia_repo} | Config].

-spec end_per_suite(xdb_ct:config()) -> ok.
end_per_suite(_) ->
  ok = application:stop(cross_db).

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec t_all_with_different_table_types(xdb_ct:config()) -> ok.
t_all_with_different_table_types(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  {atomic, ok} = mnesia:delete_table(people),
  ok = xdb_repo_sup:stop(Repo),
  _ = timer:sleep(500),
  {ok, _} = Repo:start_link([{type, ordered_set}]),
  ok = t_all_with_pagination(Config),
  {atomic, ok} = mnesia:delete_table(people),
  ok.

-spec t_raw_query(xdb_ct:config()) -> ok.
t_raw_query(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  ok = xdb_repo_basic_test:seed(Config),

  Expected = person:list_to_map(Repo:all(person)),

  MS = ets:fun2ms(fun(Record) -> Record end),
  Query = xdb_query:from(person, [{raw, MS}]),
  Expected = person:list_to_map(Repo:all(Query)),
  ok.

-spec t_boot_repo_options(xdb_ct:config()) -> ok.
t_boot_repo_options(_Config) ->
  Opts = [
    {schemas, [person]},
    {disc_copies, local},
    {disc_copies, [node()]},
    {disc_only_copies, local},
    {disc_only_copies, [node()]},
    {ram_copies, local},
    {ram_copies, [node()]},
    {majority, true},
    {snmp, {}},
    {storage_properties, {storage_properties, [{ets, [compressed]}]}},
    {type, set},
    {type, ordered_set},
    {unknown, unknown}
  ],

  try
    _ = xdb_mnesia_boot_repo:init(Opts), ok
  catch
    _:_ -> ok
  end.
