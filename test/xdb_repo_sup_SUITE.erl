-module(xdb_repo_sup_SUITE).

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-export([
  t_start_stop/1,
  t_return_ignore/1,
  t_missing_repo_config/1,
  t_no_repo_init_fun/1,
  t_sup_spec/1
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

-spec t_start_stop(xdb_ct:config()) -> ok.
t_start_stop(_Config) ->
  {ok, Pid} = xdb_repo_sup:start_link(?REPO, cross_db, ?ADAPTER, []),
  ok = xdb_repo_sup:stop(wrong),
  true = process_info(Pid) /= undefined,
  ok = xdb_repo_sup:stop(?REPO),
  _ = timer:sleep(500),
  undefined = process_info(Pid),
  ok.

-spec t_return_ignore(xdb_ct:config()) -> ok.
t_return_ignore(_Config) ->
  ignore = xdb_repo_sup:start_link(?REPO, cross_db, ?ADAPTER, [{start, ignore}]),
  ok.

-spec t_missing_repo_config(xdb_ct:config()) -> ok.
t_missing_repo_config(_Config) ->
  _ = process_flag(trap_exit, true),

  Expected = {badarg, "configuration for other_repo not specified in cross_db environment"},
  {error, {Expected, _}} = assert_error(fun() ->
    xdb_repo_sup:start_link(other_repo, cross_db, ?ADAPTER, [])
  end, badarg),
  ok.

-spec t_no_repo_init_fun(xdb_ct:config()) -> ok.
t_no_repo_init_fun(_Config) ->
  _ = xdb_repo_sup:start_link(xdb_test_repo_and_adapter, cross_db, xdb_test_repo_and_adapter, []),
  ok.

-spec t_sup_spec(xdb_ct:config()) -> ok.
t_sup_spec(_Config) ->
  #{
    id    := ?REPO,
    type  := supervisor,
    start := {?REPO, start_link, []}
  } = ?REPO:supervisor(),
  ok.
