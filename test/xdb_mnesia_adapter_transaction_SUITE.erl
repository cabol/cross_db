-module(xdb_mnesia_adapter_transaction_SUITE).

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

% %% Test Cases
% -export([
%   t_all_with_different_table_types/1,
%   t_raw_query/1,
%   t_boot_repo_options/1
% ]).

%% Test Cases
-include_lib("mixer/include/mixer.hrl").
-mixin([
  {xdb_transaction_test, [
    % CT
    init_per_testcase/2,
    end_per_testcase/2,

    % Test Cases
    t_transaction/1,
    t_transaction_error/1,
    t_in_transaction/1,
    t_rollback/1
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
