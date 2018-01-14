%%%-------------------------------------------------------------------
%%% @doc
%%% Pool supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_pool_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/1
]).

%% Supervisor callbacks
-export([init/1]).

%% Default number of workers in the pool
-define(DEFAULT_POOL_SIZE, erlang:system_info(schedulers_online)).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link(Repos) -> Res when
  Name    :: atom(),
  Adapter :: module(),
  Opts    :: xdb_lib:kv(),
  Repos   :: [{Name, Adapter, Opts}],
  Res     :: {ok, pid()} | ignore | {error, term()}.
start_link(Repos) when is_list(Repos) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Repos).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init(Repos) ->
  PoolSpecs =
    [begin
      ok = ensure_loaded([RepoAdapter]),
      Opts = xdb_lib:to_kvlist(RepoOpts),
      PoolArgs = pool_args(RepoName, Opts),
      WorkersArgs = [{repo, RepoName}, {adapter, RepoAdapter} | Opts],
      poolboy:child_spec(RepoName, PoolArgs, WorkersArgs)
    end || {RepoName, RepoAdapter, RepoOpts} <- Repos],

  supervise(PoolSpecs, #{strategy => one_for_one}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
supervise(Children, SupFlagsMap) ->
  Strategy = maps:get(strategy, SupFlagsMap, one_for_one),
  Intensity = maps:get(intensity, SupFlagsMap, 10),
  Period = maps:get(period, SupFlagsMap, 10),
  {ok, {{Strategy, Intensity, Period}, Children}}.

%% @private
pool_args(Name, Opts) ->
  PoolSize = proplists:get_value(pool_size, Opts, ?DEFAULT_POOL_SIZE),
  MaxOverflow = proplists:get_value(pool_max_overflow, Opts, PoolSize),
  [
    {name, {local, Name}},
    {worker_module, xdb_repo_worker},
    {size, PoolSize},
    {max_overflow, MaxOverflow}
  ].

%% @private
ensure_loaded(Modules) ->
  lists:foreach(fun(Module) ->
    case code:ensure_loaded(Module) of
      {module, Module} -> ok;
      {error, Reason}  -> error({Reason, Module})
    end
  end, Modules).
