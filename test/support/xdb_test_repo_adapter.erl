-module(xdb_test_repo_adapter).

-export([
  child_spec/2,
  start_link/0
]).

%% @hidden
child_spec(_Repo, _Opts) ->
  {
    ?MODULE,
    {?MODULE, start_link, []},
    permanent,
    infinity,
    supervisor,
    [?MODULE]
  }.

%% @hidden
start_link() ->
  {ok, spawn_link(fun() -> receive M -> M end end)}.
