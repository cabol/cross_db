%%%-------------------------------------------------------------------
%%% @doc
%%% Specifies the adapter transactions API.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_adapter_transaction).

-callback in_transaction(Repo) -> Res when
  Repo :: xdb_repo:t(),
  Res  :: boolean().

-callback rollback(Repo, Value) -> Res when
  Repo  :: xdb_repo:t(),
  Value :: any(),
  Res   :: no_return().

-callback transaction(Repo, Fun, Opts) -> Res when
  Repo :: xdb_repo:t(),
  Fun  :: fun(() -> any()),
  Opts :: xdb_lib:keyword(),
  Res  :: {ok, any()} | {error, any()}.
