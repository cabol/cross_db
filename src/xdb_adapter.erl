%%%-------------------------------------------------------------------
%%% @doc
%%% Adapter's behaviour.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_adapter).

%%%===================================================================
%%% Types
%%%===================================================================

-type t() :: module().

-type schema_meta() :: #{
  schema := module(),
  source := atom()
}.

-type constraints() :: xdb_lib:keyword().

-type r_response() :: {Count :: integer(), [xdb_schema:fields()] | undefined}
                    | no_return().

-type w_response() :: {ok, xdb_schema:fields()}
                    | {invalid, constraints()}
                    | no_return().

-export_type([
  t/0,
  schema_meta/0,
  constraints/0
]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-optional_callbacks([
  child_spec/2
]).

-callback child_spec(Repo, Opts) -> Res when
  Repo :: xdb_repo:t(),
  Opts :: xdb_lib:keyword(),
  Res  :: supervisor:child_spec().

-callback all(Repo, QueryMeta, Query, Opts) -> Res when
  Repo      :: xdb_repo:t(),
  QueryMeta :: schema_meta(),
  Query     :: xdb_query:t(),
  Opts      :: xdb_lib:keyword(),
  Res       :: r_response().

-callback delete(Repo, SchemaMeta, Filters, Opts) -> Res when
  Repo       :: xdb_repo:t(),
  SchemaMeta :: schema_meta(),
  Filters    :: xdb_lib:keyword(),
  Opts       :: xdb_lib:keyword(),
  Res        :: w_response() | {error, stale}.

-callback insert(Repo, SchemaMeta, Fields, Opts) -> Res when
  Repo       :: xdb_repo:t(),
  SchemaMeta :: schema_meta(),
  Fields     :: xdb_schema:fields(),
  Opts       :: xdb_lib:keyword(),
  Res        :: w_response() | {error, conflict}.

-callback update(Repo, SchemaMeta, Fields, Filters, Opts) -> Res when
  Repo       :: xdb_repo:t(),
  SchemaMeta :: schema_meta(),
  Fields     :: xdb_schema:fields(),
  Filters    :: xdb_lib:keyword(),
  Opts       :: xdb_lib:keyword(),
  Res        :: w_response() | {error, stale}.
