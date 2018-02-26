%%%-------------------------------------------------------------------
%%% @doc
%%% Repo main interface.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_repo).

%%%===================================================================
%%% Types
%%%===================================================================

%% Repo type
-type t() :: module().

%% Write command response (delete, insert and update)
-type w_respose() :: {ok, xdb_schema:t()}
                   | {error, xdb_changeset:t()}
                   | no_return().

-export_type([
  t/0,
  w_respose/0
]).

%%%===================================================================
%%% API
%%%===================================================================

-callback init(Opts) -> Res when
  Opts :: xdb_lib:keyword(),
  Res  :: {ok, xdb_lib:keyword()} | ignore.

-callback all(Queryable, Opts) -> Res when
  Queryable :: xdb_query:t() | xdb_query:queryable(),
  Opts      :: xdb_lib:keyword(),
  Res       :: [xdb_schema:t()] | no_return().

-callback delete(Data, Opts) -> Res when
  Data :: xdb_schema:t() | xdb_changeset:t(),
  Opts :: xdb_lib:keyword(),
  Res  :: w_respose().

-callback delete_all(Queryable, Opts) -> Res when
  Queryable :: xdb_query:t() | xdb_query:queryable(),
  Opts      :: xdb_lib:keyword(),
  Res       :: {integer(), [any()] | undefined} | no_return().

-callback get(Queryable, Id, Opts) -> Res when
  Queryable :: xdb_query:queryable(),
  Id        :: any(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | undefined | no_return().

-callback get_by(Queryable, Clauses, Opts) -> Res when
  Queryable :: xdb_query:queryable(),
  Clauses   :: xdb_lib:keyword(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | undefined | no_return().

-callback insert(Schema, Opts) -> Res when
  Schema :: xdb_schema:t(),
  Opts   :: xdb_lib:keyword(),
  Res    :: w_respose().

-callback update(Changeset, Opts) -> Res when
  Changeset :: xdb_changeset:t(),
  Opts      :: xdb_lib:keyword(),
  Res       :: w_respose().
