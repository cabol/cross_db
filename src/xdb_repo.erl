%%%-------------------------------------------------------------------
%%% @doc
%%% Defines a repository.
%%%
%%% A repository maps to an underlying data store, controlled by the adapter.
%%% For example, CrossDB ships with a `xdb_mnesia_adapter' that stores data into
%%% Mnesia database.
%%%
%%% When used, the repository expects the `otp_app' and `adapter' as options.
%%% The `otp_app' should point to an OTP application that has the repository
%%% configuration, and the `adapter' is a compile-time option that specifies
%%% the adapter itself and should point to an existing and valid module that
%%% implements the `xdb_adapter' behaviour. For example, the repository:
%%%
%%% ```
%%% -module(blog_repo).
%%%
%%% -include_lib("cross_db/include/xdb.hrl").
%%% -repo([{otp_app, blog}, {adapter, xdb_mnesia_adapter}]).
%%% '''
%%%
%%% The configuration for the repo is mandatory, the previous repo
%%% could be configured with:
%%%
%%% ```
%%% [
%%%   {cross_db, [
%%%     {my_repo, [
%%%       {adapter, xdb_mnesia_adapter},
%%%       {ram_copies, local},
%%%       {schemas, [person]}
%%%     ]}
%%%   ]}
%%% ].
%%% '''
%%%
%%% Most of the configuration that goes into the `*.config' file is specific
%%% to the adapter, so check the documentation provided for each adapter.
%%% However, some configuration is shared across
%%% all adapters, they are:
%%%
%%% <ul>
%%% <li>
%%%   `adapter' - a compile-time option that specifies the adapter itself.
%%%   As a compile-time option, it may also be given as an option to
%%%   `-repo([..])'.
%%% </li>
%%% </ul>
%%%
%%% <h3>Shared Options</h3>
%%%
%%% The following options are supported almost for all repositories:
%%%
%%% <ul>
%%% <li>
%%%   `timeout' - The time in milliseconds to wait for the query call to
%%%   finish, `infinity' will wait indefinitely (default: 15000).
%%% </li>
%%% <li>
%%%   `pool_timeout' - The time in milliseconds to wait for calls to the pool
%%%   to finish, `infinity' will wait indefinitely (default: 5000).
%%% </li>
%%% </ul>
%%%
%%% For extra options, check adapters documentation.
%%%
%%% @reference See
%%% <a href="https://github.com/cabol/cross_db/blob/master/guides/repo-api.md">Repo API</a>
%%% @end
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

%% Execute the actiopn or raise an error
-type exec_or_raise(R) :: R | no_return().

-export_type([
  t/0,
  w_respose/0,
  exec_or_raise/1
]).

%%%===================================================================
%%% API
%%%===================================================================

-optional_callbacks([init/1, in_transaction/0, rollback/1, transaction/2]).

-callback init(Config) -> Res when
  Config :: xdb_lib:keyword(),
  Res    :: {ok, xdb_lib:keyword()} | ignore.

-callback all(Queryable, Opts) -> Res when
  Queryable :: xdb_query:t() | xdb_query:queryable(),
  Opts      :: xdb_lib:keyword(),
  Res       :: [xdb_schema:t()] | no_return().

-callback get(Queryable, Id, Opts) -> Res when
  Queryable :: xdb_query:queryable(),
  Id        :: any(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | undefined | no_return().

-callback get_or_raise(Queryable, Id, Opts) -> Res when
  Queryable :: xdb_query:queryable(),
  Id        :: any(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | no_return().

-callback get_by(Queryable, Clauses, Opts) -> Res when
  Queryable :: xdb_query:queryable(),
  Clauses   :: xdb_lib:keyword(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | undefined | no_return().

-callback get_by_or_raise(Queryable, Clauses, Opts) -> Res when
  Queryable :: xdb_query:queryable(),
  Clauses   :: xdb_lib:keyword(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | no_return().

-callback insert(Schema, Opts) -> Res when
  Schema :: xdb_schema:t(),
  Opts   :: xdb_lib:keyword(),
  Res    :: w_respose().

-callback insert_or_raise(Schema, Opts) -> Res when
  Schema :: xdb_schema:t(),
  Opts   :: xdb_lib:keyword(),
  Res    :: exec_or_raise(xdb_schema:t()).

-callback insert_all(SchemaMod, Entries, Opts) -> Res when
  SchemaMod :: module(),
  Entries   :: [xdb_schema:fields()],
  Opts      :: xdb_lib:keyword(),
  Count     :: integer(),
  Returning :: [xdb_schema:fields()] | undefined,
  Res       :: {Count, Returning} | no_return().

-callback delete(Data, Opts) -> Res when
  Data :: xdb_schema:t() | xdb_changeset:t(),
  Opts :: xdb_lib:keyword(),
  Res  :: w_respose().

-callback delete_or_raise(Data, Opts) -> Res when
  Data :: xdb_schema:t() | xdb_changeset:t(),
  Opts :: xdb_lib:keyword(),
  Res  :: exec_or_raise(xdb_schema:t()).

-callback delete_all(Queryable, Opts) -> Res when
  Queryable :: xdb_query:t() | xdb_query:queryable(),
  Opts      :: xdb_lib:keyword(),
  Res       :: {integer(), [any()] | undefined} | no_return().

-callback update(Changeset, Opts) -> Res when
  Changeset :: xdb_changeset:t(),
  Opts      :: xdb_lib:keyword(),
  Res       :: w_respose().

-callback update_or_raise(Changeset, Opts) -> Res when
  Changeset :: xdb_changeset:t(),
  Opts      :: xdb_lib:keyword(),
  Res       :: exec_or_raise(xdb_schema:t()).

-callback update_all(Queryable, Updates, Opts) -> Res when
  Queryable :: xdb_query:t() | xdb_query:queryable(),
  Updates   :: xdb_lib:keyword(),
  Opts      :: xdb_lib:keyword(),
  Res       :: {integer(), [any()] | undefined} | no_return().

-callback in_transaction() -> boolean().

-callback rollback(any()) -> no_return().

-callback transaction(Fun, Opts) -> Res when
  Fun  :: fun(() -> any()),
  Opts :: xdb_lib:keyword(),
  Res  :: {ok, any()} | {error, any()} | {error, any(), [tuple()]}.
