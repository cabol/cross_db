%%%-------------------------------------------------------------------
%%% @doc
%%% Queryable Repo.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_repo_queryable).

%% API
-export([
  all/3,
  all/4,
  get/4,
  get/5,
  get_by/4,
  get_by/5
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv all(Repo, Adapter, Queryable, [])
all(Repo, Adapter, Queryable) ->
  all(Repo, Adapter, Queryable, []).

-spec all(Repo, Adapter, Queryable, Opts) -> Res when
  Repo      :: xdb_repo:t(),
  Adapter   :: xdb_adapter:t(),
  Queryable :: xdb_query:t() | xdb_query:queryable(),
  Opts      :: xdb_lib:keyword(),
  Res       :: [xdb_schema:t()] | no_return().
all(Repo, Adapter, Queryable, Opts) when is_atom(Queryable) ->
  Query = xdb_query:new(Queryable),
  all(Repo, Adapter, Query, Opts);
all(Repo, Adapter, #{schema := Schema} = Queryable, Opts) ->
  FieldsLst = element(2, Adapter:all(Repo, get_metadata(Queryable), Queryable, Opts)),
  [Schema:schema(Fields) || Fields <- FieldsLst].

%% @equiv get(Repo, Adapter, Queryable, Id, [])
get(Repo, Adapter, Queryable, Id) ->
  get(Repo, Adapter, Queryable, Id, []).

-spec get(Repo, Adapter, Queryable, Id, Opts) -> Res when
  Repo      :: xdb_repo:t(),
  Adapter   :: xdb_adapter:t(),
  Queryable :: xdb_query:queryable(),
  Id        :: any(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | undefined | no_return().
get(Repo, Adapter, Queryable, Id, Opts) when is_atom(Queryable) ->
  Query = query_for_get(Repo, Queryable, Id),
  one(Repo, Adapter, Queryable, Query, Opts).

%% @equiv get_by(Repo, Adapter, Queryable, Clauses, [])
get_by(Repo, Adapter, Queryable, Clauses) ->
  get_by(Repo, Adapter, Queryable, Clauses, []).

-spec get_by(Repo, Adapter, Queryable, Clauses, Opts) -> Res when
  Repo      :: xdb_repo:t(),
  Adapter   :: xdb_adapter:t(),
  Queryable :: xdb_query:queryable(),
  Clauses   :: xdb_lib:keyword(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | undefined | no_return().
get_by(Repo, Adapter, Queryable, Clauses, Opts) when is_atom(Queryable) ->
  one(Repo, Adapter, Queryable, Clauses, Opts).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
one(Repo, Adapter, Queryable, Clauses, Opts) ->
  Query = xdb_query:new(Queryable, Clauses),

  case all(Repo, Adapter, Query, Opts) of
    []    -> undefined;
    [One] -> One;
    Other -> xdb_exception:multiple_results_error(Queryable, length(Other))
  end.

%% @private
query_for_get(Repo, SchemaMod, Id) ->
  case SchemaMod:primary_key() of
    [PK] ->
      [{PK, Id}];
    PKs ->
      Text = "~p:get/3 requires the schema ~p to have exactly one primary key, got: ~p",
      xdb_lib:raise(badarg, Text, [Repo, SchemaMod, PKs])
  end.

%% @private
get_metadata(#{schema := Queryable, source := Source}) ->
  #{schema => Queryable, source => Source}.
