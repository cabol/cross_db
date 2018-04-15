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
  get_or_raise/4,
  get_or_raise/5,
  get_by/4,
  get_by/5,
  get_by_or_raise/4,
  get_by_or_raise/5,
  delete_all/3,
  delete_all/4,
  update_all/4,
  update_all/5,
  transaction/3,
  transaction/4
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
all(Repo, Adapter, Queryable, Opts) ->
  execute(all, Repo, Adapter, Queryable, Opts).

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

%% @equiv get_or_raise(Repo, Adapter, Queryable, Id, [])
get_or_raise(Repo, Adapter, Queryable, Id) ->
  get_or_raise(Repo, Adapter, Queryable, Id, []).

-spec get_or_raise(Repo, Adapter, Queryable, Id, Opts) -> Res when
  Repo      :: xdb_repo:t(),
  Adapter   :: xdb_adapter:t(),
  Queryable :: xdb_query:queryable(),
  Id        :: any(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | no_return().
get_or_raise(Repo, Adapter, Queryable, Id, Opts) when is_atom(Queryable) ->
  Query = query_for_get(Repo, Queryable, Id),
  one_or_raise(Repo, Adapter, Queryable, Query, Opts).

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

%% @equiv get_by_or_raise(Repo, Adapter, Queryable, Clauses, [])
get_by_or_raise(Repo, Adapter, Queryable, Clauses) ->
  get_by_or_raise(Repo, Adapter, Queryable, Clauses, []).

-spec get_by_or_raise(Repo, Adapter, Queryable, Clauses, Opts) -> Res when
  Repo      :: xdb_repo:t(),
  Adapter   :: xdb_adapter:t(),
  Queryable :: xdb_query:queryable(),
  Clauses   :: xdb_lib:keyword(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | no_return().
get_by_or_raise(Repo, Adapter, Queryable, Clauses, Opts) when is_atom(Queryable) ->
  one_or_raise(Repo, Adapter, Queryable, Clauses, Opts).

%% @equiv delete_all(Repo, Adapter, Queryable, [])
delete_all(Repo, Adapter, Queryable) ->
  delete_all(Repo, Adapter, Queryable, []).

-spec delete_all(Repo, Adapter, Queryable, Opts) -> Res when
  Repo      :: xdb_repo:t(),
  Adapter   :: xdb_adapter:t(),
  Queryable :: xdb_query:t() | xdb_query:queryable(),
  Opts      :: xdb_lib:keyword(),
  Res       :: {integer(), [any()]} | no_return().
delete_all(Repo, Adapter, Queryable, Opts) ->
  execute(delete_all, Repo, Adapter, Queryable, Opts).

%% @equiv update_all(Repo, Adapter, Queryable, Updates, [])
update_all(Repo, Adapter, Queryable, Updates) ->
  update_all(Repo, Adapter, Queryable, Updates, []).

-spec update_all(Repo, Adapter, Queryable, Updates, Opts) -> Res when
  Repo      :: xdb_repo:t(),
  Adapter   :: xdb_adapter:t(),
  Queryable :: xdb_query:t() | xdb_query:queryable(),
  Updates   :: xdb_lib:keyword(),
  Opts      :: xdb_lib:keyword(),
  Res       :: {integer(), [any()]} | no_return().
update_all(Repo, Adapter, Queryable, Updates, Opts) when is_atom(Queryable) ->
  Query = xdb_query:from(Queryable),
  execute(update_all, Repo, Adapter, Query#{updates => Updates}, Opts);
update_all(Repo, Adapter, #{from := _, source := _} = Query, Updates, Opts) ->
  execute(update_all, Repo, Adapter, Query#{updates => Updates}, Opts).

%% @equiv transaction(Repo, Adapter, Fun, [])
transaction(Repo, Adapter, Fun) ->
  transaction(Repo, Adapter, Fun, []).

-spec transaction(Repo, Adapter, Fun, Opts) -> Res when
  Repo    :: xdb_repo:t(),
  Adapter :: xdb_adapter:t(),
  Fun     :: fun(() -> any()),
  Opts    :: xdb_lib:keyword(),
  Res     :: {ok, any()} | {error, any()}.
transaction(Repo, Adapter, Fun, Opts) when is_function(Fun, 0) ->
  Adapter:transaction(Repo, Fun, Opts).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
execute(Op, Repo, Adapter, Queryable, Opts) when is_atom(Queryable) ->
  Query = xdb_query:from(Queryable),
  execute(Op, Repo, Adapter, Query, Opts);
execute(all, Repo, Adapter, #{from := Schema} = Queryable, Opts) ->
  Result = Adapter:execute(Repo, all, get_metadata(Queryable), Queryable, Opts),
  [Schema:schema(Fields) || Fields <- element(2, Result)];
execute(Op, Repo, Adapter, #{from := Schema} = Queryable, Opts) ->
  case Adapter:execute(Repo, Op, get_metadata(Queryable), Queryable, Opts) of
    {Count, ResL} when is_list(ResL) ->
      {Count, [Schema:schema(Fields) || Fields <- ResL]};
    {_, undefined} = Res ->
      Res
  end.

%% @private
one(Repo, Adapter, Queryable, Clauses, Opts) ->
  Query = xdb_query:from(Queryable, [{where, Clauses}]),

  case all(Repo, Adapter, Query, Opts) of
    [One] -> One;
    []    -> undefined;
    Other -> xdb_exception:multiple_results_error(Query, length(Other))
  end.

%% @private
one_or_raise(Repo, Adapter, Queryable, Clauses, Opts) ->
  Query = xdb_query:from(Queryable, [{where, Clauses}]),

  case all(Repo, Adapter, Query, Opts) of
    [One] -> One;
    []    -> xdb_exception:no_results_error(Query);
    Other -> xdb_exception:multiple_results_error(Query, length(Other))
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
get_metadata(#{from := Queryable, source := Source}) ->
  #{schema => Queryable, source => Source}.
