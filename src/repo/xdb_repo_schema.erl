%%%-------------------------------------------------------------------
%%% @doc
%%% Schema Repo.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_repo_schema).

%% API
-export([
  insert/3,
  insert/4,
  update/3,
  update/4,
  delete/3,
  delete/4
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv insert(Repo, Adapter, Data, [])
insert(Repo, Adapter, Data) ->
  insert(Repo, Adapter, Data, []).

-spec insert(Repo, Adapter, Data, Opts) -> Res when
  Repo    :: xdb_repo:t(),
  Adapter :: xdb_adapter:t(),
  Data    :: xdb_schema:t() | xdb_changeset:t(),
  Opts    :: xdb_lib:keyword(),
  Res     :: xdb_repo:w_respose().
insert(Repo, Adapter, #{'__meta__' := #{schema := _}} = Schema, Opts) ->
  Changeset = xdb_changeset:change(Schema, #{}),
  do_insert(Repo, Adapter, Changeset, Opts);
insert(Repo, Adapter, Changeset, Opts) ->
  do_insert(Repo, Adapter, Changeset, Opts).

%% @equiv update(Repo, Adapter, Changeset, [])
update(Repo, Adapter, Changeset) ->
  update(Repo, Adapter, Changeset, []).

-spec update(Repo, Adapter, Changeset, Opts) -> Res when
  Repo      :: xdb_repo:t(),
  Adapter   :: xdb_adapter:t(),
  Changeset :: xdb_changeset:t(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_repo:w_respose().
update(Repo, Adapter, Changeset, Opts) ->
  do_update(Repo, Adapter, Changeset, Opts).

%% @equiv delete(Repo, Adapter, Data, [])
delete(Repo, Adapter, Data) ->
  delete(Repo, Adapter, Data, []).

-spec delete(Repo, Adapter, Data, Opts) -> Res when
  Repo    :: xdb_repo:t(),
  Adapter :: xdb_adapter:t(),
  Data    :: xdb_schema:t() | xdb_changeset:t(),
  Opts    :: xdb_lib:keyword(),
  Res     :: xdb_repo:w_respose().
delete(Repo, Adapter, #{'__meta__' := #{schema := _}} = Schema, Opts) ->
  Changeset = xdb_changeset:change(Schema, #{}),
  do_delete(Repo, Adapter, Changeset, Opts);
delete(Repo, Adapter, Changeset, Opts) ->
  do_delete(Repo, Adapter, Changeset, Opts).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
do_insert(Repo, Adapter, #{is_valid := true} = Changeset, Opts0) ->
  {Meta, _Data, Opts} = get_metadata(Changeset, Opts0),
  Changes = dump_changes(insert, Changeset),

  Changeset1 = put_repo_and_action(Changeset, insert, Repo),

  Args = [Repo, Meta, Changes, Opts],
  exec(Changeset1, Adapter, insert, Args);
do_insert(Repo, _Adapter, #{is_valid := false} = Changeset, _Opts) ->
  {error, put_repo_and_action(Changeset, insert, Repo)}.

%% @private
do_update(Repo, Adapter, #{is_valid := true} = Changeset, Opts0) ->
  {Meta, Data, Opts} = get_metadata(Changeset, Opts0),
  Changes = dump_changes(update, Changeset),
  Filters = add_pk_filter(Data, xdb_changeset:filters(Changeset)),

  % Differently from insert, update does not copy the schema
  % fields into the changeset. All changes must be in the
  % changeset before hand.
  Changeset1 = put_repo_and_action(Changeset, update, Repo),

  Args = [Repo, Meta, Changes, Filters, Opts],
  exec(Changeset1, Adapter, update, Args);
do_update(Repo, _Adapter, #{is_valid := false} = Changeset, _Opts) ->
  {error, put_repo_and_action(Changeset, update, Repo)}.

%% @private
do_delete(Repo, Adapter, #{is_valid := true} = Changeset, Opts0) ->
  {Meta, Data, Opts} = get_metadata(Changeset, Opts0),
  Filters = add_pk_filter(Data, xdb_changeset:filters(Changeset)),

  Changeset1 = put_repo_and_action(Changeset, delete, Repo),
  Changeset2 = Changeset1#{changes := #{}},

  Args = [Repo, Meta, Filters, Opts],
  exec(Changeset2, Adapter, delete, Args);
do_delete(Repo, _Adapter, #{is_valid := false} = Changeset, _Opts) ->
  {error, put_repo_and_action(Changeset, delete, Repo)}.

%% @private
get_metadata(Changeset, Opts) ->
  Data = xdb_changeset:data(Changeset),
  Meta = maps:with([schema, source], xdb_schema:metadata(Data)),
  {Meta, Data, Opts}.

%% @private
dump_changes(insert, Changeset) ->
  Schema = xdb_changeset:apply_changes(Changeset),
  xdb_schema:fields(Schema);
dump_changes(update, Changeset) ->
  xdb_changeset:changes(Changeset).

%% @private
put_repo_and_action(#{action := Given}, Action, Repo) when Given /= undefined, Given /= Action ->
  Text = "a changeset with action ~p was given to ~p:~p/2",
  Reason = xdb_lib:stringify(Text, [Given, Repo, Action]),
  error({badarg, Reason});
put_repo_and_action(Changeset, Action, Repo) ->
  Changeset#{action := Action, repo := Repo}.

%% @private
load_changes(Changeset, State, Values) ->
  try
    Data = apply_metadata(xdb_changeset:data(Changeset), State),
    Fields = xdb_lib:to_map(Values),
    xdb_schema:set_fields(Data, Fields)
  catch
    error:{bad_type, {FName, FType, FValue}} ->
      Text = "cannot load ~p as type ~p for field ~p in schema ~p",
      Reason = xdb_lib:stringify(Text, [FValue, FType, FName, xdb_changeset:schema(Changeset)]),
      error({badarg, Reason})
  end.

%% @private
apply_metadata(#{'__meta__' := Meta} = Data, State) ->
  Data#{'__meta__' := Meta#{state := State}}.

%% @private
add_pk_filter(Schema, Filters) ->
  maps:fold(fun(K, V, Acc) ->
    [{K, V} | Acc]
  end, xdb_lib:to_kvlist(Filters), xdb_schema:pk_fields(Schema)).

%% @private
exec(Changeset, Adapter, Action, Args) ->
  Result = apply(Adapter, Action, Args),
  process_result(Result, Changeset).

%% @private
process_result({ok, Values}, Changeset) ->
  {ok, load_changes(Changeset, loaded, Values)};
process_result({invalid, Constraints}, _Changeset) ->
  %% @TODO: constraints_to_errors (add errors to changeset)
  {error, Constraints};
process_result({error, conflict}, Changeset) ->
  error({conflict, xdb_changeset:data(Changeset)});
process_result({error, stale}, Changeset) ->
  xdb_exception:stale_entry_error(xdb_changeset:action(Changeset), xdb_changeset:data(Changeset));
process_result({error, Reason}, _Changeset) ->
  error(Reason).
