%%%-------------------------------------------------------------------
%%% @doc
%%% Common exceptions.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_exception).

-export([
  invalid_changeset_error/1,
  no_primary_key_field_error/1,
  no_primary_key_value_error/2,
  multiple_results_error/2,
  no_results_error/1,
  stale_entry_error/2,
  no_transaction_is_active/0
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec invalid_changeset_error(xdb_changeset:t()) -> no_return().
invalid_changeset_error(Changeset) ->
  xdb_lib:raise(invalid_changeset_error, Changeset).

-spec no_primary_key_field_error(xdb_schema:t()) -> no_return().
no_primary_key_field_error(Schema) ->
  Text = "schema ~p has no primary key",
  xdb_lib:raise(no_primary_key_value_error, Text, [Schema]).

-spec no_primary_key_value_error(atom(), xdb_schema:fields()) -> no_return().
no_primary_key_value_error(Key, Fields) ->
  Text = "missing value for primary key ~p in fields ~p",
  xdb_lib:raise(no_primary_key_value_error, Text, [Key, Fields]).

-spec multiple_results_error(xdb_query:t(), integer()) -> no_return().
multiple_results_error(Query, Count) when is_integer(Count) ->
  Text = "expected at most one result in query ~p but got ~p",
  xdb_lib:raise(multiple_results_error, Text, [Query, Count]).

-spec no_results_error(xdb_query:t()) -> no_return().
no_results_error(Query) ->
  Text = "expected at least one result but got none in query: ~p",
  xdb_lib:raise(no_results_error, Text, [Query]).

-spec stale_entry_error(atom(), xdb_schema:t()) -> no_return().
stale_entry_error(Action, Schema) ->
  Text = "attempted to ~p a stale schema: ~p",
  xdb_lib:raise(stale_entry_error, Text, [Action, Schema]).

-spec no_transaction_is_active() -> no_return().
no_transaction_is_active() ->
  xdb_lib:raise(no_transaction_is_active, "cannot call rollback outside of transaction").
