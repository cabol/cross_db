%%%-------------------------------------------------------------------
%%% @doc
%%% Mnesia adapter.
%%%
%%% Based on SumoDB mnesia adapter `sumo_store_mnesia'
%%% @copyright 2012 Inaka &lt;hello@inaka.net&gt;
%%% @reference See
%%% <a href="https://github.com/inaka/sumo_db">SumoDB</a>
%%% @end
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_mnesia_adapter).

-behaviour(xdb_adapter).
-behaviour(xdb_adapter_transaction).

-export([
  insert/4,
  insert_all/4,
  update/5,
  delete/4,
  execute/5
]).

-export([
  transaction/3,
  in_transaction/1,
  rollback/2
]).

%%%===================================================================
%%% Adapter callbacks
%%%===================================================================

%% @hidden
insert(_Repo, #{schema := Schema, source := Source}, Fields, Opts) ->
  {PKs, FieldNames} = get_meta(Schema),
  OnConflict = xdb_lib:keyfind(on_conflict, Opts, raise),
  do_insert(Source, PKs, FieldNames, Fields, OnConflict).

%% @hidden
insert_all(_Repo, #{schema := Schema, source := Source}, List, Opts) ->
  {PKs, FieldNames} = get_meta(Schema),
  OnConflict = xdb_lib:keyfind(on_conflict, Opts, raise),
  do_insert_all(Source, PKs, FieldNames, List, OnConflict).

%% @hidden
update(_Repo, #{schema := Schema, source := Source}, Fields, Filters, _Opts) ->
  {PKs, FieldNames} = get_meta(Schema),
  do_update(Source, PKs, FieldNames, Fields, Filters).

%% @hidden
delete(_Repo, #{schema := Schema, source := Source}, Filters, _Opts) ->
  {PKs, FieldNames} = get_meta(Schema),
  {true, PkValues} = xdb_query:pk_filter(PKs, Filters),

  case do_delete(Source, PkValues) of
    {ok, Rec} ->
      {ok, rec_to_map(PKs, FieldNames, Rec)};
    Error ->
      Error
  end.

%% @hidden
execute(_Repo, Op, #{schema := Schema, source := Source}, Query, _Opts) ->
  do_execute(Op, Schema, Source, Query).

%% @hidden
transaction(_Repo, Fun, _Opts) ->
  TxFun =
    fun() ->
      case Fun() of
        {error, Reason} -> xdb_lib:raise(Reason);
        Ok              -> Ok
      end
    end,

  case mnesia:transaction(TxFun) of
    {aborted, {Error, Reason}} when is_list(Reason) ->
      {error, Error};
    {aborted, Reason} ->
      {error, Reason};
    {atomic, Result} ->
      {ok, Result}
  end.

%% @hidden
in_transaction(_Repo) ->
  mnesia:is_transaction().

%% @hidden
rollback(Repo, Value) ->
  case in_transaction(Repo) of
    true  -> error(Value);
    false -> xdb_exception:no_transaction_is_active()
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
do_insert(Source, PKs, FieldNames, Fields, OnConflict) ->
  maybe_transaction(fun() ->
    case on_conflict(Source, PKs, FieldNames, Fields, OnConflict) of
      {ok, _} -> {ok, Fields};
      Error   -> Error
    end
  end).

%% @private
do_insert_all(Source, PKs, FieldNames, List, OnConflict) ->
  ReduceFun =
    fun(Fields, Acc) ->
      case on_conflict(Source, PKs, FieldNames, Fields, OnConflict) of
        {ok, Record} ->
          {cont, [Record | Acc]};
        Error ->
          {halt, Error}
      end
    end,

  InsertAll =
    maybe_transaction(fun() ->
      xdb_lib:reduce_while(ReduceFun, [], List)
    end),

  case InsertAll of
    {error, _} = Error ->
      Error;
    Records ->
      {length(Records), to_fields(PKs, FieldNames, lists:reverse(Records))}
  end.

%% @private
on_conflict(Source, PKs, FieldNames, Fields, OnConflict) ->
  Record = map_to_rec(Source, PKs, FieldNames, Fields),

  case OnConflict of
    replace_all ->
      {mnesia:write(Record), Record};
    _ ->
      insert_new(Source, PKs, FieldNames, Fields, OnConflict, Record)
  end.

%% @private
insert_new(Source, PKs, FieldNames, Fields, OnConflict, Record) ->
  case {mnesia:read({Source, pk(PKs, Fields)}), OnConflict} of
    {[StoredRec], {replace, OnConflictFields}} ->
      StoredFields = rec_to_map(PKs, FieldNames, StoredRec),
      UpdatedFields = maps:merge(StoredFields, maps:with(OnConflictFields, Fields)),
      NewRec = map_to_rec(Source, PKs, FieldNames, UpdatedFields),
      {mnesia:write(NewRec), NewRec};
    {[StoredRec], nothing} ->
      {ok, StoredRec};
    {[_], raise} ->
      {error, conflict};
    {[], _} ->
      {mnesia:write(Record), Record}
  end.

%% @private
do_update(Source, PKs, FieldNames, Fields, Filters) ->
  maybe_transaction(fun() ->
    {true, PkValues} = xdb_query:pk_filter(PKs, Filters),
    Key = pk(xdb_lib:kv_values(PkValues)),

    case mnesia:read({Source, Key}) of
      [Record] ->
        {NewRecord, NewFields} = update_record(Record, Source, PKs, FieldNames, Fields),
        ok = mnesia:write(NewRecord),
        {ok, NewFields};
      [] ->
        {error, stale}
    end
  end).

%% @private
do_delete(Source, PkValues) ->
  maybe_transaction(fun() ->
    Key = pk(xdb_lib:kv_values(PkValues)),

    case mnesia:read({Source, Key}) of
      [Record] ->
        ok = mnesia:delete({Source, Key}),
        {ok, Record};
      [] ->
        {error, stale}
    end
  end).

%% @private
do_execute(Op, Schema, Source, #{raw := MatchSpec, limit := Limit, offset := Offset}) ->
  {PKs, FieldNames} = get_meta(Schema),
  do_execute(Op, Source, PKs, FieldNames, {MatchSpec, Limit, Offset});
do_execute(all, Schema, Source, #{where := Conditions, limit := Limit, offset := Offset}) ->
  {PKs, FieldNames} = get_meta(Schema),
  case xdb_query:pk_filter(PKs, Conditions) of
    {true, PkValues} ->
      do_execute(all, Source, PKs, FieldNames, {pk_query, PkValues});
    _ ->
      MatchSpec = build_match_spec(Source, FieldNames, Conditions),
      do_execute(all, Source, PKs, FieldNames, {MatchSpec, Limit, Offset})
  end;
do_execute(delete_all, _Schema, Source, #{where := []}) ->
  do_execute(delete_all, Source, undefined, undefined, all);
do_execute(Op, Schema, Source, #{where := Conditions, updates := Updates}) ->
  {PKs, FieldNames} = get_meta(Schema),
  MatchSpec = build_match_spec(Source, FieldNames, Conditions),
  case Op of
    update_all ->
      do_execute(Op, Source, PKs, FieldNames, {MatchSpec, Updates});
    _ ->
      do_execute(Op, Source, PKs, FieldNames, MatchSpec)
  end.

do_execute(all, Source, PKs, FieldNames, {pk_query, PkValues}) ->
  GetTx =
    fun() ->
       mnesia:read({Source, pk(xdb_lib:kv_values(PkValues))})
    end,

  Records = maybe_transaction(GetTx),
  {length(Records), to_fields(PKs, FieldNames, Records)};

do_execute(all, Source, PKs, FieldNames, {MatchSpec, Limit, Offset}) ->
  SelectAll =
    fun() ->
      mnesia:select(Source, MatchSpec)
    end,

  SelectBy =
    fun() ->
      case mnesia:select(Source, MatchSpec, Offset + Limit, read) of
        {Records, _Cont} when length(Records) >= Offset ->
          process_records(Source, Records, Limit, Offset);
        {_Records, _Cont} ->
          [];
        '$end_of_table' ->
          []
      end
    end,

  Transaction =
    case Limit of
      0 -> SelectAll;
      _ -> SelectBy
    end,

  Records = maybe_transaction(Transaction),
  {length(Records), to_fields(PKs, FieldNames, Records)};

do_execute(delete_all, Source, _PKs, _FieldNames, all) ->
  Count = mnesia:table_info(Source, size),
  case mnesia:clear_table(Source) of
    {atomic, ok} ->
      {Count, undefined};
    {aborted, Reason} ->
      xdb_lib:raise(Reason)
  end;

do_execute(delete_all, Source, PKs, FieldNames, MatchSpec) ->
  Transaction =
    fun() ->
      Records = mnesia:select(Source, MatchSpec),
      ok = lists:foreach(fun mnesia:delete_object/1, Records),
      Records
    end,

  Records = maybe_transaction(Transaction),
  {length(Records), to_fields(PKs, FieldNames, Records)};

do_execute(update_all, Source, PKs, FieldNames, {MatchSpec, Updates}) ->
  Fields = maps:from_list(Updates),
  Transaction =
    fun() ->
      [begin
        {NewRecord, NewFields} = update_record(Record, Source, PKs, FieldNames, Fields),
        ok = mnesia:write(NewRecord),
        NewFields
      end || Record <- mnesia:select(Source, MatchSpec)]
    end,

  ResL = maybe_transaction(Transaction),
  {length(ResL), ResL}.

%% @private
process_records(Source, Records, Limit, Offset) ->
  SortedRecords =
    case mnesia:table_info(Source, type) of
      ordered_set -> Records;
      _           -> lists:reverse(Records)
    end,

  lists:sublist(SortedRecords, Offset + 1, Limit).

%% @doc http://www.erlang.org/doc/apps/erts/match_spec.html
%% @private
build_match_spec(Source, FieldNames, Conditions) ->
  Fields = [field_tuple(I, FieldNames) || I <- lists:seq(1, length(FieldNames))],
  FieldsMap = maps:from_list(Fields),

  % The following ordering function avoids '$10' been added between
  % '$1' and '$2' in the MatchHead list. Without this fix, this store
  % would fail when trying to use `find_by` function.
  OrderingFun =
    fun(A, B) ->
      "$" ++ ANumber = atom_to_list(A),
      "$" ++ BNumber = atom_to_list(B),
      list_to_integer(ANumber) =< list_to_integer(BNumber)
    end,

  ValuesSorted = lists:sort(OrderingFun, maps:values(FieldsMap)),
  MatchHead = list_to_tuple([Source | ValuesSorted]),
  Guard = [condition_to_guard(Condition, FieldsMap) || Condition <- Conditions],
  Result = '$_',

  [{MatchHead, Guard, [Result]}].

%% @private
field_tuple(I, Fields) ->
  FieldName = lists:nth(I, Fields),
  FieldWildcard = list_to_atom([$$ | integer_to_list(I)]),
  {FieldName, FieldWildcard}.

%% @private
condition_to_guard({'and', [Expr1]}, FieldsMap) ->
  condition_to_guard(Expr1, FieldsMap);
condition_to_guard({'and', [Expr1 | Exprs]}, FieldsMap) ->
  {
    'andalso',
    condition_to_guard(Expr1, FieldsMap),
    condition_to_guard({'and', Exprs}, FieldsMap)
  };
condition_to_guard({'or', [Expr1]}, FieldsMap) ->
  condition_to_guard(Expr1, FieldsMap);
condition_to_guard({'or', [Expr1 | Exprs]}, FieldsMap) ->
  {
    'orelse',
    condition_to_guard(Expr1, FieldsMap),
    condition_to_guard({'or', Exprs}, FieldsMap)
  };
condition_to_guard({'not', Expr}, FieldsMap) ->
  {
    'not',
    condition_to_guard(Expr, FieldsMap)
  };
condition_to_guard({Name1, Op, Name2}, FieldsMap) when is_atom(Name2) ->
  %NOTE: Name2 can be a field name or a value, that's why the following happens
  {check_operator(Op), maps:get(Name1, FieldsMap), maps:get(Name2, FieldsMap, {const, Name2})};
condition_to_guard({Name1, Op, Value}, FieldsMap) ->
  {check_operator(Op), maps:get(Name1, FieldsMap), {const, Value}};
condition_to_guard({Name, 'null'}, FieldsMap) ->
  condition_to_guard({Name, '==', undefined}, FieldsMap);
condition_to_guard({Name, 'not_null'}, FieldsMap) ->
  condition_to_guard({Name, '/=', undefined}, FieldsMap);
condition_to_guard({Name, Value}, FieldsMap) ->
  condition_to_guard({Name, '==', Value}, FieldsMap).

%% @private
check_operator(like) ->
  error({unsupported_operator, like});
check_operator(Op) ->
  xdb_query:validate_operator(Op).

%% @private
get_meta(Schema) ->
  SchemaSpec = Schema:schema_spec(),
  PKFieldNames = xdb_schema_spec:pk_field_names(SchemaSpec),
  FieldNames = xdb_schema_spec:field_names(SchemaSpec),
  {PKFieldNames, FieldNames}.

%% @private
pk(Values) ->
  list_to_tuple(Values).

%% @private
pk(PKs, Fields) ->
  list_to_tuple(xdb_schema:get_pk_values(PKs, Fields)).

%% @private
to_fields(PKs, FieldNames, Records) when is_list(Records) ->
  [rec_to_map(PKs, FieldNames, Record) || Record <- Records].

%% @private
rec_to_map(PKs, FieldNames, Record) ->
  [_, PKValue | Values] = tuple_to_list(Record),
  PkPairs = lists:zip(PKs, tuple_to_list(PKValue)),
  KvPairs = lists:zip(FieldNames -- PKs, Values),
  maps:from_list(PkPairs ++ KvPairs).

%% @private
map_to_rec(Source, PKs, FieldNames, Fields) ->
  Key = pk(PKs, Fields),
  FieldValues = [maps:get(K, Fields, undefined) || K <- FieldNames -- PKs],
  list_to_tuple([Source, Key | FieldValues]).

%% @private
update_record(Record, Source, PKs, FieldNames, Fields) ->
  StoredFields = rec_to_map(PKs, FieldNames, Record),
  NewFields = maps:merge(StoredFields, maps:without(PKs, Fields)),
  NewRec = map_to_rec(Source, PKs, FieldNames, NewFields),
  {NewRec, NewFields}.

%% @private
exec_transaction(Fun) ->
  exec_transaction(Fun, fun xdb_lib:raise/1).

%% @private
exec_transaction(Fun, ErrorFun) ->
  case mnesia:transaction(Fun) of
    {aborted, {{_, Reason} = Error, _}} when is_list(Reason) ->
      ErrorFun(Error);
    {aborted, Reason} ->
      ErrorFun(Reason);
    {atomic, Result} ->
      Result
  end.

%% @private
maybe_transaction(Fun) ->
  maybe_transaction(Fun, mnesia:is_transaction()).

%% @private
maybe_transaction(Fun, true) ->
  Fun();
maybe_transaction(Fun, false) ->
  exec_transaction(Fun).
