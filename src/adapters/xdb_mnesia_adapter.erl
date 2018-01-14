%%%-------------------------------------------------------------------
%%% @doc
%%% Mnesia adapter.
%%%
%%% Based on SumoDB `sumo_store_mnesia`
%%% @copyright 2012 Inaka &lt;hello@inaka.net&gt;
%%% @author Brujo Benavides <elbrujohalcon@inaka.net>
%%% @reference See
%%% <a href="https://github.com/inaka/sumo_db">SumoDB</a>
%%% @end
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_mnesia_adapter).

-behaviour(xdb_adapter).

-export([
  insert/4,
  update/5,
  delete/4,
  all/4
]).

%%%===================================================================
%%% Adapter callbacks
%%%===================================================================

%% @hidden
insert(_Repo, #{schema := Schema, source := Source}, Fields, Opts) ->
  {_, PKs, FieldNames} = get_meta(Schema),
  OnConflict = xdb_lib:keyfind(on_conflict, Opts, raise),
  do_insert(Source, PKs, FieldNames, Fields, OnConflict).

%% @hidden
update(_Repo, #{schema := Schema, source := Source}, Fields, Filters, _Opts) ->
  {_, PKs, FieldNames} = get_meta(Schema),
  do_update(Source, PKs, FieldNames, Fields, Filters).

%% @hidden
delete(_Repo, #{schema := Schema, source := Source}, Filters, _Opts) ->
  {_SchemaSpec, PKs, FieldNames} = get_meta(Schema),
  {true, PkValues} = xdb_query:pk_filter(PKs, Filters),

  case do_delete(Source, PkValues) of
    {ok, Rec} ->
      {ok, rec_to_map(PKs, FieldNames, Rec)};
    Error ->
      Error
  end.

%% @hidden
all(_Repo, #{schema := Schema, source := Source}, #{q_body := {raw, MatchSpec}}, Opts) ->
  {_SchemaSpec, PKs, FieldNames} = get_meta(Schema),
  Records = do_select(Source, MatchSpec, Opts),
  {length(Records), to_fields(PKs, FieldNames, Records)};
all(_Repo, #{schema := Schema, source := Source}, #{q_body := Conditions}, Opts) ->
  {_SchemaSpec, PKs, FieldNames} = get_meta(Schema),

  Records =
    case xdb_query:pk_filter(PKs, Conditions) of
      {true, PkValues} ->
        do_get(Source, PkValues);
      _ ->
        do_select(Source, FieldNames, Conditions, Opts)
    end,

  {length(Records), to_fields(PKs, FieldNames, Records)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
get_meta(Schema) ->
  SchemaSpec = Schema:schema_spec(),
  PKFieldNames = xdb_schema_spec:pk_field_names(SchemaSpec),
  FieldNames = xdb_schema_spec:field_names(SchemaSpec),
  {SchemaSpec, PKFieldNames, FieldNames}.

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
exec_transaction(Transaction) ->
  exec_transaction(Transaction, fun xdb_lib:raise/1).

%% @private
exec_transaction(Transaction, ErrorFun) ->
  case mnesia:transaction(Transaction) of
    {aborted, {{_, Reason} = Error, _}} when is_list(Reason) ->
      ErrorFun(Error);
    {aborted, Reason} ->
      ErrorFun(Reason);
    {atomic, Result} ->
      Result
  end.

%% @private
do_insert(Source, PKs, FieldNames, Fields, OnConflict) ->
  InsertNew =
    fun(Tab, Key, Rec) ->
      case mnesia:read({Tab, Key}) of
        [_] ->
          {error, conflict};
        [] ->
          ok = mnesia:write(Rec),
          {ok, Fields}
      end
    end,

  exec_transaction(fun() ->
    Key = pk(PKs, Fields),
    FieldValues = [maps:get(K, Fields, undefined) || K <- FieldNames -- PKs],
    Record = list_to_tuple([Source, Key | FieldValues]),

    case OnConflict of
      replace ->
        ok = mnesia:write(Record),
        {ok, Fields};
      nothing ->
        {ok, Fields};
      raise ->
        InsertNew(Source, Key, Record)
    end
  end).

%% @private
do_update(Source, PKs, FieldNames, Fields, Filters) ->
  exec_transaction(fun() ->
    {true, PkValues} = xdb_query:pk_filter(PKs, Filters),
    Key = pk(xdb_lib:kv_values(PkValues)),

    case mnesia:read({Source, Key}) of
      [StoredRecord] ->
        StoredFields = rec_to_map(PKs, FieldNames, StoredRecord),
        NewFields = maps:merge(StoredFields, Fields),
        FieldValues = [maps:get(K, NewFields, undefined) || K <- FieldNames -- PKs],
        Record = list_to_tuple([Source, Key | FieldValues]),
        ok = mnesia:write(Record),
        {ok, NewFields};
      [] ->
        {error, stale}
    end
  end).

%% @private
do_delete(Source, PkValues) ->
  exec_transaction(fun() ->
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
do_get(Source, PkValues) ->
  exec_transaction(fun() ->
    mnesia:read({Source, pk(xdb_lib:kv_values(PkValues))})
  end).

%% @private
do_select(Source, FieldNames, Conditions, Opts) ->
  MatchSpec = build_match_spec(Source, FieldNames, Conditions),
  do_select(Source, MatchSpec, Opts).

%% @private
do_select(Source, MatchSpec, Opts) ->
  Limit = xdb_lib:keyfind(limit, Opts, 0),
  Offset = xdb_lib:keyfind(offset, Opts, 0),

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

  SelectTx =
    case Limit of
      0 -> SelectAll;
      _ -> SelectBy
    end,

  exec_transaction(SelectTx).

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
