%%%-------------------------------------------------------------------
%%% @doc
%%% SQL Utilities.
%%%
%%% Based on SumoDB `sumo_sql_builder'.
%%% @copyright 2012 Inaka &lt;hello@inaka.net&gt;
%%% @reference See
%%% <a href="https://github.com/inaka/sumo_db">SumoDB</a>
%%% @end
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_sql).

%% Basic SQL builder
-export([
  i/2,
  i_all/2,
  u/3,
  d/1,
  d/2,
  s/7,
  s_count/3,
  s_count/4
]).

%% Query utilities
-export([
  parse_conditions/1,
  where_clause/1,
  where_clause/2,
  where_clause/3,
  order_by_clause/1,
  order_by_clause/2
]).

%% Extras
-export([
  escape/1,
  slot_numbered/1,
  slot_question/1
]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

-type tab_name()  :: atom() | string().
-type field()     :: atom() | string().
-type condition() :: {'and', [condition()]}
                   | {'or', [condition()]}
                   | {field(), term()}.

-define(logical_op(Op_), Op_ == 'and'; Op_ == 'or'; Op_ == 'not').
-define(null_value(V_), V_ =/= 'null', V_ =/= 'not_null').

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns number of results, useful for pagination.
-spec s_count(Table, SelectFields, ExtraWhere) -> Res when
  Table        :: tab_name(),
  SelectFields :: [field()],
  ExtraWhere   :: string(),
  Res          :: {iolist(), [term()]}.
s_count(Table, _SelectFields, _ExtraWhere) ->
  {
    ["SELECT COUNT(1) AS `count` FROM ", escape(Table)],
    []
  }.

%% @doc Returns number of results, useful for pagination.
-spec s_count(Table, SelectFields, Conditions, ExtraWhere) -> Res when
  Table        :: tab_name(),
  SelectFields :: [field()],
  Conditions   :: condition(),
  ExtraWhere   :: string(),
  Res          :: {iolist(), [term()]}.
s_count(Table, SelectFields, Conditions, ExtraWhere) ->
  {_Select, Where, Values} = form_select_query(SelectFields, Conditions, ExtraWhere),

  {
    ["SELECT COUNT(1) AS `count` FROM ", escape(Table), " WHERE ", Where],
    Values
  }.

%% @doc Generic select function.
-spec s(Table, SelectFields, Conditions, ExtraWhere, Page, PageSize, OrderBy) -> Res when
  Table        :: tab_name(),
  SelectFields :: [field()],
  Conditions   :: condition(),
  ExtraWhere   :: string(),
  Page         :: non_neg_integer(),
  PageSize     :: non_neg_integer(),
  OrderBy      :: string(),
  Res          :: {iolist(), [term()]}.
s(Table, SelectFields, Conditions, ExtraWhere, Page, PageSize, OrderBy) ->
  Paging =
    case PageSize of
      0 ->
        [];
      _ ->
        [
          " LIMIT ", integer_to_list(Page), ", ",
          integer_to_list(PageSize)
        ]
    end,

  {Select, Where, Values} = form_select_query(SelectFields, Conditions, ExtraWhere),

  case Where of
    [] ->
      {
        [
          "SELECT ", Select,
          " FROM ", escape(Table),
          " ", OrderBy, " ", Paging
        ],
        Values
      };
    _ ->
      {
        [
          "SELECT ", Select,
          " FROM ", escape(Table),
          " WHERE ", Where, " ", OrderBy, " ", Paging
        ],
        Values
      }
  end.

%% @doc INSERT.
-spec i(Table, PropList) -> Res when
  Table    :: tab_name(),
  PropList :: proplists:proplist(),
  Res      :: {iodata(), [term()]}.
i(Table, PropList) ->
  {Fields, Values, Args} =
    lists:foldr(fun({K, V}, {Fs, Vs, Args}) ->
      {[escape(K) | Fs], [V | Vs], ["?" | Args]}
    end, {[], [], []}, PropList),

  {
    [
     "INSERT INTO ", escape(Table), " (", string:join(Fields, ", "), ") ",
     "VALUES (", string:join(Args, ", "), ")"
    ],
    Values
  }.

-spec i_all(SchemaMeta, PropList) -> Res when
  SchemaMeta :: xdb_adapter:schema_meta(),
  PropList   :: proplists:proplist(),
  Res        :: {iodata(), [term()]}.
i_all(#{schema := Schema, source := Table}, PropList) ->
  {Fields, Values, Args} = form_insert_query(Schema, PropList, {[], [], []}),
  {
    [
     "INSERT INTO ", escape(Table), " (", string:join(Fields, ", "), ") VALUES ",
     get_args(Args, "")
    ],
    get_values(Values, [])
  }.

%% @doc UPDATE.
-spec u(Table, UpdateFields, Conditions) -> Res when
  Table        :: tab_name(),
  UpdateFields :: proplists:proplist(),
  Conditions   :: condition(),
  Res          :: {iodata(), [term()], [term()]}.
u(Table, UpdateFields, Conditions) ->
  {_Select, Where, WValues} = form_select_query([], Conditions, ""),

  {UFields, UValues} =
    lists:foldr(fun({K, V}, {Fs, Vs}) ->
      {[escape(K) ++ "=?" | Fs], [V | Vs]}
    end, {[], []}, UpdateFields),

  Update = string:join(UFields, ","),

  case Where of
    [] ->
      {
        ["UPDATE ", escape(Table), " SET ", Update],
         UValues,
         WValues
      };
    _ ->
      {
        ["UPDATE ", escape(Table), " SET ", Update, " WHERE ", Where],
         UValues,
         WValues
      }
  end.

%% @doc DELETE.
-spec d(Table) -> Res when
  Table :: tab_name(),
  Res   :: {iolist(), [term()]}.
d(Table) ->
  {["DELETE FROM ", escape(Table)], []}.

%% @doc DELETE.
-spec d(Table, Conditions) -> Res when
  Table      :: tab_name(),
  Conditions :: condition(),
  Res        :: {iolist(), [term()]}.
d(Table, Conditions) ->
  {_Select, Where, WValues} = form_select_query([], Conditions, ""),
  {["DELETE FROM ", escape(Table), " WHERE ", Where], WValues}.

%%%=============================================================================
%%% Query utilities
%%%=============================================================================

-spec parse_conditions(Expr) -> Res when
  Expr :: xdb_query:conditions(),
  Res  :: {Values :: [any()], xdb_query:conditions()}.
parse_conditions(Expr) ->
  {Values, CleanExprs, _} = parse_conditions(Expr, {[], [], 1}),
  {lists:reverse(Values), lists:reverse(CleanExprs)}.

%% @private
parse_conditions(Exprs, Acc) when is_list(Exprs) ->
  lists:foldl(fun parse_conditions/2, Acc, Exprs);
parse_conditions({Op, Exprs}, {Values, CleanExprs, Count}) when ?logical_op(Op) ->
  {NewValues, NewCleanExprs, NewCount} = parse_conditions(Exprs, {Values, [], Count}),

  {
    NewValues,
    [{Op, lists:reverse(NewCleanExprs)} | CleanExprs],
    NewCount
  };
parse_conditions({Name, 'in', Value}, {Values, CleanExprs, Count}) when is_list(Value) ->
  CountSeq = lists:seq(Count, Count - 1 + length(Value)),
  QMarks = [{'?', C} || C <- CountSeq],
  {
    lists:append(Value, Values),
    [{Name, xdb_query:validate_operator('in'), QMarks} | CleanExprs],
    lists:last(CountSeq) + 1
  };
parse_conditions({Name, Op, Value}, {Values, CleanExprs, Count}) when not is_atom(Value) ->
  {
    [Value | Values],
    [{Name, xdb_query:validate_operator(Op), {'?', Count}} | CleanExprs],
    Count + 1
  };
parse_conditions({Name1, Op, Name2}, {Values, CleanExprs, Count}) when is_atom(Name2) ->
  {
    Values,
    [{Name1, xdb_query:validate_operator(Op), Name2} | CleanExprs],
    Count
  };
parse_conditions({Name, Value}, {Values, CleanExprs, Count}) when ?null_value(Value) ->
  {
    [Value | Values],
    [{Name, {'?', Count}} | CleanExprs],
    Count + 1
  };
parse_conditions({Name, Value}, {Values, CleanExprs, Count}) ->
  {
    Values,
    [{Name, Value} | CleanExprs],
    Count
  };
parse_conditions([], Acc) ->
  Acc;
parse_conditions(Expr, _) ->
  exit({unsupported_expression, Expr}).

-spec where_clause(xdb_query:conditions()) -> iodata().
where_clause(Exprs) ->
  where_clause(Exprs, fun escape/1, fun slot_question/1).

-spec where_clause(xdb_query:conditions(), fun()) -> iodata().
where_clause(Exprs, EscapeFun) ->
  where_clause(Exprs, EscapeFun, fun slot_question/1).

-spec where_clause(xdb_query:conditions(), fun(), fun()) -> iodata().
where_clause([], _EscapeFun, _SlotFun) ->
  [];
where_clause(Exprs, EscapeFun, SlotFun) when is_list(Exprs) ->
  Clauses = [where_clause(Expr, EscapeFun, SlotFun) || Expr <- Exprs],
  ["(", interpose(" AND ", Clauses), ")"];
where_clause({'and', Exprs}, EscapeFun, SlotFun) ->
  where_clause(Exprs, EscapeFun, SlotFun);
where_clause({'or', Exprs}, EscapeFun, SlotFun) ->
  Clauses = [where_clause(Expr, EscapeFun, SlotFun) || Expr <- Exprs],
  ["(", interpose(" OR ", Clauses), ")"];
where_clause({'not', Expr}, EscapeFun, SlotFun) ->
  [" NOT ", "(", where_clause(Expr, EscapeFun, SlotFun), ")"];
where_clause({Name, 'in', Slot}, EscapeFun, SlotFun) when is_list(Slot) ->
  Slots = lists:join(",", [SlotFun(S) || S <- Slot]),
  [EscapeFun(Name), " ", operator_to_string('in'), "(", Slots, ")"];
where_clause({Name, Op, {'?', _} = Slot}, EscapeFun, SlotFun) ->
  [EscapeFun(Name), " ", operator_to_string(Op), SlotFun(Slot)];
where_clause({Name1, Op, Name2}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name1), " ", operator_to_string(Op), " ", escape(Name2)];
where_clause({Name,  {'?', _} = Slot}, EscapeFun, SlotFun) ->
  [EscapeFun(Name), " = ", SlotFun(Slot)];
where_clause({Name, 'null'}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name), " IS NULL "];
where_clause({Name, 'not_null'}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name), " IS NOT NULL "].

-spec order_by_clause([{atom(), xdb_query:sort()}]) -> iolist().
order_by_clause(SortFields) ->
  order_by_clause(SortFields, fun escape/1).

-spec order_by_clause([{atom(), xdb_query:sort()}], fun()) -> iolist().
order_by_clause(SortFields, EscapeFun) ->
  ClauseFun =
    fun({Name, SortOrder}) ->
      [EscapeFun(atom_to_list(Name)), " ", atom_to_list(SortOrder)]
    end,

  Clauses = lists:map(ClauseFun, SortFields),
  [" ORDER BY ", interpose(", ", Clauses)].

-spec escape(field()) -> string().
escape(Field) when is_atom(Field) ->
  escape(atom_to_list(Field));
escape(Field) when is_list(Field) ->
  lists:flatten(["`", Field, "`"]).

-spec slot_question({'?', integer()}) -> string().
slot_question(_) ->
  " ? ".

-spec slot_numbered({'?', integer()}) -> iodata().
slot_numbered({_, N}) ->
  [" $", integer_to_list(N), " "].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
form_select_query(SelectFields, Conditions, ExtraWhere) ->
  {Values, CleanConditions} = parse_conditions([Conditions]),
  WhereTmp = where_clause(CleanConditions),

  SFields =
    case SelectFields of
      [] ->
        ["*"];
      _ ->
        [escape(F) || F <- SelectFields]
    end,

  Where =
    case ExtraWhere of
      [] ->
        WhereTmp;
      ExtraWhere ->
        [WhereTmp, case WhereTmp of [] -> " "; _ -> " AND " end, ExtraWhere]
    end,

  Select = string:join(SFields, ","),
  {Select, Where, Values}.

%% @private
form_insert_query(_Schema, [], {Fields, Values, Args}) ->
  {Fields, Values, Args};
form_insert_query(Schema, [PropList0 | Remaining], {_F0, V0, A0}) ->
  {_PKs, FieldNames} = get_meta(Schema),
  PropList = maps:to_list(maps:merge(maps:from_list([{K , null} || K <- FieldNames]), PropList0)),
  {Fields, Values, Args} =
    lists:foldr(fun({K, V}, {Fs, Vs, Args}) ->
      {[escape(K) | Fs], [V | Vs], ["?" | Args]}
    end, {[], [], []}, PropList),
  form_insert_query(Schema, Remaining, {Fields, [Values] ++ V0, [Args] ++ A0}).

%% @private
interpose(Sep, List) ->
  interpose(Sep, List, []).

%% @private
interpose(_Sep, [], Result) ->
  lists:reverse(Result);
interpose(Sep, [Item | []], Result) ->
  interpose(Sep, [], [Item | Result]);
interpose(Sep, [Item | Rest], Result) ->
  interpose(Sep, Rest, [Sep, Item | Result]).

%% @private
operator_to_string('=<') -> "<=";
operator_to_string('/=') -> "!=";
operator_to_string('==') -> "=";
operator_to_string(Op)   -> atom_to_list(Op).

%% @private
get_args([], Result) ->
  string:slice(Result, 0, string:length(Result) - 2);
get_args([Arg| Remaining], Result) ->
  Result0 = "(" ++ string:join(Arg, ", ") ++ "), ",
  get_args(Remaining, Result ++ Result0).

%% @private
get_values([], Result) ->
  Result;
get_values([Value | Remaining], Result) ->
  get_values(Remaining, Value ++ Result).

%% @private
get_meta(Schema) ->
  SchemaSpec = Schema:schema_spec(),
  PKFieldNames = xdb_schema_spec:pk_field_names(SchemaSpec),
  FieldNames = xdb_schema_spec:field_names(SchemaSpec),
  {PKFieldNames, FieldNames}.
