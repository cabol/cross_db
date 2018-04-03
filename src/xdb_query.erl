%%%-------------------------------------------------------------------
%%% @doc
%%% Query utilities.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_query).

%% API
-export([
  from/1,
  from/2,
  operators/0,
  validate_operator/1,
  pk_filter/2
]).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

%% Queryable
-type schema_mod() :: module().
-type queryable()  :: schema_mod().

%% Query grammar based on conditions
-type field_name()  :: atom().
-type field_value() :: any().
-type operator()    :: '<' | '>' | '==' | '=<' | '>=' | '/=' | 'like'.
-type conditions()  :: [condition()].
-type condition()   :: {'and', [condition()]}
                     | {'or', [condition()]}
                     | {'not', condition()}
                     | {field_name(), field_value()}
                     | {field_name(), operator(), field_value()}
                     | {field_name(), operator(), field_name()}.

%% Sort order
-type sort() :: asc | desc.

%% Join
-type join() :: join
              | inner_join
              | cross_join
              | left_join
              | right_join
              | full_join
              | inner_lateral_join
              | left_lateral_join.

%% Query definition
-type t() :: #{
  source   := atom(),
  from     := schema_mod(),
  select   => [field_name()],
  where    => conditions(),
  limit    => non_neg_integer(),
  offset   => non_neg_integer(),
  updates  => xdb_lib:keyword(),
  raw      => any(),
  order_by => field_name() | [field_name()] | xdb_lib:keyword(),
  group_by => field_name() | [field_name()],
  preload  => xdb_lib:keyword(),
  distinct => field_name() | xdb_lib:keyword(),
  having   => conditions(),
  join()   => xdb_lib:keyword() | xdb_lib:kwentry()
}.

-export_type([
  queryable/0,
  conditions/0,
  sort/0,
  t/0
]).

%% Join options
-define(JOINS, [
  join,
  inner_join,
  cross_join,
  left_join,
  right_join,
  full_join,
  inner_lateral_join,
  left_lateral_join
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv from(Schema, [])
from(Schema) ->
  from(Schema, []).

-spec from(schema_mod(), xdb_lib:keyword()) -> t().
from(Schema, Exprs) when is_atom(Schema), is_list(Exprs) ->
  Query = #{
    from   => Schema,
    source => xdb_schema_spec:name(Schema:schema_spec())
  },
  parse_exprs(Exprs, Query).

-spec operators() -> [operator()].
operators() ->
  ['<', '>', '==', '=<', '>=', '/=', 'like'].

-spec validate_operator(atom()) -> operator() | no_return().
validate_operator(Op) when is_atom(Op) ->
  case lists:member(Op, operators()) of
    true  -> Op;
    false -> error({unknown_operator, Op})
  end.

-spec pk_filter(PKs, Conditions) -> Res when
  PKs        :: [atom()],
  Conditions :: conditions(),
  PkPairs    :: xdb_lib:keyword(),
  Res        :: {boolean(), PkPairs}.
pk_filter([_ | _], []) ->
  {false, []};
pk_filter([_ | _] = PKs, Conditions) ->
  xdb_lib:reduce_while(fun(PK, {Bool, Acc}) ->
    case xdb_lib:keyfind(PK, Conditions) of
      undefined -> {halt, {false, Acc}};
      Value     -> {cont, {Bool, [{PK, Value} | Acc]}}
    end
  end, {true, []}, lists:reverse(PKs)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
parse_exprs([], Acc) ->
  Acc;
parse_exprs([{select, Value} | Exprs], Acc) when is_list(Value) ->
  parse_exprs(Exprs, Acc#{select => Value});
parse_exprs([{where, Value} | Exprs], Acc) when is_list(Value) ->
  parse_exprs(Exprs, Acc#{where => Value});
parse_exprs([{limit, Value} | Exprs], Acc) when is_integer(Value), Value >= 0 ->
  parse_exprs(Exprs, Acc#{limit => Value});
parse_exprs([{offset, Value} | Exprs], Acc) when is_integer(Value), Value >= 0 ->
  parse_exprs(Exprs, Acc#{offset => Value});
parse_exprs([{updates, Value} | Exprs], Acc) when is_list(Value) ->
  parse_exprs(Exprs, Acc#{updates => Value});
parse_exprs([{order_by, Value} | Exprs], Acc) when is_list(Value); is_atom(Value) ->
  parse_exprs(Exprs, Acc#{order_by => Value});
parse_exprs([{group_by, Value} | Exprs], Acc) when is_list(Value); is_atom(Value) ->
  parse_exprs(Exprs, Acc#{group_by => Value});
parse_exprs([{preload, Value} | Exprs], Acc) when is_list(Value) ->
  parse_exprs(Exprs, Acc#{preload => Value});
parse_exprs([{distinct, Value} | Exprs], Acc) when is_list(Value); is_atom(Value) ->
  parse_exprs(Exprs, Acc#{distinct => Value});
parse_exprs([{having, Value} | Exprs], Acc) when is_list(Value) ->
  parse_exprs(Exprs, Acc#{having => Value});
parse_exprs([{raw, Value} | Exprs], Acc) ->
  parse_exprs(Exprs, Acc#{raw => Value});
parse_exprs([{Opt, Value} | Exprs], Acc) ->
  case lists:member(Opt, ?JOINS) of
    true when is_list(Value); is_tuple(Value) ->
      parse_exprs(Exprs, Acc#{Opt => Value});
    false ->
      parse_exprs(Exprs, Acc)
  end.
