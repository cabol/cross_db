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

%% Query definition
-type t() :: #{
  source  := atom(),
  from    := schema_mod(),
  select  => [field_name()],
  where   => conditions(),
  limit   => non_neg_integer(),
  offset  => non_neg_integer(),
  updates => xdb_lib:keyword(),
  raw     => any()
}.

-export_type([
  queryable/0,
  conditions/0,
  sort/0,
  t/0
]).

%% Valid operators
-define(OPERATORS, ['<', '>', '==', '=<', '>=', '/=', 'like']).

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv from(Schema, [])
from(Schema) ->
  from(Schema, []).

-spec from(schema_mod(), xdb_lib:keyword()) -> t().
from(Schema, Exprs) when is_atom(Schema), is_list(Exprs) ->
  Query = #{
    from    => Schema,
    source  => xdb_schema_spec:name(Schema:schema_spec()),
    select  => xdb_lib:keyfind(select, Exprs, []),
    where   => xdb_lib:keyfind(where, Exprs, []),
    limit   => xdb_lib:keyfind(limit, Exprs, 0),
    offset  => xdb_lib:keyfind(offset, Exprs, 0),
    updates => xdb_lib:keyfind(updates, Exprs, [])
  },

  case xdb_lib:keyfind(raw, Exprs) of
    undefined -> Query;
    RawQuery  -> Query#{raw => RawQuery}
  end.

-spec validate_operator(atom()) -> operator() | no_return().
validate_operator(Op) when is_atom(Op) ->
  case lists:member(Op, ?OPERATORS) of
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
