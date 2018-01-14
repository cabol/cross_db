%%%-------------------------------------------------------------------
%%% @doc
%%% Query utilities.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_query).

%% API
-export([
  new/1,
  new/2,
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

%% Query body
-type q_body() :: conditions() | {raw, any()}.

%% Sort order
-type sort() :: asc | desc.

%% Query definition
-type t() :: #{
  schema := schema_mod(),
  source := atom(),
  q_body := q_body()
}.

-export_type([
  queryable/0,
  conditions/0,
  q_body/0,
  sort/0,
  t/0
]).

%% Valid operators
-define(OPERATORS, ['<', '>', '==', '=<', '>=', '/=', 'like']).

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv new(Schema, [])
new(Schema) ->
  new(Schema, []).

-spec new(schema_mod(), q_body()) -> t().
new(Schema, QBody) when is_atom(Schema) ->
  #{
    schema => Schema,
    source => xdb_schema_spec:name(Schema:schema_spec()),
    q_body => validate_query(QBody)
  }.

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
pk_filter(PKs, Conditions) ->
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
validate_query(Q) when is_list(Q) ->
  Q;
validate_query({raw, _} = Q) ->
  Q.
