-module(xdb_query_SUITE).

%% Common Test
-export([
  all/0
]).

%% Test Cases
-export([
  t_from/1,
  t_validate_operator/1,
  t_pk_filter/1
]).

-import(xdb_ct, [assert_error/2]).

-define(EXCLUDED_FUNS, [
  module_info,
  all
]).

%%%===================================================================
%%% CT
%%%===================================================================

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec t_from(xdb_ct:config()) -> ok.
t_from(_Config) ->
  #{
    source   := people,
    from     := person,
    select   := [],
    where    := [],
    limit    := 1,
    offset   := 1,
    updates  := [],
    raw      := [],
    order_by := [],
    group_by := [],
    preload  := [],
    distinct := [],
    having   := [],
    join     := []
  } = full_query(),

  #{
    source := people,
    from   := person
  } = xdb_query:from(person),
  ok.

-spec t_validate_operator(xdb_ct:config()) -> ok.
t_validate_operator(_Config) ->
  ok = lists:foreach(fun(Op) ->
    Op = xdb_query:validate_operator(Op)
  end, xdb_query:operators()),

  ok = assert_error(fun() ->
    xdb_query:validate_operator(invalid)
  end, {unknown_operator, invalid}).

-spec t_pk_filter(xdb_ct:config()) -> ok.
t_pk_filter(_Config) ->
  PKs = person:primary_key(),
  {true, _} = xdb_query:pk_filter(PKs, [{id, 1}]),
  {false, _} = xdb_query:pk_filter(PKs, [{a, 1}]),
  {false, _} = xdb_query:pk_filter(PKs, []),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
full_query() ->
  xdb_query:from(person, [
    {select,   []},
    {where,    []},
    {limit,    1},
    {offset,   1},
    {updates,  []},
    {raw,      []},
    {order_by, []},
    {group_by, []},
    {preload,  []},
    {distinct, []},
    {having,   []},
    {join,     []},
    {invalid,  []}
  ]).
