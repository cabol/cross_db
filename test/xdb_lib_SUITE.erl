-module(xdb_lib_SUITE).

%% Common Test
-export([
  all/0
]).

%% Test Cases
-export([
  t_to_bin/1,
  t_to_atom/1,
  t_to_kvlist/1,
  t_to_map/1,
  t_is_datetime/1,
  t_stringify/1,
  t_keyfetch/1,
  t_keyfind/1,
  t_keymerge/1,
  t_kv_keys_values/1,
  t_reduce_while/1,
  t_raise/1
]).

-define(EXCLUDED_FUNS, [
  module_info,
  all
]).

%%%=============================================================================
%%% CT
%%%=============================================================================

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

-spec t_to_bin(xdb_ct:config()) -> ok.
t_to_bin(_Config) ->
  <<"foo">> = xdb_lib:to_bin(foo),
  <<"foo">> = xdb_lib:to_bin("foo"),
  <<"foo">> = xdb_lib:to_bin(<<"foo">>),
  <<"123">> = xdb_lib:to_bin(123),
  <<"123.123">> = xdb_lib:to_bin(123.123),
  ok.

-spec t_to_atom(xdb_ct:config()) -> ok.
t_to_atom(_Config) ->
  foo = xdb_lib:to_atom(foo),
  foo = xdb_lib:to_atom("foo"),
  foo = xdb_lib:to_atom(<<"foo">>),
  ok.

-spec t_to_kvlist(xdb_ct:config()) -> ok.
t_to_kvlist(_Config) ->
  Map = #{a => 1, {x, y} => {1, 2}, "foo" => "bar"},
  KVList = xdb_lib:to_kvlist(Map),
  Map = xdb_lib:to_map(KVList),
  KVList = xdb_lib:to_kvlist(KVList),
  ok.

-spec t_to_map(xdb_ct:config()) -> ok.
t_to_map(_Config) ->
  Map = #{a => 1, {x, y} => {1, 2}, "foo" => "bar"},
  Map = xdb_lib:to_map(Map),
  Map = xdb_lib:to_map(xdb_lib:to_kvlist(Map)),
  ok.

-spec t_is_datetime(xdb_ct:config()) -> ok.
t_is_datetime(_Config) ->
  true = xdb_lib:is_datetime({2012, 1, 1}),
  true = xdb_lib:is_datetime({{2012, 1, 1}, {0, 0, 0}}),
  true = xdb_lib:is_datetime(calendar:universal_time()),
  false = xdb_lib:is_datetime({2012, 1, 100}),
  false = xdb_lib:is_datetime({{2012, 1, 1}, {100, 0, 0}}),
  false = xdb_lib:is_datetime(20120101),
  ok.

-spec t_stringify(xdb_ct:config()) -> ok.
t_stringify(_Config) ->
  "Hello world 3 times" = xdb_lib:stringify("Hello ~p ~p times", [world, 3]),
  ok.

-spec t_keyfetch(xdb_ct:config()) -> ok.
t_keyfetch(_Config) ->
  bar = xdb_lib:keyfetch(foo, [{foo, bar}, {a, 1}]),
  ok = xdb_ct:assert_error(fun() ->
    xdb_lib:keyfetch(bar, [{foo, bar}, {a, 1}])
  end, {badkey, bar}).

-spec t_keyfind(xdb_ct:config()) -> ok.
t_keyfind(_Config) ->
  bar = xdb_lib:keyfind(foo, [{foo, bar}, {a, 1}]),
  bar = xdb_lib:keyfind(foo, [{foo, bar}, {a, 1}], undefined),
  undefined = xdb_lib:keyfind(bar, [{foo, bar}, {a, 1}], undefined),
  ok.

-spec t_keymerge(xdb_ct:config()) -> ok.
t_keymerge(_Config) ->
  [{foo, bar}, {bar, foo}] = xdb_lib:keymerge([{foo, bar}], [{bar, foo}]),
  ok.

-spec t_kv_keys_values(xdb_ct:config()) -> ok.
t_kv_keys_values(_Config) ->
  [foo, bar] = xdb_lib:kv_keys([{foo, bar}, {bar, foo}]),
  [bar, foo] = xdb_lib:kv_values([{foo, bar}, {bar, foo}]),
  ok.

-spec t_reduce_while(xdb_ct:config()) -> ok.
t_reduce_while(_Config) ->
  Fun =
    fun
      (V, Acc) when V > 3 ->
        {halt, Acc};
      (V, Acc) ->
        {cont, [V | Acc]}
    end,

  R = xdb_lib:reduce_while(Fun, [], lists:seq(1, 4)),
  R = xdb_lib:reduce_while(Fun, [], lists:seq(1, 3)),

  try
    xdb_lib:reduce_while(fun(V, Acc) ->
      a = V ++ Acc
    end, 0, lists:seq(1, 10))
  catch
    _:_ -> ok
  end.

-spec t_raise(xdb_ct:config()) -> ok.
t_raise(_Config) ->
  ok = xdb_ct:assert_error(fun() ->
    xdb_lib:raise(my_error)
  end, my_error),

  ok = xdb_ct:assert_error(fun() ->
    xdb_lib:raise(my_error, "my reason")
  end, {my_error, "my reason"}),

  ok = xdb_ct:assert_error(fun() ->
    xdb_lib:raise(my_error, "missing ~p", [{a, 1}])
  end, {my_error, "missing {a,1}"}).
