-module(xdb_repo_basic_test).

%% Common Test
-export([
  init_per_testcase/2,
  end_per_testcase/2
]).

%% Test Cases
-export([
  t_insert/1,
  t_insert_or_raise/1,
  t_insert_errors/1,
  t_insert_on_conflict/1,
  t_insert_all/1,
  t_insert_all_on_conflict/1,
  t_update/1,
  t_update_or_raise/1,
  t_delete/1,
  t_delete_or_raise/1,
  t_get/1,
  t_get_or_raise/1,
  t_get_by/1,
  t_get_by_or_raise/1,
  t_all/1,
  t_all_with_pagination/1,
  t_delete_all/1,
  t_delete_all_with_conditions/1,
  t_update_all/1,
  t_update_all_with_conditions/1
]).

%% Helpers
-export([
  seed/1
]).

-import(xdb_ct, [assert_error/2]).

%%%===================================================================
%%% CT
%%%===================================================================

-spec init_per_testcase(atom(), xdb_ct:config()) -> xdb_ct:config().
init_per_testcase(_, Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  {ok, _} = Repo:start_link(),
  {_, _} = Repo:delete_all(person),
  Config.

-spec end_per_testcase(atom(), xdb_ct:config()) -> xdb_ct:config().
end_per_testcase(_, Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  ok = xdb_repo_sup:stop(Repo),
  Config.

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec t_insert(xdb_ct:config()) -> ok.
t_insert(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  {ok, #{id := 1}} = Repo:insert(person:schema(#{id => 1})),
  #{id := 1, first_name := undefined} = Repo:get(person, 1),

  CS = xdb_changeset:change(person:schema(#{id => 2}), #{first_name => <<"Joe">>}),
  {ok, #{id := 2, first_name := <<"Joe">>}} = Repo:insert(CS),

  #{id := 2, first_name := <<"Joe">>} = Repo:get(person, 2),
  ok.

-spec t_insert_or_raise(xdb_ct:config()) -> ok.
t_insert_or_raise(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  #{id := 1} = Repo:insert_or_raise(person:schema(#{id => 1})),
  #{id := 1, first_name := undefined} = Repo:get(person, 1),

  CS = xdb_changeset:change(person:schema(#{id => 2}), #{first_name => <<"Joe">>}),
  #{id := 2, first_name := <<"Joe">>} = Repo:insert_or_raise(CS),
  #{id := 2, first_name := <<"Joe">>} = Repo:get(person, 2),

  ErrChangeset =
    try
      xdb_ct:pipe(CS, [
        {fun xdb_changeset:add_error/3, [first_name, <<"Invalid">>]},
        {fun Repo:insert_or_raise/1, []}
      ])
    catch
      error:{invalid_changeset_error, ErrCS} ->
        ErrCS
    end,

  [{first_name, {<<"Invalid">>, []}}] = xdb_changeset:errors(ErrChangeset),
  ok.

-spec t_insert_errors(xdb_ct:config()) -> ok.
t_insert_errors(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  {error, CS} =
    xdb_ct:pipe(#{id => 1}, [
      {fun person:schema/1, []},
      {fun xdb_changeset:change/2, [#{first_name => <<"Joe">>}]},
      {fun xdb_changeset:add_error/3, [first_name, <<"Invalid">>]},
      {fun Repo:insert/1, []}
    ]),

  ok = assert_error(fun() -> Repo:update(CS) end, badarg).

-spec t_insert_on_conflict(xdb_ct:config()) -> ok.
t_insert_on_conflict(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  ok = lists:foreach(fun(_) ->
    {ok, #{id := 1}} =
      Repo:insert(
        person:schema(#{id => 1, first_name => <<"Alan">>}),
        [{on_conflict, nothing}]
      )
  end, lists:seq(1, 3)),

  #{id := 1, first_name := <<"Alan">>} = Repo:get(person, 1),

  {ok, #{id := 1}} =
    Repo:insert(
      person:schema(#{id => 1, first_name => <<"FakeAlan">>}),
      [{on_conflict, replace_all}]
    ),

  #{id := 1, first_name := <<"FakeAlan">>, last_name := undefined} = Repo:get(person, 1),

  {ok, #{id := 1}} =
    Repo:insert(
      person:schema(#{id => 1, first_name => <<"Alan">>, last_name => <<"Poe">>}),
      [{on_conflict, {replace, [last_name]}}]
    ),

  #{id := 1, first_name := <<"FakeAlan">>, last_name := <<"Poe">>} = Repo:get(person, 1),

  ok = assert_error(fun() ->
    Repo:insert(person:schema(#{id => 1}))
  end, conflict).

-spec t_insert_all(xdb_ct:config()) -> ok.
t_insert_all(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  [] = Repo:all(person),

  People = [
    #{id => 1, first_name => "Alan", last_name => "Turing", age => 41},
    #{id => 2, first_name => "Charles", last_name => "Darwin", age => 73},
    #{id => 3, first_name => "Alan", last_name => "Poe", age => 40}
  ],

  {3, _} = Repo:insert_all(person, People),

  ok = lists:foreach(fun(_) ->
    {3, _} = Repo:insert_all(person, People, [{on_conflict, nothing}])
  end, lists:seq(1, 3)),

  #{
    1 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Turing">>},
    2 := #{'__meta__' := _, first_name := <<"Charles">>, last_name := <<"Darwin">>},
    3 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Poe">>}
  } = All1 = person:list_to_map(Repo:all(person)),
  3 = maps:size(All1),
  ok.

-spec t_insert_all_on_conflict(xdb_ct:config()) -> ok.
t_insert_all_on_conflict(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  ok = seed(Config),

  {1, [#{id := 1}]} =
    Repo:insert_all(
      person,
      [#{id => 1, first_name => <<"FakeAlan">>}],
      [{on_conflict, nothing}]
    ),

  #{id := 1, first_name := <<"Alan">>} = Repo:get(person, 1),

  {1, [#{id := 1}]} =
    Repo:insert_all(
      person,
      [#{id => 1, first_name => <<"FakeAlan">>}],
      [{on_conflict, replace_all}]
    ),

  #{id := 1, first_name := <<"FakeAlan">>, last_name := undefined} = Repo:get(person, 1),

  {1, [#{id := 1}]} =
    Repo:insert_all(
      person,
      [#{id => 1, first_name => <<"Alan">>, last_name => <<"Poe">>}],
      [{on_conflict, {replace, [last_name]}}]
    ),

  #{id := 1, first_name := <<"FakeAlan">>, last_name := <<"Poe">>} = Repo:get(person, 1),

  ok = assert_error(fun() ->
    Repo:insert_all(person, [#{id => 1}])
  end, conflict).

-spec t_update(xdb_ct:config()) -> ok.
t_update(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  ok = seed(Config),
  Person = Repo:get(person, 1),

  {ok, _CS} =
    xdb_ct:pipe(Person, [
      {fun person:changeset/2, [#{first_name => <<"Joe2">>}]},
      {fun Repo:update/1, []}
    ]),

  #{id := 1, first_name := <<"Joe2">>} = Repo:get(person, 1),

  ok = assert_error(fun() ->
    xdb_ct:pipe(#{id => 11}, [
      {fun person:schema/1, []},
      {fun person:changeset/2, [#{first_name => "other", last_name => "other", age => 33}]},
      {fun Repo:update/1, []}
    ])
  end, stale_entry_error).

-spec t_update_or_raise(xdb_ct:config()) -> ok.
t_update_or_raise(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  ok = seed(Config),
  Person = Repo:get(person, 1),

  _ = xdb_ct:pipe(Person, [
    {fun person:changeset/2, [#{first_name => <<"Joe2">>}]},
    {fun Repo:update_or_raise/1, []}
  ]),

  #{id := 1, first_name := <<"Joe2">>} = Repo:get(person, 1),

  ok = assert_error(fun() ->
    xdb_ct:pipe(Person, [
      {fun xdb_changeset:change/2, [#{first_name => <<"Joe">>}]},
      {fun xdb_changeset:add_error/3, [first_name, <<"Invalid">>]},
      {fun Repo:update_or_raise/1, []}
    ])
  end, invalid_changeset_error),

  ok = assert_error(fun() ->
    xdb_ct:pipe(#{id => 11}, [
      {fun person:schema/1, []},
      {fun person:changeset/2, [#{first_name => "other", last_name => "other", age => 33}]},
      {fun Repo:update_or_raise/1, []}
    ])
  end, stale_entry_error).

-spec t_delete(xdb_ct:config()) -> ok.
t_delete(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  undefined = Repo:get(person, 1),
  ok = seed(Config),
  P1 = #{'__meta__' := _, id := 1} = Repo:get(person, 1),

  {ok, #{'__meta__' := _, id := 1}} = Repo:delete(P1),
  undefined = Repo:get(person, 1),

  {ok, #{id := 2}} =
    xdb_ct:pipe(#{id => 2}, [
      {fun person:schema/1, []},
      {fun xdb_changeset:change/2, [#{first_name => <<"Joe">>}]},
      {fun Repo:delete/1, []}
    ]),

  {error, #{}} =
    xdb_ct:pipe(#{id => 3}, [
      {fun person:schema/1, []},
      {fun xdb_changeset:change/2, [#{first_name => <<"Joe">>}]},
      {fun xdb_changeset:add_error/3, [first_name, <<"Invalid">>]},
      {fun Repo:delete/1, []}
    ]),

  ok = assert_error(fun() -> Repo:delete(P1) end, stale_entry_error).

-spec t_delete_or_raise(xdb_ct:config()) -> ok.
t_delete_or_raise(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  undefined = Repo:get(person, 1),
  ok = seed(Config),
  P1 = #{'__meta__' := _, id := 1} = Repo:get(person, 1),

  #{'__meta__' := _, id := 1} = Repo:delete_or_raise(P1),
  undefined = Repo:get(person, 1),

  #{id := 2} =
    xdb_ct:pipe(#{id => 2}, [
      {fun person:schema/1, []},
      {fun xdb_changeset:change/2, [#{first_name => <<"Joe">>}]},
      {fun Repo:delete_or_raise/1, []}
    ]),

  ok = assert_error(fun() ->
    xdb_ct:pipe(#{id => 3}, [
      {fun person:schema/1, []},
      {fun xdb_changeset:change/2, [#{first_name => <<"Joe">>}]},
      {fun xdb_changeset:add_error/3, [first_name, <<"Invalid">>]},
      {fun Repo:delete_or_raise/1, []}
    ])
  end, invalid_changeset_error),

  ok = assert_error(fun() -> Repo:delete_or_raise(P1) end, stale_entry_error).

-spec t_get(xdb_ct:config()) -> ok.
t_get(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  undefined = Repo:get(person, 1),
  ok = seed(Config),

  #{'__meta__' := _, id := 1} = Repo:get(person, 1),
  #{'__meta__' := _, id := 2} = Repo:get(person, 2),
  #{'__meta__' := _, id := 3} = Repo:get(person, 3),
  ok.

-spec t_get_or_raise(xdb_ct:config()) -> ok.
t_get_or_raise(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  ok = seed(Config),

  #{'__meta__' := _, id := 1} = Repo:get_or_raise(person, 1),
  #{'__meta__' := _, id := 2} = Repo:get_or_raise(person, 2),
  #{'__meta__' := _, id := 3} = Repo:get_or_raise(person, 3),

  ok = assert_error(fun() ->
    Repo:get_or_raise(person, 11)
  end, no_results_error).

-spec t_get_by(xdb_ct:config()) -> ok.
t_get_by(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  [] = Repo:all(person),
  ok = seed(Config),

  #{id := 1} = Repo:get_by(person, [{id, 1}]),
  #{id := 2} = Repo:get_by(person, [{id, 2}]),
  #{id := 3} = Repo:get_by(person, [{id, 3}]),
  #{id := 3} = Repo:get_by(person, [{last_name, <<"Poe">>}]),
  #{id := 1} = Repo:get_by(person, [{first_name, <<"Alan">>}, {last_name, <<"Turing">>}]),

  ok = assert_error(fun() ->
    Repo:get_by(person, [{first_name, <<"Alan">>}])
  end, multiple_results_error).

-spec t_get_by_or_raise(xdb_ct:config()) -> ok.
t_get_by_or_raise(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  [] = Repo:all(person),
  ok = seed(Config),

  #{id := 1} = Repo:get_by_or_raise(person, [{id, 1}]),
  #{id := 3} = Repo:get_by_or_raise(person, [{last_name, <<"Poe">>}]),
  #{id := 1} = Repo:get_by_or_raise(person, [{first_name, <<"Alan">>}, {last_name, <<"Turing">>}]),

  ok = assert_error(fun() ->
    Repo:get_by_or_raise(person, [{first_name, <<"Alan">>}])
  end, multiple_results_error),

  ok = assert_error(fun() ->
    Repo:get_by_or_raise(person, [{first_name, <<"Me">>}])
  end, no_results_error).

-spec t_all(xdb_ct:config()) -> ok.
t_all(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  [] = Repo:all(person),
  ok = seed(Config),

  #{
    1 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Turing">>},
    2 := #{'__meta__' := _, first_name := <<"Charles">>, last_name := <<"Darwin">>},
    3 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Poe">>}
  } = All1 = person:list_to_map(Repo:all(person)),
  3 = maps:size(All1),

  Query = xdb_query:from(person, [{select, [id, first_name]}, {where, [{age, '>', 40}]}]),
  #{
    1 := #{first_name := <<"Alan">>, last_name := undefined},
    2 := #{first_name := <<"Charles">>, last_name := undefined}
  } = All2 = person:list_to_map(Repo:all(Query)),
  2 = maps:size(All2),
  ok.

-spec t_all_with_pagination(xdb_ct:config()) -> ok.
t_all_with_pagination(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  ok = seed(Config),

  Expected = person:list_to_map(Repo:all(person)),
  Query = xdb_query:from(person),

  [P1] = Repo:all(Query#{limit => 1, offset => 0}),
  [P2] = Repo:all(Query#{limit => 1, offset => 1}),
  [P3] = Repo:all(Query#{limit => 1, offset => 2}),
  [P2, P3] = Repo:all(Query#{limit => 2, offset => 1}),
  [] = Repo:all(Query#{limit => 1, offset => 3}),
  [] = Repo:all(Query#{limit => 10, offset => 4}),

  Expected = person:list_to_map([P1, P2, P3]),

  Query1 =
    xdb_query:from(person, [
      {where, [{age, '>', 100}]},
      {limit, 10},
      {offset, 0}
    ]),
  [] = Repo:all(Query1),

  Query2 = xdb_query:from(person, [{where, [{age, '>', 40}]}]),
  #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>},
    2 := #{first_name := <<"Charles">>, last_name := <<"Darwin">>}
  } = All1 = person:list_to_map(Repo:all(Query2)),
  2 = maps:size(All1),
  ok.

-spec t_delete_all(xdb_ct:config()) -> ok.
t_delete_all(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  [] = Repo:all(person),
  ok = seed(Config),
  [_, _, _] = Repo:all(person),

  {3, undefined} = Repo:delete_all(person),
  [] = Repo:all(person),
  ok.

-spec t_delete_all_with_conditions(xdb_ct:config()) -> ok.
t_delete_all_with_conditions(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  [] = Repo:all(person),
  ok = seed(Config),
  [_, _, _] = Repo:all(person),

  Query1 =
    xdb_query:from(person, [
      {where, [
        {'and', [
          {first_name, <<"Alan">>},
          {age, '>', 40}
        ]}
      ]}
    ]),
  {1, _} = Repo:delete_all(Query1),

  #{
    2 := #{'__meta__' := _, first_name := <<"Charles">>, last_name := <<"Darwin">>},
    3 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Poe">>}
  } = All2 = person:list_to_map(Repo:all(person)),
  2 = maps:size(All2),
  ok.

-spec t_update_all(xdb_ct:config()) -> ok.
t_update_all(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  [] = Repo:all(person),
  ok = seed(Config),

  #{
    1 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Turing">>},
    2 := #{'__meta__' := _, first_name := <<"Charles">>, last_name := <<"Darwin">>},
    3 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Poe">>}
  } = All1 = person:list_to_map(Repo:all(person)),
  3 = maps:size(All1),

  {3, _} = Repo:update_all(person, [{last_name, <<"Other">>}]),

  #{
    1 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Other">>},
    2 := #{'__meta__' := _, first_name := <<"Charles">>, last_name := <<"Other">>},
    3 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Other">>}
  } = All2 = person:list_to_map(Repo:all(person)),
  3 = maps:size(All2),
  ok.

-spec t_update_all_with_conditions(xdb_ct:config()) -> ok.
t_update_all_with_conditions(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  [] = Repo:all(person),
  ok = seed(Config),

  #{
    1 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Turing">>},
    2 := #{'__meta__' := _, first_name := <<"Charles">>, last_name := <<"Darwin">>},
    3 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Poe">>}
  } = All1 = person:list_to_map(Repo:all(person)),
  3 = maps:size(All1),

  Query1 =
    xdb_query:from(person, [
      {where, [
        {'and', [
          {first_name, <<"Alan">>},
          {age, '>', 40}
        ]}
      ]}
    ]),
  {1, _} = Repo:update_all(Query1, [{last_name, <<"Other">>}]),

  #{
    1 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Other">>},
    2 := #{'__meta__' := _, first_name := <<"Charles">>, last_name := <<"Darwin">>},
    3 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Poe">>}
  } = All2 = person:list_to_map(Repo:all(person)),
  3 = maps:size(All2),
  ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec seed(xdb_ct:config()) -> ok.
seed(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  People = [
    person:schema(#{id => 1, first_name => "Alan", last_name => "Turing", age => 41}),
    person:schema(#{id => 2, first_name => "Charles", last_name => "Darwin", age => 73}),
    person:schema(#{id => 3, first_name => "Alan", last_name => "Poe", age => 40})
  ],

  _ = [_ = Repo:insert_or_raise(P) || P <- People],
  ok.
