-module(xdb_query_conditions_test).

%% Common Test
-export([
  init_per_testcase/2,
  end_per_testcase/2
]).

%% Test Cases
-export([
  t_all/1,
  t_or_conditional/1,
  t_and_conditional/1,
  t_not_conditional/1,
  t_not_null_conditional/1,
  t_null_conditional/1,
  t_operators/1,
  t_like_operator/1,
  t_deeply_nested/1
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
  ok = seed(Config),
  Config.

-spec end_per_testcase(atom(), xdb_ct:config()) -> xdb_ct:config().
end_per_testcase(_, Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  ok = xdb_repo_sup:stop(Repo),
  Config.

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec t_all(xdb_ct:config()) -> ok.
t_all(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  #{
    1 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Turing">>},
    2 := #{'__meta__' := _, first_name := <<"Charles">>, last_name := <<"Darwin">>},
    3 := #{'__meta__' := _, first_name := <<"Alan">>, last_name := <<"Poe">>},
    4 := #{'__meta__' := _, first_name := <<"John">>, last_name := <<"Lennon">>}
  } = All = person:list_to_map(Repo:all(person)),
  4 = maps:size(All),

  {2, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>},
    3 := #{first_name := <<"Alan">>, last_name := <<"Poe">>}
  }} = person:all(Repo, [{first_name, <<"Alan">>}]),
  ok.

-spec t_or_conditional(xdb_ct:config()) -> ok.
t_or_conditional(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  Conditions1 = [
    {'or', [
      {first_name, <<"John">>},
      {first_name, <<"Joe">>},
      {first_name, <<"Alan">>}
    ]}
  ],
  {3, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>},
    3 := #{first_name := <<"Alan">>, last_name := <<"Poe">>},
    4 := #{first_name := <<"John">>, last_name := <<"Lennon">>}
  }} = person:all(Repo, Conditions1),

  Conditions2 = [
    {first_name, <<"Alan">>},
    {'or', [
      {last_name, <<"Poe">>},
      {last_name, <<"Lennon">>}
    ]}
  ],
  {1, #{
    3 := #{first_name := <<"Alan">>, last_name := <<"Poe">>}
  }} = person:all(Repo, Conditions2),

  Conditions3 = [
    {'or', [
      {age, '=<', 40},
      {birthdate, '<', {1900, 1, 1}}
    ]}
  ],
  {3, #{
    2 := #{first_name := <<"Charles">>, last_name := <<"Darwin">>},
    3 := #{first_name := <<"Alan">>, last_name := <<"Poe">>},
    4 := #{first_name := <<"John">>, last_name := <<"Lennon">>}
  }} = person:all(Repo, Conditions3),
  ok.

-spec t_and_conditional(xdb_ct:config()) -> ok.
t_and_conditional(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  Conditions1 = [
    {'and', [
      {first_name, <<"John">>},
      {last_name, <<"Lennon">>},
      {age, '>', 50}
    ]}
  ],
  [] = Repo:all(xdb_query:from(person, [{where, Conditions1}])),

  Conditions2 = [
    {'and', [
      {first_name, <<"John">>},
      {last_name, <<"Lennon">>},
      {age, '==', 40}
    ]}
  ],
  {1, #{
    4 := #{first_name := <<"John">>, last_name := <<"Lennon">>}
  }} = person:all(Repo, Conditions2),

  Conditions3 = [
    {'and', [
      {first_name, <<"Alan">>},
      {'or', [
        {last_name, <<"Poe">>},
        {last_name, <<"Turing">>}
      ]}
    ]}
  ],
  {2, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>},
    3 := #{first_name := <<"Alan">>, last_name := <<"Poe">>}
  }} = person:all(Repo, Conditions3),

  ok = assert_error(fun() ->
    xdb_query:validate_operator('<>')
  end, {unknown_operator, '<>'}).

-spec t_not_conditional(xdb_ct:config()) -> ok.
t_not_conditional(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  Conditions1 = [
    {'not', {first_name, <<"Alan">>}}
  ],
  {2, #{
    2 := #{first_name := <<"Charles">>, last_name := <<"Darwin">>},
    4 := #{first_name := <<"John">>, last_name := <<"Lennon">>}
  }} = person:all(Repo, Conditions1),

  Conditions2 = [
    {'not', {'or', [
      {age, '=<', 40},
      {birthdate, '<', {1900, 1, 1}}
    ]}}
  ],
  {1, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>}
  }} = person:all(Repo, Conditions2),
  ok.

-spec t_not_null_conditional(xdb_ct:config()) -> ok.
t_not_null_conditional(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  {3, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>},
    2 := #{first_name := <<"Charles">>, last_name := <<"Darwin">>},
    3 := #{first_name := <<"Alan">>, last_name := <<"Poe">>}
  }} = person:all(Repo, [{address, not_null}]),
  ok.

-spec t_null_conditional(xdb_ct:config()) -> ok.
t_null_conditional(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  {1, #{
    4 := #{first_name := <<"John">>, last_name := <<"Lennon">>}
  }} = person:all(Repo, [{address, null}]),
  ok.

-spec t_operators(xdb_ct:config()) -> ok.
t_operators(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  Conditions1 = [
    {'and', [
      {age, 'not_null'},
      {age, '<', 50}
    ]}
  ],
  {3, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>},
    3 := #{first_name := <<"Alan">>, last_name := <<"Poe">>},
    4 := #{first_name := <<"John">>, last_name := <<"Lennon">>}
  }} = person:all(Repo, Conditions1),

  Conditions2 = [
    {'and', [
      {age, 'not_null'},
      {age, '>', 50}
    ]}
  ],
  {1, #{
    2 := #{first_name := <<"Charles">>, last_name := <<"Darwin">>}
  }} = person:all(Repo, Conditions2),

  Conditions3 = [
    {'and', [
      {age, 'not_null'},
      {age, '>', 100}
    ]}
  ],
  [] = Repo:all(xdb_query:from(person, [{where, Conditions3}])),

  Conditions4 = [
    {'and', [
      {age, 'not_null'},
      {age, '>=', 41}
    ]}
  ],
  {2, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>},
    2 := #{first_name := <<"Charles">>, last_name := <<"Darwin">>}
  }} = person:all(Repo, Conditions4),

  Conditions5 = [
    {'and', [
      {age, 'not_null'},
      {age, '=<', 40}
    ]}
  ],
  {2, #{
    3 := #{first_name := <<"Alan">>, last_name := <<"Poe">>},
    4 := #{first_name := <<"John">>, last_name := <<"Lennon">>}
  }} = person:all(Repo, Conditions5),

  Conditions6 = [
    {'and', [
      {age, 'not_null'},
      {age, '==', 41}
    ]}
  ],
  {1, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>}
  }} = person:all(Repo, Conditions6),

  Conditions7 = [
    {'and', [
      {age, 'not_null'},
      {age, '/=', 40}
    ]}
  ],
  {2, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>},
    2 := #{first_name := <<"Charles">>, last_name := <<"Darwin">>}
  }} = person:all(Repo, Conditions7),
  ok.

-spec t_like_operator(xdb_ct:config()) -> ok.
t_like_operator(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  {2, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>},
    3 := #{first_name := <<"Alan">>, last_name := <<"Poe">>}
  }} = person:all(Repo, [{first_name, 'like', <<"A%">>}]),
  ok.

-spec t_deeply_nested(xdb_ct:config()) -> ok.
t_deeply_nested(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  Conditions1 = [
    {address, 'not_null'},
    {'or', [
      {'and', [
        {age, '>=', 40},
        {birthdate, '<', {1900, 1, 1}}
      ]},
      {address, <<"abc">>}
    ]}
  ],
  {3, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>},
    2 := #{first_name := <<"Charles">>, last_name := <<"Darwin">>},
    3 := #{first_name := <<"Alan">>, last_name := <<"Poe">>}
  }} = person:all(Repo, Conditions1),

  Conditions2 = [
    {'or', [
      {'and', [
        {age, '>', 40},
        {'or', [
          {address, <<"abc">>},
          {address, <<"cde">>}
        ]}
      ]},
      {first_name, <<"John">>}
    ]}
  ],
  {2, #{
    1 := #{first_name := <<"Alan">>, last_name := <<"Turing">>},
    4 := #{first_name := <<"John">>, last_name := <<"Lennon">>}
  }} = person:all(Repo, Conditions2),
  ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec seed(xdb_ct:config()) -> ok.
seed(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  {_, _} = Repo:delete_all(person),
  [] = Repo:all(person),

  People = [
    person:schema(#{
      id         => 1,
      first_name => "Alan",
      last_name  => "Turing",
      age        => 41,
      birthdate  => {1912, 6, 23},
      address    => <<"abc">>
    }),
    person:schema(#{
      id => 2,
      first_name => "Charles",
      last_name  => "Darwin",
      age        => 73,
      birthdate  => {1809, 2, 12},
      address    => <<"bcd">>
    }),
    person:schema(#{
      id         => 3,
      first_name => "Alan",
      last_name  => "Poe",
      age        => 40,
      birthdate  => {1809, 1, 19},
      address    => <<"cde">>
    }),
    person:schema(#{
      id         => 4,
      first_name => "John",
      last_name  => "Lennon",
      age        => 40,
      birthdate  => {1940, 10, 9}
    })
  ],

  _ = [{ok, _} = Repo:insert(P) || P <- People],
  ok.
