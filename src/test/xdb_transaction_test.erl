-module(xdb_transaction_test).

%% Common Test
-export([
  init_per_testcase/2,
  end_per_testcase/2
]).

%% Test Cases
-export([
  t_transaction/1,
  t_transaction_error/1,
  t_in_transaction/1,
  t_rollback/1
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

-spec t_transaction(xdb_ct:config()) -> ok.
t_transaction(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  {ok, [_, _]} =
    Repo:transaction(fun() ->
      {ok, #{id := 1}} =
        Repo:insert(person:schema(#{
          id         => 1,
          first_name => "Alan",
          last_name  => "Turing",
          age        => 41
        })),

      {ok, #{id := 10}} =
        Repo:insert(person:schema(#{
          id         => 10,
          first_name => "Other"
        })),

      Repo:all(person)
    end),

  {ok, [_, _]} =
    Repo:transaction(fun() ->
      {ok, #{id := 2}} =
        Repo:insert(person:schema(#{
          id         => 2,
          first_name => "Alan",
          last_name  => "Poe",
          age        => 40
        })),

      {ok, _CS} =
        xdb_ct:pipe(person, [
          {fun Repo:get/2, [1]},
          {fun person:changeset/2, [#{first_name => <<"John">>, last_name => <<"other">>}]},
          {fun Repo:update/1, []}
        ]),

      {ok, #{id := 10}} = Repo:delete(Repo:get(person, 10)),

      Repo:all(person)
    end),

  #{
    1 := #{first_name := <<"John">>, last_name := <<"other">>},
    2 := #{first_name := <<"Alan">>, last_name := <<"Poe">>}
  } = All1 = person:list_to_map(Repo:all(person)),
  2 = maps:size(All1),
  ok.

-spec t_transaction_error(xdb_ct:config()) -> ok.
t_transaction_error(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  {ok, #{id := 1}} =
    Repo:insert(person:schema(#{
      id         => 1,
      first_name => "Alan",
      last_name  => "Turing",
      age        => 41
    })),

  {error, _, _} =
    Repo:transaction(fun() ->
      {ok, #{id := 2}} =
        Repo:insert(person:schema(#{
          id         => 2,
          first_name => "Alan",
          last_name  => "Poe",
          age        => 40
        })),

      xdb_ct:pipe(person, [
        {fun Repo:get/2, [1]},
        {fun person:changeset/2, [#{last_name => <<"Poe">>}]},
        {fun Repo:update/1, []}
      ])
    end),

  [_] = Repo:all(person),

  {error, _, _} =
    Repo:transaction(fun() ->
      {ok, #{id := 2}} =
        Repo:insert(person:schema(#{
          id         => 2,
          first_name => "Alan",
          last_name  => "Poe",
          age        => 40
        })),

      {ok, _CS} =
        xdb_ct:pipe(person, [
          {fun Repo:get/2, [1]},
          {fun person:changeset/2, [#{first_name => <<"John">>, last_name => <<"other">>}]},
          {fun Repo:update/1, []}
        ]),

      Repo:insert(account:schema(#{id => 1, username => "cabol"}))
    end),

  [_] = Repo:all(person),
  ok.

-spec t_in_transaction(xdb_ct:config()) -> ok.
t_in_transaction(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),
  false = Repo:in_transaction(),

  {ok, []} =
    Repo:transaction(fun() ->
      true = Repo:in_transaction(),
      Repo:all(person)
    end),

  false = Repo:in_transaction(),
  ok.

-spec t_rollback(xdb_ct:config()) -> ok.
t_rollback(Config) ->
  Repo = xdb_lib:keyfetch(repo, Config),

  {error, my_error, _} =
    Repo:transaction(fun() ->
      {ok, #{id := 1}} =
        Repo:insert(person:schema(#{
          id         => 1,
          first_name => "Alan",
          last_name  => "Turing",
          age        => 41
        })),

      Repo:rollback(my_error)
    end),

  [] = Repo:all(person),

  ok = assert_error(fun() ->
    Repo:rollback(my_error)
  end, no_transaction_is_active).
