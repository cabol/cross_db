-module(xdb_changeset_SUITE).

%% Common Test
-export([
  all/0
]).

%% Test Cases
-export([
  t_changeset/1,
  t_add_error/1,
  t_cast/1,
  t_change/1,
  t_put_change/1,
  t_get_change/1,
  t_delete_change/1,
  t_apply_changes/1,
  t_validate_change/1,
  t_validate_required/1,
  t_validate_inclusion/1,
  t_validate_number/1,
  t_validate_length/1,
  t_validate_format/1,
  t_nested_changeset_validations/1
]).

-include_lib("common_test/include/ct.hrl").

-import(xdb_ct, [assert_error/2]).

-define(EXCLUDED_FUNS, [
  module_info,
  all
]).

-define(PERMITTED, [
  id,
  first_name,
  last_name,
  age,
  address,
  height,
  description,
  status,
  birthdate
]).

-define(REQUIRED, [
  id,
  first_name,
  last_name,
  age
]).

-define(PERSON, #{
  id         => 1,
  last_name  => <<"other">>,
  age        => 33,
  height     => 1.85,
  birthdate  => <<"1980-09-22">>,
  created_at => {{2012, 2, 16}, {1, 6, 48}},
  is_blocked => false,
  status     => "active"
}).

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

-spec t_changeset(xdb_ct:config()) -> ok.
t_changeset(_Config) ->
  CS = xdb_changeset:change(person:schema(default_person()), #{}),
  person = xdb_changeset:schema(CS),
  #{'__meta__' := #{schema := person}} = xdb_changeset:data(CS),
  undefined = xdb_changeset:params(CS),
  [] = xdb_changeset:errors(CS),
  #{} = xdb_changeset:changes(CS),
  true = xdb_changeset:is_valid(CS),
  #{id := integer} = xdb_changeset:types(CS),
  [] = xdb_changeset:required(CS),
  undefined = xdb_changeset:repo(CS),
  undefined = xdb_changeset:action(CS),
  #{} = xdb_changeset:filters(CS),
  ok.

-spec t_add_error(xdb_ct:config()) -> ok.
t_add_error(_Config) ->
  %% run changeset pipeline adding an error
  CS = [pipe](
    default_person(),
    person:schema(_),
    xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
    xdb_changeset:add_error(_, status, <<"Invalid">>)),

  %% validate errors
  false = xdb_changeset:is_valid(CS),
  1 = length(xdb_changeset:errors(CS)),
  _ = validate_cs_errors(CS, [status]),

  ok.

-spec t_cast(xdb_ct:config()) -> ok.
t_cast(_Config) ->
  %% create a person doc
  Person = default_person(),
  PersonSchema = person:schema(Person),

  %% create params to be cast adding some intentional errors
  Params = ?PERSON#{age => '33', missing => 1},
  Permitted = [missing | ?PERMITTED],

  %% run changeset pipeline
  ExpectedChanges = #{
    birthdate => {{1980, 9, 22}, {0, 0, 0}},
    height    => 1.85,
    id        => 1,
    last_name => <<"other">>,
    status    => <<"active">>
  },
  CS = [pipe](
    PersonSchema,
    xdb_changeset:cast(_, Params, Permitted),
    validate_cs(_, #{
      schema   => person,
      data     => PersonSchema,
      params   => maps:with(Permitted, Params),
      changes  => ExpectedChanges,
      types    => {true, fun(M) -> maps:size(M) > 0 end},
      required => {[], fun(L) -> L end}
    })),

  %% validate errors
  false = xdb_changeset:is_valid(CS),
  1 = length(xdb_changeset:errors(CS)),
  _ = validate_cs_errors(CS, [age]),

  CS1 = [pipe](
    PersonSchema,
    xdb_changeset:cast(_, #{}, ?PERMITTED),
    xdb_changeset:cast(_, #{last_name => <<"other">>}, Permitted)),

  %% validate errors
  true = xdb_changeset:is_valid(CS1),
  0 = length(xdb_changeset:errors(CS1)),

  ok.

-spec t_change(xdb_ct:config()) -> ok.
t_change(_Config) ->
  CS1 = [pipe](
    default_person(),
    person:schema(_),
    xdb_changeset:cast(_, #{}, ?PERMITTED),
    xdb_changeset:change(_, #{last_name => <<"other">>})),
  1 = maps:size(xdb_changeset:changes(CS1)),

  CS2 = [pipe](
    default_person(),
    person:schema(_),
    xdb_changeset:change(_, #{last_name => <<"Darwin">>}),
    xdb_changeset:change(_, #{last_name => <<"other">>})),
  1 = maps:size(xdb_changeset:changes(CS2)),

  CS3 = [pipe](
    default_person(),
    person:schema(_),
    xdb_changeset:change(_, #{last_name => <<"Darwin">>}),
    xdb_changeset:cast(_, #{}, ?PERMITTED)),
  1 = maps:size(xdb_changeset:changes(CS3)),

  ok.

-spec t_put_change(xdb_ct:config()) -> ok.
t_put_change(_Config) ->
  #{last_name := <<"other">>} = [pipe](
    default_person(),
    person:schema(_),
    xdb_changeset:cast(_, #{}, ?PERMITTED),
    xdb_changeset:put_change(_, last_name, <<"other">>),
    xdb_changeset:changes(_)),

  0 = [pipe](
    default_person(),
    person:schema(_),
    xdb_changeset:cast(_, #{}, ?PERMITTED),
    xdb_changeset:put_change(_, last_name, <<"Doe">>),
    xdb_changeset:changes(_),
    maps:size(_)),

  2 = [pipe](
    default_person(),
    person:schema(_),
    xdb_changeset:cast(_, #{}, ?PERMITTED),
    xdb_changeset:put_change(_, first_name, <<"other">>),
    xdb_changeset:put_change(_, last_name, <<"other">>),
    xdb_changeset:put_change(_, weird_field1, undefined),
    xdb_changeset:changes(_),
    maps:size(_)),

  0 = [pipe](
    <<"other">>,
    person:new(_, <<"other">>),
    person:schema(_),
    xdb_changeset:cast(_, #{}, ?PERMITTED),
    xdb_changeset:put_change(_, last_name, <<"other">>),
    xdb_changeset:cast(_, #{last_name => <<"other">>}, ?PERMITTED),
    xdb_changeset:put_change(_, last_name, <<"other">>),
    xdb_changeset:changes(_),
    maps:size(_)),

  ok.

-spec t_get_change(xdb_ct:config()) -> ok.
t_get_change(_Config) ->
  CS1 = [pipe](
    default_person(),
    person:schema(_),
    xdb_changeset:cast(_, #{}, ?PERMITTED),
    xdb_changeset:put_change(_, last_name, <<"other">>)),
  1 = maps:size(xdb_changeset:changes(CS1)),

  <<"other">> = xdb_changeset:get_change(CS1, last_name),
  undefined = xdb_changeset:get_change(CS1, first_name),
  <<"default">> = xdb_changeset:get_change(CS1, first_name, <<"default">>),

  ok.

-spec t_delete_change(xdb_ct:config()) -> ok.
t_delete_change(_Config) ->
  CS1 = [pipe](
    default_person(),
    person:schema(_),
    xdb_changeset:cast(_, #{}, ?PERMITTED),
    xdb_changeset:put_change(_, last_name, <<"other">>)),
  1 = maps:size(xdb_changeset:changes(CS1)),

  CS2 = xdb_changeset:delete_change(CS1, last_name),
  0 = maps:size(xdb_changeset:changes(CS2)),

  ok.

-spec t_apply_changes(xdb_ct:config()) -> ok.
t_apply_changes(_Config) ->
  %% create a person doc
  Person = person:schema(default_person()),

  %% run changeset pipeline
  CS1 = xdb_changeset:cast(Person, ?PERSON#{missing => 1}, ?PERMITTED),
  Data = xdb_changeset:data(CS1),
  true = Data == Person,
  undefined = person:id(Person),
  <<"Doe">> = person:last_name(Person),
  undefined = person:age(Person),

  %% apply changes
  NewPerson = xdb_changeset:apply_changes(CS1),
  false = NewPerson == Person,
  1 = person:id(NewPerson),
  <<"other">> = person:last_name(NewPerson),
  33 = person:age(NewPerson),
  6 = maps:size(xdb_changeset:changes(CS1)),

  %% run changeset pipeline
  CS2 = xdb_changeset:cast(Person, #{}, ?PERMITTED),
  0 = maps:size(xdb_changeset:changes(CS2)),
  Person = xdb_changeset:apply_changes(CS2),

  %% run changeset pipeline
  CS3 = [pipe](
    Person,
    xdb_changeset:cast(_, #{}, ?PERMITTED),
    xdb_changeset:put_change(_, missing, 2)),

  0 = maps:size(xdb_changeset:changes(CS3)),
  Person = xdb_changeset:apply_changes(CS3),

  ok.

-spec t_validate_change(xdb_ct:config()) -> ok.
t_validate_change(_Config) ->
  %% create a person doc
  Person = person:schema(default_person()),

  %% run changeset pipeline
  CS1 = [pipe](
    Person,
    xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
    xdb_changeset:validate_change(_, age, fun(age, Age) ->
      case Age > 30 of
        true  -> [{age, <<"cannot be greater than 30">>}];
        false -> []
      end
    end)),

  %% validate errors
  false = xdb_changeset:is_valid(CS1),
  [{age, {<<"cannot be greater than 30">>, []}}] = xdb_changeset:errors(CS1),

  ok.

-spec t_validate_required(xdb_ct:config()) -> ok.
t_validate_required(_Config) ->
  %% create a person doc
  Person = person:schema(default_person()),

  %% run changeset pipeline
  CS1 = [pipe](
    Person,
    xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
    xdb_changeset:validate_required(_, ?REQUIRED)),

  %% validate errors
  true = xdb_changeset:is_valid(CS1),
  0 = length(xdb_changeset:errors(CS1)),

  %% run changeset pipeline
  CS2 = [pipe](
    Person,
    xdb_changeset:cast(_, ?PERSON#{age => nil}, ?PERMITTED),
    xdb_changeset:validate_required(_, [address | ?REQUIRED])),

  %% validate errors
  false = xdb_changeset:is_valid(CS2),
  2 = length(xdb_changeset:errors(CS2)),
  _ = validate_cs_errors(CS2, [address, age]),

  %% should fails
  assert_error(fun() ->
    [pipe](
      Person,
      xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
      xdb_changeset:validate_required(_, [invalid | ?REQUIRED]))
  end, {badarg, invalid}).

-spec t_validate_inclusion(xdb_ct:config()) -> ok.
t_validate_inclusion(_Config) ->
  %% create a person doc
  Person = person:schema(default_person()),

  %% valid statuses
  Statuses = [<<"active">>, <<"blocked">>],

  %% run changeset pipeline
  CS1 = [pipe](
    Person,
    xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
    xdb_changeset:validate_required(_, ?REQUIRED),
    xdb_changeset:validate_inclusion(_, status, Statuses)),

  %% validate errors
  true = xdb_changeset:is_valid(CS1),
  0 = length(xdb_changeset:errors(CS1)),

  %% run changeset pipeline
  CS2 = [pipe](
    Person,
    xdb_changeset:cast(_, ?PERSON#{status => <<"invalid">>}, ?PERMITTED),
    xdb_changeset:validate_required(_, ?REQUIRED),
    xdb_changeset:validate_inclusion(_, status, Statuses)),

  %% validate errors
  false = xdb_changeset:is_valid(CS2),
  1 = length(xdb_changeset:errors(CS2)),
  _ = validate_cs_errors(CS2, [status]),

  ok.

-spec t_validate_number(xdb_ct:config()) -> ok.
t_validate_number(_Config) ->
  %% create a person doc
  Person = person:schema(default_person()),

  %% run changeset pipeline
  CS1 = [pipe](
    Person,
    xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
    xdb_changeset:validate_number(_, age, [
      {less_than, 34},
      {less_than_or_equal_to, 33},
      {greater_than, 32},
      {greater_than_or_equal_to, 33},
      {equal_to, 33}
    ])),

  %% validate errors
  true = xdb_changeset:is_valid(CS1),
  0 = length(xdb_changeset:errors(CS1)),

  ValidationSet = [
    [{less_than, 30}],
    [{less_than_or_equal_to, 30}],
    [{greater_than, 40}],
    [{greater_than_or_equal_to, 40}],
    [{equal_to, 30}],
    [{less_than, 30}, {equal_to, 30}]
  ],

  ok = lists:foreach(fun(Validations) ->
    %% run changeset pipeline
    CS = [pipe](
      Person,
      xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
      xdb_changeset:validate_number(_, age, Validations)),

    %% validate errors
    false = xdb_changeset:is_valid(CS),
    1 = length(xdb_changeset:errors(CS)),
    _ = validate_cs_errors(CS, [age])
  end, ValidationSet),

  %% should fails
  assert_error(fun() ->
    [pipe](
      Person,
      xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
      xdb_changeset:validate_number(_, age, [{invalid_validation, 33}]))
  end, {badarg, invalid_validation}).

-spec t_validate_length(xdb_ct:config()) -> ok.
t_validate_length(_Config) ->
  %% create a person doc
  Person = person:schema(default_person()),

  %% run changeset pipeline
  CS1 = [pipe](
    Person,
    xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
    xdb_changeset:validate_length(_, last_name, [{is, 5}, {min, 2}, {max, 10}])),

  %% validate errors
  true = xdb_changeset:is_valid(CS1),
  0 = length(xdb_changeset:errors(CS1)),

  ValidationSet = [
    [{is, 3}],
    [{min, 10}],
    [{max, 3}],
    [{is, 5}, {min, 2}, {max, 3}]
  ],
  ok = lists:foreach(fun(Validations) ->
    %% run changeset pipeline
    CS = [pipe](
      Person,
      xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
      xdb_changeset:validate_length(_, last_name, Validations)),

    %% validate errors
    false = xdb_changeset:is_valid(CS),
    [{last_name, {_, [{validation, length}]}}] = xdb_changeset:errors(CS)
  end, ValidationSet),

  ok.

-spec t_validate_format(xdb_ct:config()) -> ok.
t_validate_format(_Config) ->
  %% create a person doc
  Person = person:schema(default_person()),

  %% run changeset pipeline
  CS1 = [pipe](
    Person,
    xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
    xdb_changeset:validate_required(_, ?REQUIRED),
    xdb_changeset:validate_format(_, last_name, <<"^oth">>)),

  %% validate errors
  true = xdb_changeset:is_valid(CS1),
  0 = length(xdb_changeset:errors(CS1)),

  %% run changeset pipeline
  CS2 = [pipe](
    Person,
    xdb_changeset:cast(_, ?PERSON, ?PERMITTED),
    xdb_changeset:validate_required(_, ?REQUIRED),
    xdb_changeset:validate_format(_, last_name, <<"^Doe">>)),

  %% validate errors
  false = xdb_changeset:is_valid(CS2),
  [{last_name, {<<"has invalid format">>, [{validation, format}]}}] = xdb_changeset:errors(CS2),

  ok.

-spec t_nested_changeset_validations(xdb_ct:config()) -> ok.
t_nested_changeset_validations(_Config) ->
  Person = person:new(<<"John">>, <<"Doe">>),
  Params = #{<<"age">> => 33, id => 1, <<"last_name">> => <<"other">>},
  _ = person:changeset(person:schema(Person), Params),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
default_person() ->
  person:new(<<"John">>, <<"Doe">>).

%% @private
validate_cs(CS, ParamsToCheck) ->
  maps:fold(fun
    (K, {Expected, Fun}, Acc) when is_function(Fun) ->
      Expected = Fun(xdb_changeset:K(CS)),
      Acc;
    (K, V, Acc) ->
      V = xdb_changeset:K(CS),
      Acc
  end, CS, ParamsToCheck).

%% @private
validate_cs_errors(CS, ErrorKeys) ->
  Errors = xdb_changeset:errors(CS),
  [true = lists:keymember(K, 1, Errors) || K <- ErrorKeys].
