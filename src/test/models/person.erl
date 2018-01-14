-module(person).

-export([
  new/2,
  new/3,
  new/4,
  changeset/2,
  all/2,
  list_to_map/1
]).

-include_lib("cross_db/include/xdb.hrl").
-schema({people, [
  {id,            integer,  [primary_key]},
  {first_name,    string},
  {last_name,     string},
  {age,           integer},
  {address,       string},
  {birthdate,     date},
  {created_at,    datetime, [{setter, false}]},
  {height,        float},
  {description,   string},
  {profile_image, binary,   [{getter, false}]},
  {is_blocked,    boolean},
  {status,        string},
  {weird_field1,  custom},
  {weird_field2,  custom},
  {weird_field3,  custom}
]}).

-define(PERMITTED, xdb_schema_spec:field_names(schema_spec())).

%% @equiv new(FirstName, LastName, undefined)
new(FirstName, LastName) ->
  new(FirstName, LastName, undefined).

%% @equiv new(FirstName, LastName, Age, undefined)
new(FirstName, LastName, Age) ->
  new(FirstName, LastName, Age, undefined).

-spec new(binary(), binary(), integer() | undefined, binary() | undefined) -> xdb_schema:fields().
new(FirstName, LastName, Age, Address) ->
  {BirthDate, _} = calendar:universal_time(),
  Datetime = calendar:universal_time(),
  #{
    first_name    => FirstName,
    last_name     => LastName,
    age           => Age,
    address       => Address,
    birthdate     => BirthDate,
    created_at    => Datetime,
    height        => undefined,
    description   => undefined,
    profile_image => undefined,
    is_blocked    => undefined,
    weird_field1  => undefined,
    weird_field2  => undefined,
    weird_field3  => undefined,
    missing       => undefined
  }.

-spec changeset(t(), xdb_schema:fields()) -> xdb_changeset:t().
changeset(Person, Params) ->
  xdb_ct:pipe(Person, [
    {fun xdb_changeset:cast/3, [Params, ?PERMITTED]},
    {fun xdb_changeset:validate_required/2, [[id, first_name, last_name, age]]},
    {fun xdb_changeset:validate_inclusion/3, [status, [<<"active">>, <<"blocked">>]]},
    {fun xdb_changeset:validate_number/3, [age, [{less_than_or_equal_to, 33}]]},
    {fun xdb_changeset:validate_length/3, [last_name, [{min, 3}]]},
    {fun xdb_changeset:validate_format/3, [last_name, <<"^oth">>]}
  ]).

-spec all(xdb_repo:t(), xdb_query:conditions()) -> {integer(), #{any() => t()}} | no_return().
all(Repo, Conditions) ->
  Records = list_to_map(Repo:all(xdb_query:new(person, Conditions))),
  {maps:size(Records), Records}.

-spec list_to_map([t()]) -> #{any() => t()}.
list_to_map(People) ->
  lists:foldl(fun(#{id := Id} = P, Acc) ->
    Acc#{Id => P}
  end, #{}, People).
