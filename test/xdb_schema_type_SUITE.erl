-module(xdb_schema_type_SUITE).

%% Common Test
-export([
  all/0
]).

%% Test Cases
-export([
  t_cast_string/1,
  t_cast_float/1,
  t_cast_integer/1,
  t_cast_boolean/1,
  t_cast_date/1,
  t_cast_datetime/1,
  t_cast_binary/1,
  t_cast_custom/1,
  t_cast_field_name/1
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

-spec t_cast_string(xdb_ct:config()) -> ok.
t_cast_string(_Config) ->
  {ok, undefined} = xdb_schema_type:cast(string, undefined),
  {ok, <<"binary">>} = xdb_schema_type:cast(string, <<"binary">>),
  {ok, <<"list">>} = xdb_schema_type:cast(string, "list"),
  {ok, <<"atom">>} = xdb_schema_type:cast(string, atom),
  {ok, <<"1">>} = xdb_schema_type:cast(string, 1),
  {ok, <<"3.14", _Rest/binary>>} = xdb_schema_type:cast(string, 3.14),

  _ = t_common_invalid_types(string),
  ok.

-spec t_cast_float(xdb_ct:config()) -> ok.
t_cast_float(_Config) ->
  {ok, undefined} = xdb_schema_type:cast(float, undefined),
  {ok, 3.14} = xdb_schema_type:cast(float, <<"3.14">>),
  {ok, 3.14} = xdb_schema_type:cast(float, "3.14"),
  {ok, 3.0} = xdb_schema_type:cast(float, "3"),
  {ok, 3.0} = xdb_schema_type:cast(float, 3),
  {ok, 3.14} = xdb_schema_type:cast(float, 3.14),

  {error, {invalid, _}} = xdb_schema_type:cast(float, "no_float"),
  {error, {invalid, _}} = xdb_schema_type:cast(float, '3.14'),
  _ = t_common_invalid_types(string),
  ok.

-spec t_cast_integer(xdb_ct:config()) -> ok.
t_cast_integer(_Config) ->
  {ok, undefined} = xdb_schema_type:cast(integer, undefined),
  {ok, 3} = xdb_schema_type:cast(integer, <<"3">>),
  {ok, 3} = xdb_schema_type:cast(integer, "3"),
  {ok, 3} = xdb_schema_type:cast(integer, 3),
  {ok, 3} = xdb_schema_type:cast(integer, 3.14),

  {error, {invalid, _}} = xdb_schema_type:cast(integer, "no_integer"),
  {error, {invalid, _}} = xdb_schema_type:cast(integer, '3.14'),
  _ = t_common_invalid_types(integer),
  ok.

-spec t_cast_boolean(xdb_ct:config()) -> ok.
t_cast_boolean(_Config) ->
  {ok, undefined} = xdb_schema_type:cast(boolean, undefined),
  {ok, true} = xdb_schema_type:cast(boolean, true),
  {ok, false} = xdb_schema_type:cast(boolean, false),
  {ok, true} = xdb_schema_type:cast(boolean, "true"),
  {ok, false} = xdb_schema_type:cast(boolean, "false"),
  {ok, true} = xdb_schema_type:cast(boolean, <<"true">>),
  {ok, false} = xdb_schema_type:cast(boolean, <<"false">>),
  {ok, true} = xdb_schema_type:cast(boolean, "1"),
  {ok, false} = xdb_schema_type:cast(boolean, "0"),
  {ok, true} = xdb_schema_type:cast(boolean, <<"1">>),
  {ok, false} = xdb_schema_type:cast(boolean, <<"0">>),

  {error, {invalid, _}} = xdb_schema_type:cast(boolean, <<"invalid">>),
  {error, {invalid, _}} = xdb_schema_type:cast(boolean, atom),
  {error, {invalid, _}} = xdb_schema_type:cast(boolean, 1),
  {error, {invalid, _}} = xdb_schema_type:cast(boolean, 3.14),
  _ = t_common_invalid_types(boolean),
  ok.

-spec t_cast_date(xdb_ct:config()) -> ok.
t_cast_date(_Config) ->
  {ok, undefined} = xdb_schema_type:cast(datetime, undefined),
  {ok, {2012, 2, 16}} = xdb_schema_type:cast(date, {2012, 2, 16}),
  {ok, {{2012, 2, 16}, {0, 0, 0}}} = xdb_schema_type:cast(date, <<"2012-02-16">>),
  {ok, {{2012, 2, 16}, {0, 0, 0}}} = xdb_schema_type:cast(date, "2012-02-16"),

  {error, {invalid, _}} = xdb_schema_type:cast(date, <<"wrong_format">>),
  {error, {invalid, _}} = xdb_schema_type:cast(date, "wrong_format"),
  {error, {invalid, _}} = xdb_schema_type:cast(date, atom),
  {error, {invalid, _}} = xdb_schema_type:cast(date, 1),
  {error, {invalid, _}} = xdb_schema_type:cast(date, 3.14),
  _ = t_common_invalid_types(integer),
  ok.

-spec t_cast_datetime(xdb_ct:config()) -> ok.
t_cast_datetime(_Config) ->
  {ok, undefined} = xdb_schema_type:cast(datetime, undefined),
  {ok, {{2012, 2, 16}, {1, 6, 48}}} = xdb_schema_type:cast(datetime, {{2012, 2, 16}, {1, 6, 48}}),
  {ok, {{2012, 2, 16}, {1, 6, 48}}} = xdb_schema_type:cast(datetime, <<"2012-02-16T01:06:48Z">>),
  {ok, {{2012, 2, 16}, {1, 6, 48}}} = xdb_schema_type:cast(datetime, "2012-02-16T01:06:48Z"),

  {error, {invalid, _}} = xdb_schema_type:cast(datetime, <<"wrong_format">>),
  {error, {invalid, _}} = xdb_schema_type:cast(datetime, "wrong_format"),
  {error, {invalid, _}} = xdb_schema_type:cast(datetime, atom),
  {error, {invalid, _}} = xdb_schema_type:cast(datetime, 1),
  {error, {invalid, _}} = xdb_schema_type:cast(datetime, 3.14),
  _ = t_common_invalid_types(integer),
  ok.

-spec t_cast_binary(xdb_ct:config()) -> ok.
t_cast_binary(_Config) ->
  {ok, undefined} = xdb_schema_type:cast(binary, undefined),
  {ok, <<100, 101, 102>>} = xdb_schema_type:cast(binary, <<100, 101, 102>>),
  {ok, <<"binary">>} = xdb_schema_type:cast(binary, <<"binary">>),

  {error, {invalid, _}} = xdb_schema_type:cast(binary, "list"),
  {error, {invalid, _}} = xdb_schema_type:cast(binary, atom),
  {error, {invalid, _}} = xdb_schema_type:cast(binary, 1),
  {error, {invalid, _}} = xdb_schema_type:cast(binary, 3.14),
  _ = t_common_invalid_types(string),
  ok.

-spec t_cast_custom(xdb_ct:config()) -> ok.
t_cast_custom(_Config) ->
  {ok, undefined} = xdb_schema_type:cast(custom, undefined),
  {ok, <<100, 101, 102>>} = xdb_schema_type:cast(custom, <<100, 101, 102>>),
  {ok, <<"binary">>} = xdb_schema_type:cast(custom, <<"binary">>),
  {ok, "list"} = xdb_schema_type:cast(custom, "list"),
  {ok, atom} = xdb_schema_type:cast(custom, atom),
  {ok, 1} = xdb_schema_type:cast(custom, 1),
  {ok, 3.14} = xdb_schema_type:cast(custom, 3.14),
  {ok, _} = xdb_schema_type:cast(custom, self()),
  {ok, {tuple, 1, <<"a">>}} = xdb_schema_type:cast(custom, {tuple, 1, <<"a">>}),
  {ok, #{a := 1}} = xdb_schema_type:cast(custom, #{a => 1}),
  {ok, [1, "2", {3, 4}]} = xdb_schema_type:cast(custom, [1, "2", {3, 4}]),
  ok.

-spec t_cast_field_name(xdb_ct:config()) -> ok.
t_cast_field_name(_Config) ->
  foo = xdb_schema_type:cast_field_name(foo),
  foo = xdb_schema_type:cast_field_name(<<"foo">>),
  ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private
t_common_invalid_types(Type) ->
  {error, {invalid, _}} = xdb_schema_type:cast(Type, self()),
  {error, {invalid, _}} = xdb_schema_type:cast(Type, {tuple, 1, <<"a">>}),
  {error, {invalid, _}} = xdb_schema_type:cast(Type, #{a => 1}),
  {error, {invalid, _}} = xdb_schema_type:cast(Type, [1, "2", {3, 4}]).
