-module(xdb_test_adapter).

-behaviour(xdb_adapter).

-export([
  insert/4,
  insert_all/4,
  update/5,
  delete/4,
  execute/5
]).

%%%===================================================================
%%% Adapter callbacks
%%%===================================================================

%% @hidden
insert(_Repo, _Meta, #{id := -1}, _Opts) ->
  {error, my_error};
insert(_Repo, _Meta, #{id := -2}, _Opts) ->
  {invalid, [{constraint, invalid_key}]};
insert(_Repo, _Meta, #{id := -11}, _Opts) ->
  {ok, #{id => {id, -11}}};
insert(_Repo, _Meta, Fields, _Opts) ->
  {ok, Fields}.

%% @hidden
insert_all(_Repo,_Meta, List, _Opts) ->
  {length(List), List}.

%% @hidden
update(_Repo, _Meta, #{id := -2}, [{id, -1}], _Opts) ->
  {error, stale};
update(_Repo, _Meta, Fields, _Filters, _Opts) ->
  {ok, Fields}.

%% @hidden
delete(_Repo, _Meta, [{id, -1}], _Opts) ->
  {error, stale};
delete(_Repo, _Meta, _Filters, _Opts) ->
  Values = #{id => 1},
  {ok, Values}.

%% @hidden
execute(_Repo, _Op, _Meta, #{where := [{_, -1}]}, _Opts) ->
  {2, []};
execute(_Repo, _Op, _Meta, #{where := [{_, -11}]}, _Opts) ->
  {2, [person:schema(#{id => 1}), person:schema(#{id => 2})]};
execute(_Repo, _Op, _Meta, #{where := [{PK, Id}]}, _Opts) ->
  {1, [person:schema(#{PK => Id})]};
execute(_Repo, _Op, _Meta, _Query, _Opts) ->
  {1, [person:schema(#{id => 1})]}.
