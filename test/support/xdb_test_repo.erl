-module(xdb_test_repo).

-include_lib("cross_db/include/xdb.hrl").
-repo([{otp_app, cross_db}, {adapter, xdb_test_adapter}]).

-define(ETS, ?MODULE).

-export([init/1, prehook/2, posthook/2]).

%% @hidden
init(Opts) ->
  case xdb_lib:keyfind(start, Opts) of
    ignore -> ignore;
    _      -> {ok, Opts}
  end.

%% @hidden
prehook(Action, Data) ->
  ets:info(?ETS) /= undefined andalso
    ets:insert(?ETS, {erlang:timestamp(), {Action, Data}}).

posthook(Action, Data) ->
  ets:info(?ETS) /= undefined andalso
    ets:insert(?ETS, {erlang:timestamp(), {Action, Data}}).
