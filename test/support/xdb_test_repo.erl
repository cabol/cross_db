-module(xdb_test_repo).

-include_lib("cross_db/include/xdb.hrl").
-repo([{otp_app, cross_db}, {adapter, xdb_test_adapter}]).

-export([init/1]).

%% @hidden
init(Opts) ->
  case xdb_lib:keyfind(start, Opts) of
    ignore -> ignore;
    _      -> {ok, Opts}
  end.
