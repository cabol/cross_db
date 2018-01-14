%%%-------------------------------------------------------------------
%%% @doc
%%% Main Application.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb).

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @hidden
start(_StartType, _StartArgs) ->
  xdb_sup:start_link().

%% @hidden
stop(_State) ->
  ok.
