%%%-------------------------------------------------------------------
%%% @doc
%%% Pool worker.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_pool_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([
  start_link/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Opts) -> Res when
  Opts :: xdb_lib:kv(),
  Res  :: {ok, pid()} | {error, any()}.
start_link(Opts) when is_list(Opts); is_map(Opts) ->
  gen_server:start_link(?MODULE, xdb_lib:to_map(Opts), []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init(#{adapter := Adapter} = Opts) ->
  _ = process_flag(trap_exit, true),

  InitResult =
    case erlang:function_exported(Adapter, init, 1) of
      true  -> Adapter:init(Opts);
      false -> {ok, undefined}
    end,

  handle_init_result(InitResult, #{adapter => Adapter}).

%% @hidden
handle_call({delete, Schema, Opts}, _From, #{adapter := Mod, rstate := RState} = State) ->
  handle_reply(Mod:delete(Schema, Opts, RState), State);
handle_call({get, Queryable, Id, Opts}, _From, #{adapter := Mod, rstate := RState} = State) ->
  handle_reply(Mod:get(Queryable, Id, Opts, RState), State);
handle_call({insert, Schema, Opts}, _From, #{adapter := Mod, rstate := RState} = State) ->
  handle_reply(Mod:insert(Schema, Opts, RState), State);
handle_call({update, Schema, Opts}, _From, #{adapter := Mod, rstate := RState} = State) ->
  handle_reply(Mod:update(Schema, Opts, RState), State);
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @hidden
handle_cast(_Request, State) ->
  {noreply, State}.

%% @hidden
handle_info(Info, #{repo := Repo, rstate := RepoState} = State) ->
  case erlang:function_exported(Repo, handle_info, 2) of
    true ->
      try
        Result = Repo:handle_info(Info, RepoState),
        handle_info_result(Result, State)
      catch
        throw:Return -> handle_info_result(Return, State)
      end;
    false ->
      {noreply, State}
  end.

%% @hidden
terminate(Reason, #{adapter := Mod, rstate := RepoState}) ->
  case erlang:function_exported(Mod, terminate, 2) of
    true ->
      try
        Mod:terminate(Reason, RepoState)
      catch
        throw:Return -> Return
      end;
    false ->
      ok
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
handle_init_result({ok, RepoState}, State) ->
  {ok, State#{rstate => RepoState}};
handle_init_result({ok, RepoState, hibernate}, State) ->
  {ok, State#{rstate => RepoState}, hibernate};
handle_init_result({ok, RepoState, Timeout}, State) ->
  {ok, State#{rstate => RepoState}, Timeout};
handle_init_result(ignore, State) ->
  {ok, State};
handle_init_result({stop, Reason}, State) ->
  {stop, Reason, State}.

%% @private
handle_info_result({noreply, RepoState}, State) ->
  {noreply, State#{rstate => RepoState}};
handle_info_result({noreply, RepoState, hibernate}, State) ->
  {noreply, State#{rstate => RepoState}, hibernate};
handle_info_result({noreply, RepoState, Timeout}, State) ->
  {noreply, State#{rstate => RepoState}, Timeout};
handle_info_result({stop, Reason, RepoState}, State) ->
  {stop, Reason, State#{rstate => RepoState}}.

%% @private
handle_reply({reply, OkOrError, RepoState}, State) ->
  {reply, OkOrError, State#{rstate => RepoState}}.
