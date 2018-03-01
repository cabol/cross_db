%%%-------------------------------------------------------------------
%%% @doc
%%% Parse transform to generate schema and repo functions.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_transform).

-export([parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").

%%%===================================================================
%%% Parse Transform
%%%===================================================================

%% @hidden
parse_transform(ASTs, _Options) ->
  try
    Result =
      lists:flatten([begin
        erl_syntax_lib:map(fun(T) ->
          transform(erl_syntax:revert(T))
        end, AST)
      end || AST <- ASTs]),

    maybe_add_funs(Result)
  catch
    _E:R -> erlang:raise(error, R, erlang:get_stacktrace())
  end.

%% @private
transform({attribute, _, module, Module} = Term) ->
  _ = erlang:put(module, Module),
  Term;
transform({attribute, _, schema, {Name, Fields}}) ->
  _ = schema_spec(Name, Fields),
  Type = type_spec(Fields),
  Type ++ [build_export()];
transform({attribute, _, repo, Opts}) ->
  ok = build_repo(Opts),
  [?Q("-behaviour(xdb_repo)."), build_export()];
transform(Term) ->
  Term.

%%%===================================================================
%%% Helpers
%%%===================================================================

%% @private
maybe_add_funs(Result) ->
  case erlang:get(funs) of
    undefined -> Result;
    ExtFuns   -> lists:droplast(Result) ++ ExtFuns ++ [lists:last(Result)]
  end.

%% @private
build_export() ->
  build_export(erlang:erase(exports)).

%% @private
build_export([{FirstFun, FirstArity} | Exports]) ->
  ?Q(lists:foldl(fun({Fun, Arity}, Acc) ->
    Acc ++ text(", ~p/~p", [Fun, Arity])
  end, text("-export([~p/~p", [FirstFun, FirstArity]), Exports) ++ "]).");
build_export(_) ->
  ?Q("-export([]).").

%% @private
add_funs(FunSpecs) ->
  lists:foreach(fun({Name, Arity, Body}) ->
    add_fun(Name, Arity, Body)
  end, FunSpecs).

%% @private
add_fun(Name, Arity, Body) ->
  _ = do_put(exports, {Name, Arity}, erlang:get(exports)),
  do_put(funs, Body, erlang:get(funs)).

%% @private
do_put(Key, Value, undefined) ->
  erlang:put(Key, [Value]);
do_put(Key, Value, CurrentValues) ->
  erlang:put(Key, [Value | CurrentValues]).

%% @private
text(Msg, Args) ->
  lists:flatten(io_lib:format(Msg, Args)).

%% @private
splicing_args(Arity) when Arity >= 0 ->
  do_splicing_args(lists:seq(1, Arity)).

%% @private
do_splicing_args([]) ->
  "";
do_splicing_args([H]) ->
  "A" ++ integer_to_list(H);
do_splicing_args([H | T]) ->
  lists:foldl(fun(N, Acc) ->
    Acc ++ ", A" ++ integer_to_list(N)
  end, "A" ++ integer_to_list(H), T).

%% @private
get_value(Key, KVList) ->
  case lists:keyfind(Key, 1, KVList) of
    {Key, Value} -> Value;
    false        -> error({missing_value, Key})
  end.

%% @private
erlang_type(string) ->
  "binary()";
erlang_type(binary) ->
  "binary()";
erlang_type(integer) ->
  "integer()";
erlang_type(float) ->
  "float()";
erlang_type(boolean) ->
  "boolean()";
erlang_type(date) ->
  "calendar:date()";
erlang_type(datetime) ->
  "calendar:datetime()";
erlang_type(custom) ->
  "any()".

%%%===================================================================
%%% Schema generators
%%%===================================================================

%% @private
schema_spec(Name, [{FName, FType} | Fields]) ->
  schema_spec(Name, [{FName, FType, []} | Fields]);
schema_spec(Name, [{FName, FType, FOpts} | Fields]) ->
  Init =
    text("schema_spec() -> #{name => ~p, fields => [", [Name]) ++
    process_field(FName, FType, FOpts),

  FunBody =
    lists:foldl(fun
      ({FName1, FType1, FOpts1}, Acc) ->
        Acc ++ ", " ++ process_field(FName1, FType1, FOpts1);
      ({FName1, FType1}, Acc) ->
        Acc ++ ", " ++ process_field(FName1, FType1, [])
    end, Init, Fields) ++ "]}.",

  add_funs(lists:flatten([
    {schema_spec, 0, ?Q(FunBody)},
    schema(),
    primary_key()
  ])).

%% @private
type_spec([{FName, FType} | Fields]) ->
  type_spec([{FName, FType, []} | Fields]);
type_spec([{FName, FType, _} | Fields]) ->
  FirstField = text("~p := ", [FName]) ++ erlang_type(FType),
  Init = "-type t() :: #{'__meta__' := xdb_schema:metadata(), " ++ FirstField,

  Type =
    lists:foldl(fun
      ({FName1, FType1, _}, Acc) ->
        Acc ++ text(", ~p := ", [FName1]) ++ erlang_type(FType1);
      ({FName1, FType1}, Acc) ->
        Acc ++ text(", ~p := ", [FName1]) ++ erlang_type(FType1)
    end, Init, Fields) ++ "}.",

  ?Q(Type ++ " -export_type([t/0]).").

%% @private
process_field(Name, Type, Opts) ->
  _ = maybe_getter(Name, lists:keyfind(getter, 1, Opts)),
  _ = maybe_setter(Name, lists:keyfind(setter, 1, Opts)),
  text("{~p, ~p, ~p}", [Name, Type, Opts]).

%% @private
schema() ->
  Module = erlang:get(module),
  New0 = ?Q(text("schema() -> xdb_schema:new(~p, #{}).", [Module])),
  New1 = ?Q(text("schema(Params) -> xdb_schema:new(~p, Params).", [Module])),
  [{schema, 0, New0}, {schema, 1, New1}].

%% @private
primary_key() ->
  Module = erlang:get(module),
  Text = "primary_key() -> xdb_schema_spec:pk_field_names(~p:schema_spec()).",
  Fun = ?Q(text(Text, [Module])),
  {primary_key, 0, Fun}.

%% @private
maybe_getter(_Name, {getter, false}) ->
  skip;
maybe_getter(Name, _) ->
  add_fun(Name, 1, getter(Name)).

%% @private
getter(Name) ->
  ?Q(text("~p(Schema) -> xdb_schema:get_field(Schema, ~p).", [Name, Name])).

%% @private
maybe_setter(_Name, {setter, false}) ->
  skip;
maybe_setter(Name, _) ->
  add_fun(Name, 2, setter(Name)).

%% @private
setter(Name) ->
  ?Q(text("~p(Schema, Value) -> xdb_schema:set_field(Schema, ~p, Value).", [Name, Name])).

%%%===================================================================
%%% Repo generators
%%%===================================================================

%% @private
build_repo(Opts) ->
  Repo = erlang:get(module),
  OtpApp = get_value(otp_app, Opts),
  Adapter = get_value(adapter, Opts),

  RepoFunSpecs = [
    {xdb_repo_schema,    insert,     1},
    {xdb_repo_schema,    insert,     2},
    {xdb_repo_schema,    update,     1},
    {xdb_repo_schema,    update,     2},
    {xdb_repo_schema,    delete,     1},
    {xdb_repo_schema,    delete,     2},
    {xdb_repo_queryable, all,        1},
    {xdb_repo_queryable, all,        2},
    {xdb_repo_queryable, get,        2},
    {xdb_repo_queryable, get,        3},
    {xdb_repo_queryable, get_by,     2},
    {xdb_repo_queryable, get_by,     3},
    {xdb_repo_queryable, delete_all, 1},
    {xdb_repo_queryable, delete_all, 2},
    {xdb_repo_queryable, update_all, 2},
    {xdb_repo_queryable, update_all, 3}
  ],

  RepoFuns = [
    repo_fun_template(Mod, Action, Arity, Repo, Adapter)
    || {Mod, Action, Arity} <- RepoFunSpecs
  ],

  RepoMetaFuns = repo_meta_funs(Repo, OtpApp, Adapter),
  add_funs(RepoMetaFuns ++ RepoFuns).

%% @private
repo_meta_funs(Repo, OtpApp, Adapter) ->
  [
    {adapter,    0, ?Q(text("adapter() -> ~p.", [Adapter]))},
    {start_link, 0, repo_start_link_0()},
    {start_link, 1, repo_start_link_1(Repo, OtpApp, Adapter)},
    {supervisor, 0, repo_sup_spec(Repo)}
  ].

%% @private
repo_start_link_0() ->
  ?Q("start_link() -> start_link([]).").

%% @private
repo_start_link_1(Repo, OtpApp, Adapter) ->
  Msg = "start_link(Opts) -> xdb_repo_sup:start_link(~p, ~p, ~p, Opts).",
  ?Q(text(Msg, [Repo, OtpApp, Adapter])).

%% @private
repo_sup_spec(Repo) ->
  Spec = "supervisor() -> #{id => ~p, start => {~p, start_link, []}, type => supervisor}.",
  ?Q(text(Spec, [Repo, Repo])).

%% @private
repo_fun_template(Mod, Fun, Arity, Repo, Adapter) ->
  FN = atom_to_list(Fun),
  Args = splicing_args(Arity),
  Body = FN ++ "(" ++ Args ++ ") -> ~p:" ++ FN ++ "(~p, ~p, " ++ Args ++ ").",
  {Fun, Arity, ?Q(text(Body, [Mod, Repo, Adapter]))}.
