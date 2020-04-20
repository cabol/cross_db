%%%-------------------------------------------------------------------
%%% @doc
%%% Mnesia bootable repo.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_mnesia_boot_repo).

%% Repo callbacks
-export([init/1]).

%%%===================================================================
%%% Repo callbacks
%%%===================================================================

%% @hidden
init(Opts) ->
  ok = init_mnesia(Opts),
  {ok, Opts}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
init_mnesia(Opts) ->
  %% stopped = mnesia:stop(),
  ok = mnesia_create_schema(node()),
  ok = mnesia:start(),
  ok = create_schemas(Opts),
  ok.

%% @private
mnesia_create_schema(Node) ->
  case mnesia:create_schema([Node]) of
    ok ->
      ok;
    {error, {Node, {already_exists, Node}}} ->
      ok
  end.

%% @private
create_schemas(Opts) ->
  DefaultOpts = parse(Opts),
  Schemas = xdb_lib:keyfind(schemas, Opts, []),
  _ = [ok = create_schema(Schema, DefaultOpts) || Schema <- Schemas],
  ok.

%% @private
create_schema(Schema, Opts) ->
  SchemaSpec = Schema:schema_spec(),
  Name = xdb_schema_spec:name(SchemaSpec),
  Fields = xdb_schema_spec:fields(SchemaSpec),
  Attributes = xdb_schema_spec:field_names(SchemaSpec),
  Indexes = [FN || {FN, _, FOpts} <- Fields, lists:member(index, FOpts)],

  TableOpts = [
    {attributes, Attributes},
    {index, Indexes}
    | Opts
  ],

  case mnesia:create_table(Name, TableOpts) of
    {atomic, ok}                      -> ok;
    {aborted, {already_exists, Name}} -> ok;
    {aborted, Reason}                 -> {error, Reason}
  end.

%% @private
parse(Options) ->
  parse(Options, []).

%% @private
parse([], Acc) ->
  Acc;
parse([{disc_copies, local} | Options], Acc) ->
  parse(Options, [{disc_copies, [node()]} | Acc]);
parse([{disc_copies, Nodes} | Options], Acc) ->
  parse(Options, [{disc_copies, Nodes} | Acc]);
parse([{disc_only_copies, local} | Options], Acc) ->
  parse(Options, [{disc_only_copies, [node()]} | Acc]);
parse([{disc_only_copies, Nodes} | Options], Acc) ->
  parse(Options, [{disc_only_copies, Nodes} | Acc]);
parse([{ram_copies, local} | Options], Acc) ->
  parse(Options, [{ram_copies, [node()]} | Acc]);
parse([{ram_copies, Nodes} | Options], Acc) ->
  parse(Options, [{ram_copies, Nodes} | Acc]);
parse([{majority, Flag} | Options], Acc) ->
  parse(Options, [{majority, Flag} | Acc]);
parse([{snmp, SnmpStruct} | Options], Acc) ->
  parse(Options, [{snmp, SnmpStruct} | Acc]);
parse([{storage_properties, Props} | Options], Acc) ->
  parse(Options, [{storage_properties, Props} | Acc]);
parse([{type, Type} | Options], Acc) when Type =:= set; Type =:= ordered_set ->
  parse(Options, [{type, Type} | Acc]);
parse([_IgnoredOption | Options], Acc) ->
  parse(Options, Acc).
