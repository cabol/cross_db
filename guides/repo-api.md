# Repo API

Defines a repository.

A repository maps to an underlying data store, controlled by the adapter.
For example, CrossDB ships with a `xdb_mnesia_adapter` that stores data into
Mnesia database.

When used, the repository expects the `otp_app` and `adapter` as options.
The `otp_app` should point to an OTP application that has the repository
configuration, and the `adapter` is a compile-time option that specifies
the adapter itself and should point to an existing and valid module that
implements the `xdb_adapter` behaviour. For example, the repository:

```erlang
-module(blog_repo).

-include_lib("cross_db/include/xdb.hrl").
-repo([{otp_app, blog}, {adapter, xdb_mnesia_adapter}]).
```

## init/1

```erlang
-callback init(Config) -> Res when
  Config :: xdb_lib:keyword(),
  Res    :: {ok, xdb_lib:keyword()} | ignore.
```

A callback executed when the repo starts or when configuration is read.

The argument is the repository configuration as stored in the application
environment. It must return `{ok, Keyword}` with the updated list of
configuration or `ignore`.

## get/2, get/3

```erlang
-callback get(Queryable, Id, Opts) -> Res when
  Queryable :: xdb_query:queryable(),
  Id        :: any(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | undefined | no_return().
```

Fetches a single entity from the data store where the primary key matches the
given id.

Returns `undefined` if no result was found. If the entity in the queryable
has no or more than one primary key, it will raise an argument error.

**Options**

See the "Shared options" section at the module documentation.

**Example**

```erlang
my_repo:get(post, 1).
```

## get_by/2, get_by/3

```erlang
-callback get_by(Queryable, Clauses, Opts) -> Res when
  Queryable :: xdb_query:queryable(),
  Clauses   :: xdb_lib:keyword(),
  Opts      :: xdb_lib:keyword(),
  Res       :: xdb_schema:t() | undefined | no_return().
```

Fetches a single result from the query.

Returns `undefined` if no result was found. Raises if more than one entry.

**Options**

See the "Shared options" section at the module documentation.

**Example**

```erlang
my_repo:get_by(post, [{blog_id, 1}]).
```

## all/1, all/2

```erlang
-callback all(Queryable, Opts) -> Res when
  Queryable :: xdb_query:t() | xdb_query:queryable(),
  Opts      :: xdb_lib:keyword(),
  Res       :: [xdb_schema:t()] | no_return().
```

Fetches all entries from the data store matching the given query.

**Options**

See the "Shared options" section at the module documentation.

**Example**

```erlang
Query = xdb_query:from(post, [{where, [{text, "hello"}]}]).

my_repo:all(Query).
```

## insert/1, insert/2

```erlang
-callback insert(Schema, Opts) -> Res when
  Schema :: xdb_schema:t(),
  Opts   :: xdb_lib:keyword(),
  Res    :: w_respose().
```

Inserts a schema defined via `xdb_schema` or a changeset.

In case a schema is given, the schema is converted into a changeset
with all `undefined` fields as part of the changeset.

In case a changeset is given, the changes in the changeset are
merged with the struct fields, and all of them are sent to the
database.

It returns `{ok, Schema}` if the schema has been successfully
inserted or `{error, Changeset}` if there was a validation
or a known constraint error.

**Options**

- `on_conflict` -  It may be one of `raise` (the default), `nothing`,
- `replace_all` and `{replace, Fields}`. See the "Upserts" section
  for more information.
- `conflict_target` -  Which columns to verify for conflicts. If none
  is specified, the conflict target is left up to the database and is
  usually made of primary keys and/or unique/exclusion constraints.

See the "Shared options" section at the module documentation.

**Example**

A typical example is calling `my_repo:insert/1` with a schema
and acting on the return value:

```erlang
Post = post:schema(#{id => 1, blog_id => 1, text => "my first post"}),
case my_repo:insert(Post) of
  {ok, IntertedPost} -> % Inserted with success
  {error, Changeset} -> % Something went wrong
end,
```

**Upserts**

`insert/2` provides upserts via the `on_conflict` option.
The `on_conflict` option supports the following values:

- `raise` - raises if there is a conflicting primary key or unique index
- `nothing` - ignores the error in case of conflicts
- `replace_all` - replace all values on the existing row with the values
in the schema/changeset
- `{replace, Fields}` - replace only specific columns. This option requires
  `conflict_target`

As an example, imagine `title` is marked as a unique column in the database:

```erlang
Post = post:schema(#{id => 1, blog_id => 1, title => "this is unique"}),
{ok, Inserted} = my_repo:insert(Post),
```

Now we can insert with the same title but do nothing on conflicts:

```erlang
{ok, Updated} = my_repo:insert(Post, [{on_conflict, nothing}]),
```

Because we used `{on_conflict, nothing}`, instead of getting an error,
we got `{ok, Updated}`. However the returned entity does not reflect
the data in the database.

Let's insert a post with the same title but using `{replace, Fields}`
in case of conflicts:

```erlang
{ok, Updated} = my_repo:insert(Post, [{on_conflict, {replace, [body]}}]),
```

## insert_all/2, insert_all/3

```erlang
-callback insert_all(SchemaMod, Entries, Opts) -> Res when
  SchemaMod :: module(),
  Entries   :: [xdb_schema:fields()],
  Opts      :: xdb_lib:keyword(),
  Count     :: integer(),
  Returning :: [xdb_schema:fields()] | undefined,
  Res       :: {Count, Returning} | no_return().
```

Inserts all entries into the repository.

It returns a tuple containing the number of entries and any returned
result as second element.

**Options**

> See `xdb:repo:insert/2`.

See the "Shared options" section at the module documentation.

**Example**

```erlang
Posts = [
  #{id => 1, blog_id => 1, title => "Post1"},
  #{id => 2, blog_id => 2, title => "Post1"}
],

{2, [_, _]} = my_repo::insert_all(post, Posts),
```

**Upserts**

> See `xdb:repo:insert/2`.

## delete/, delete/2

```erlang
-callback delete(Data, Opts) -> Res when
  Data :: xdb_schema:t() | xdb_changeset:t(),
  Opts :: xdb_lib:keyword(),
  Res  :: w_respose().
```

Deletes a struct using its primary key.

If the schema has no primary key, `no_primary_key_field_error` exception
will be raised.

It returns `{ok, Schema}` if the schema has been successfully
deleted or `{error, Changeset}` if there was a validation
or a known constraint error.

**Options**

See the "Shared options" section at the module documentation.

**Example**

```erlang
Post = my_repo:get(post, 1),
case my_repo:delete(Post) of
  {ok, DeletedPost}  -> % Deleted with success
  {error, Changeset} -> % Something went wrong
end,
```

## delete_all/1, delete_all/2

```erlang
-callback delete_all(Queryable, Opts) -> Res when
  Queryable :: xdb_query:t() | xdb_query:queryable(),
  Opts      :: xdb_lib:keyword(),
  Res       :: {integer(), [any()] | undefined} | no_return().
```

Deletes all entries matching the given query.

It returns a tuple containing the number of entries and any returned
result as second element. The second element is `undefined` by default
unless a `select` is supplied in the delete query. Note, however,
not all databases support returning data from DELETEs.

**Options**

See the "Shared options" section at the module documentation.

**Example**

```erlang
my_repo:delete_all(post),

Query1 = xdb_query:from(post, [{where, [{title => "my first post"}]}]),
my_repo:delete_all(Query1),
```

## update/1, update/2

```erlang
-callback update(Changeset, Opts) -> Res when
  Changeset :: xdb_changeset:t(),
  Opts      :: xdb_lib:keyword(),
  Res       :: w_respose().
```

Updates a changeset using its primary key.

A changeset is required as it is the only mechanism for
tracking changes. Only the fields present in the `changes` part
of the changeset are sent to the database. Any other, in-memory
changes done to the schema are ignored.

If the schema has no primary key, `no_primary_key_field_error` exception
will be raised.

It returns `{ok, Schema}` if the schema has been successfully
updated or `{error, Changeset}` if there was a validation
or a known constraint error.

**Options**

See the "Shared options" section at the module documentation.

**Example**

```erlang
Post = my_repo:get(post, 1),
Changeset = xdb_changeset:change(Post, #{title => <<"New Title">>}),
case my_repo:update(Changeset) of
  {ok, UpdatedPost}  -> % Updated with success
  {error, Changeset} -> % Something went wrong
end,
```

## update_all/2, update_all/3

```erlang
-callback update_all(Queryable, Updates, Opts) -> Res when
  Queryable :: xdb_query:t() | xdb_query:queryable(),
  Updates   :: xdb_lib:keyword(),
  Opts      :: xdb_lib:keyword(),
  Res       :: {integer(), [any()] | undefined} | no_return().
```

Updates all entries matching the given query with the given values.

It returns a tuple containing the number of entries and any returned
result as second element. The second element is `undefined` by default
unless a `select` is supplied in the update query. Note, however,
not all databases support returning data from UPDATEs.

**Options**

See the "Shared options" section at the module documentation.

**Example**

```erlang
my_repo:update_all(post, [{title, <<"New title">>}]),

Query1 = xdb_query:from(post, [{where, [{title => "my first post"}]}]),
my_repo:update_all(Query1, [{title, <<"New title">>}]),
```

## in_transaction/0

```erlang
-callback in_transaction() -> boolean().
```

Returns `true` if the current process is inside a transaction.

**Example**

```erlang
my_repo:transaction(fun() ->
  my_repo:in_transaction(), %=> true
  %% your code
end).

my_repo:in_transaction(). %=> false
```

## rollback/1

```erlang
-callback rollback(Value :: any()) -> no_return().
```

Rolls back the current transaction.

The transaction will return the value given as `{error, Value}`.

## transaction/1, transaction/2

```erlang
-callback transaction(Fun, Opts) -> Res when
  Fun  :: fun(() -> any()),
  Opts :: xdb_lib:keyword(),
  Res  :: {ok, any()} | {error, any()}.
```

Runs the given function inside a transaction.

If an unhandled error occurs the transaction will be rolled back
and the error will bubble up from the transaction function.
If no error occurred the transaction will be committed when the
function returns. A transaction can be explicitly rolled back
by calling `my_repo:rollback/1`, this will immediately leave the
function and return the value given to `rollback` as `{error, Value}`.

A successful transaction returns the value returned by the function
 wrapped in a tuple as `{ok, FunResult}`.

**Options**

See the "Shared options" section at the module documentation.

**Example**

```erlang
-import(xdb_changeset, [change/2]).

my_repo:transaction(fun() ->
  my_repo:update(change(Alice, #{balance => account:balance(Alice) - 100})),
  my_repo:update(change(Bob, #{balance => account:balance(Bob) + 100}))
end),

% Roll back a transaction explicitly
my_repo:transaction(fun() ->
  case my_repo:insert(post:schema(#{id => 10, title => "Hello"})) of
    {ok, Post} -> Post;
    _          -> my_repo:rollback(some_custom_error)
  end
end),
```
