# Getting Started

This guide is an introduction to [CrossDB](https://github.com/cabol/cross_db),
a simple persistence framework for Erlang. **CrossDB** is highly inspired by
[Ecto](https://github.com/elixir-ecto/ecto) and also
[SumoDB](https://github.com/inaka/sumo_db).

## Adding CrossDB to an application

Let's start creating a new Erlang application by running this command:

```
$ rebar3 new app blog
$ cd blog
```

To add `cross_db` to this application, there are a few steps that we need to
take.

The first step will be adding `cross_db` to our `*.config` file (e.g.:
`config/sys.config`), along with other extra deps used only in this example.
The config file might looks like this:

```erlang
{erl_opts, [
  debug_info,

  %% For fancyflow dependency
  {parse_transform, fancyflow_trans}
]}.

{deps, [
  %% CrossDB dependency
  {cross_db, {git, "https://github.com/cabol/cross_db", {tag, "v0.1.0"}}},

  %% Pipe operator (useful for changesets)
  {fancyflow, {git, "https://github.com/ferd/fancyflow", {ref, "81cf9df"}}},

  %% Inherit the default repo `init/1` callback from `xdb_mnesia_boot_repo`
  {mixer, "1.0.0", {pkg, inaka_mixer}}
]}.

%% Start the app when we run the shell
{shell, [
  {apps, [blog]},
  {config, ["config/sys.config"]}
]}.
```

> **NOTE:** Only the `cross_db` dependency is mandatory, rest of the deps and
  configuration is for purpose of the example.

For the second step we need to setup some configuration for `cross_db` so that
we can perform actions on a repo from within the application's code.

Let's create our `blog_repo` module:

```erlang
-module(blog_repo).

-include_lib("cross_db/include/xdb.hrl").
-repo([{otp_app, blog}]).
```

> The adapter will be set in the config, in this case we are going to use
  the `mnesia` build-in adapter.

Mnesia adapter comes along with a default repo that implements the `init/1`
callback, which creates the tables for us, but only works local (single-node).
So let's edit the `blog_repo` module in order to inherit the `init/1` callback:

```erlang
-module(blog_repo).

-include_lib("cross_db/include/xdb.hrl").
-repo([{otp_app, blog}]).

%% Inherit the default repo `init/1` callback from `xdb_mnesia_boot_repo`
-include_lib("mixer/include/mixer.hrl").
-mixin([xdb_mnesia_boot_repo]).
```

Then let's add some configuration to our repo within the config file
(e.g.: `config/sys.config`):

```erlang
[
  {blog, [
    {blog_repo, [
      {adapter, xdb_mnesia_adapter},
      {ram_copies, local},
      {schemas, []}
    ]}
  ]}
].
```

> **IMPORTANT:** By default, `cross_db` assumes the config file is
  `config/sys.config`, if you want to change the name and location
  you have to add the `{sys_config, "config/sys.config"}` option in
  the `relx` section within the `rebar.config`.

The final piece of configuration is to setup the `blog_repo` as a supervisor
within the application's supervision tree, which we can do in
`src/blog_sup.erl`, inside the `init/1` callback function:

```erlang
init(_Args) ->
  Children = [
    blog_repo:supervisor()
  ],
  {ok, {{one_for_all, 0, 1}, Children}}.
```

Finally we are ready to compile the app and run it:

```
$ rebar3 shell
```

## Creating schemas

First of all, let's create a sub-folder `models` within `src` in order to
organize a bit better the code, and then we can create the first model
`blog`, like so:

```erlang
-module(blog).

-include_lib("cross_db/include/xdb.hrl").
-schema({blogs, [
  {id,         integer,  [primary_key]},
  {name,       string},  % by default the options are []
  {status,     string},
  {created_at, datetime, [{setter, false}]},
  {updated_at, datetime}
]}).
```

Then a second model `post`:

```erlang
-module(post).

-include_lib("cross_db/include/xdb.hrl").
-schema({posts, [
  {id,         integer,  [primary_key]},
  {blog_id,    integer},  % by default the options are []
  {title,      string},
  {body,       string},
  {created_at, datetime, [{setter, false}]},
  {updated_at, datetime}
]}).
```

And before to compile the code we need to edit our `config/sys.config` file in
order to add the created schemas and let `mnesia` adapter create the tables
for us, like so:

```erlang
[
  {blog, [
    {blog_repo, [
      {adapter, xdb_mnesia_adapter},
      {ram_copies, local},
      {schemas, [blog, post]}
    ]}
  ]}
].
```

## Validating changes

In **CrossDB**, you may wish to validate changes before they go to the database.
For instance, you may wish that a post has both a `blog_id` and a `text` before
a record can be entered into the database. For this, **CrossDB** has changesets.

Let’s add a changeset to our `post` module inside `src/models/post.erl` now:

```erlang
-spec changeset(t(), xdb_schema:fields()) -> xdb_changeset:t().
changeset(Post, Params) ->
  [pipe](
    Post,
    post:schema(_),
    xdb_changeset:cast(_, Params, [id, blog_id, text, created_at, updated_at]),
    xdb_changeset:validate_required(_, [id, blog_id, title]),
    xdb_changeset:validate_length(_, title, [{min, 4}, {max, 256}])
  ).
```

> **NOTE:** This example uses [**FancyFlow**](https://github.com/ferd/fancyflow)
  in order to pipe changeset functions in a nicer way.

This changeset takes a `post` and a set of params, which are to be the changes
to apply to this `post`. The `changeset` function first casts the `id`, `blog_id`,
`created_at`, `updated_at` and `text` keys from the parameters passed in to the
changeset. Casting tells the changeset what parameters are allowed to be passed
through in this changeset, and anything not in the list will be ignored.

On the next line, we call `validate_required` which says that, for this
changeset, we expect `id`, `blog_id` and `text` to have values specified.
Let’s use this changeset to attempt to create a new record without a `blog_id`
and `text`:

```erlang
> Post = post:schema(#{}).

> Changeset = post:changeset(Post, #{}).

> blog_repo:insert(Changeset).
```

On the first line here, we get a struct from the `post` module. We know what
that does, because we saw it not too long ago. On the second line we do
something brand new: we define a changeset. This changeset says that on the
specified `post` object, we’re looking to make some changes. In this case,
we’re not looking to change anything at all.

On the final line, rather than inserting the `post`, we insert the `changeset`.
The `changeset` knows about the `post`, the changes and the validation rules
that must be met before the data can be entered into the database. When this
third line runs, we’ll see this:

```erlang
{error,#{action => insert,changes => #{},
         data =>
             #{'__meta__' =>
                   #{schema => post,source => posts,state => built},
               blog_id => undefined,body => undefined,
               created_at => undefined,id => undefined,title => undefined,
               updated_at => undefined},
         errors =>
             [{id,{<<"can't be blank">>,[{validation,required}]}},
              {blog_id,{<<"can't be blank">>,[{validation,required}]}},
              {title,{<<"can't be blank">>,[{validation,required}]}}],
         filters => #{},is_valid => false,params => #{},
         repo => blog_repo,
         required => [id,blog_id,title],
         schema => post,
         types =>
             #{blog_id => integer,body => string,created_at => datetime,
               id => integer,title => string,updated_at => datetime}}}
```

Just like the last time we did an insertion, this returns a tuple. This time
however, the first element in the tuple is `error`, which indicates something
bad happened. The specifics of what happened are included in the changeset which
is returned. We can access these by doing some pattern matching:

```erlang
{error, Changeset1} = blog_repo:insert(Changeset).
```

Then we can get to the errors by doing `xdb_changeset:errors(Changeset1)`:

```erlang
[
  {id,{<<"can't be blank">>,[{validation,required}]}},
  {blog_id,{<<"can't be blank">>,[{validation,required}]}},
  {title,{<<"can't be blank">>,[{validation,required}]}}
]
```

And we can ask the changeset itself it is valid, even before doing an insertion:

```erlang
> xdb_changeset:is_valid(Changeset1).
false
```

Since this changeset has errors, no new record was inserted into the `posts`
table.

Let’s try now with some valid data.

```erlang
> Post = post:schema(#{}).

> Changeset = post:changeset(Post, #{id => 1, blog_id => 1, title => <<"My first post">>}).

> xdb_changeset:errors(Changeset).
[]

> xdb_changeset:is_valid(Changeset).
true
```

The changeset does not have errors, and is valid. Therefore if we try to insert
this changeset it will work:

```erlang
> blog_repo:insert(Changeset).
{ok,#{'__meta__' =>
          #{schema => post,source => posts,state => loaded},
      blog_id => 1,body => undefined,created_at => undefined,
      id => 1,title => <<"My first post">>,
      updated_at => undefined}}
```

## Inserting data

We can insert a new entries into our blog repo with this code:

```erlang
> blog_repo:insert(blog:schema(#{id => 1, name => "erlang"})).
{ok,#{'__meta__' =>
          #{schema => blog,source => blogs,state => loaded},
      created_at => undefined,id => 1,name => <<"erlang">>,
      status => undefined,updated_at => undefined}}

> blog_repo:insert(post:schema(#{id => 1, blog_id => 1, title => "my first post"})).
{ok,#{'__meta__' =>
          #{schema => post,source => posts,state => loaded},
      blog_id => 1,body => undefined,created_at => undefined,
      id => 1,title => <<"my first post">>,
      updated_at => undefined}}

> blog_repo:insert(post:schema(#{id => 2, blog_id => 1, title => "my second post"})).
{ok,#{'__meta__' =>
          #{schema => post,source => posts,state => loaded},
      blog_id => 1,body => undefined,created_at => undefined,
      id => 2,title => <<"my second post">>,
      updated_at => undefined}}
```

## Retrieving entries

We are going to use the entries we inserted before.

### Fetching all entries

To fetch all records from the schema, `cross_db` provides the `all` function:

```erlang
> blog_repo:all(blog).
[#{'__meta__' =>
       #{schema => blog,source => blogs,state => built},
   created_at => undefined,id => 1,name => <<"erlang">>,
   status => undefined,updated_at => undefined}]

> blog_repo:all(post).
[#{'__meta__' =>
       #{schema => post,source => posts,state => built},
   blog_id => 1,body => undefined,created_at => undefined,
   id => 1,title => <<"my first post">>,
   updated_at => undefined},
 #{'__meta__' =>
       #{schema => post,source => posts,state => built},
   blog_id => 1,body => undefined,created_at => undefined,
   id => 2,title => <<"my second post">>,
   updated_at => undefined}]
```

### Filtering results

You can create your own queries/filters using the [xdb_query](../src/xdb_query.erl)
module, like so:

```erlang
> blog_repo:all(xdb_query:from(post, [{where, [{blog_id, 1}]}])).
[#{'__meta__' =>
       #{schema => post,source => posts,state => built},
   blog_id => 1,body => undefined,created_at => undefined,
   id => 1,title => <<"my first post">>,
   updated_at => undefined},
 #{'__meta__' =>
       #{schema => post,source => posts,state => built},
   blog_id => 1,body => undefined,created_at => undefined,
   id => 2,title => <<"my second post">>,
   updated_at => undefined}]

> blog_repo:all(xdb_query:from(post, [{where, [{blog_id, 2}]}])).
[]

> blog_repo:all(xdb_query:from(post, [{where, [{title, <<"my first post">>}]}])).
[#{'__meta__' =>
       #{schema => post,source => posts,state => built},
   blog_id => 1,body => undefined,created_at => undefined,
   id => 1,title => <<"my first post">>,
   updated_at => undefined}]
```

### Fetching a single entry

```erlang
> blog_repo:get(blog, 1).
#{'__meta__' =>
      #{schema => blog,source => blogs,state => built},
  created_at => undefined,id => 1,name => <<"erlang">>,
  status => undefined,updated_at => undefined}

> blog_repo:get(post, 1).
#{'__meta__' =>
      #{schema => post,source => posts,state => built},
  blog_id => 1,body => undefined,created_at => undefined,
  id => 1,title => <<"my first post">>,
  updated_at => undefined}

> blog_repo:get_by(post, [{title, <<"my second post">>}]).
#{'__meta__' =>
      #{schema => post,source => posts,state => built},
  blog_id => 1,body => undefined,created_at => undefined,
  id => 2,title => <<"my second post">>,
  updated_at => undefined}
```

## Updating records

Updating records in CrossDB requires us to first fetch a record from the
database. We then create a changeset from that record and the changes we
want to make to that record, and then call the `your_repo:update` function.

Let’s fetch the first post from our database and change its text. First,
we’ll fetch the post:

```erlang
> Post = blog_repo:get(post, 1).
#{'__meta__' =>
      #{schema => post,source => posts,state => built},
  blog_id => 1,created_at => undefined,id => 1,
  text => <<"my first post">>,updated_at => undefined}
```

Next, let's build a changeset, otherwise `CrossDB` wouldn’t be able to know that
the `text` has changed without inspecting the database. Let’s build the
changeset then:

```erlang
> Changeset = post:changeset(Post, #{title => <<"edited post">>}).
#{action => undefined,
  changes => #{title => <<"edited post">>},
  data =>
      #{'__meta__' =>
            #{schema => post,source => posts,state => built},
        blog_id => 1,body => undefined,created_at => undefined,
        id => 1,title => <<"my first post">>,
        updated_at => undefined},
  errors => [],filters => #{},is_valid => true,
  params => #{title => <<"edited post">>},
  repo => undefined,
  required => [id,blog_id,title],
  schema => post,
  types =>
      #{blog_id => integer,body => string,created_at => datetime,
        id => integer,title => string,updated_at => datetime}}
```

Then update the database with this change:

```erlang
> blog_repo:update(Changeset).
{ok,#{'__meta__' =>
          #{schema => post,source => posts,state => loaded},
      blog_id => 1,body => undefined,created_at => undefined,
      id => 1,title => <<"edited post">>,updated_at => undefined}}
```

We can check if the changes were made:

```erlang
> blog_repo:get(post, 1).
#{'__meta__' =>
      #{schema => post,source => posts,state => built},
  blog_id => 1,body => undefined,created_at => undefined,
  id => 1,title => <<"edited post">>,updated_at => undefined}
```

If the changeset fails for any reason, the result of `blog_repo:update` will be
`{error, Changeset}`. We can see this in action by passing through a blank
`text` in our changeset’s parameters:

```erlang
> ChangesetWithErrors = post:changeset(Post, #{title => <<"">>}).
#{action => undefined,
  changes => #{title => <<>>},
  data =>
      #{'__meta__' =>
            #{schema => post,source => posts,state => built},
        blog_id => 1,body => undefined,created_at => undefined,
        id => 1,title => <<"my first post">>,
        updated_at => undefined},
  errors =>
      [{title,{<<"should be at least 4 character(s)">>,
               [{validation,length}]}}],
  filters => #{},is_valid => false,
  params => #{title => <<>>},
  repo => undefined,
  required => [id,blog_id,title],
  schema => post,
  types =>
      #{blog_id => integer,body => string,created_at => datetime,
        id => integer,title => string,updated_at => datetime}}

> blog_repo:update(ChangesetWithErrors).
{error,#{action => update,
         changes => #{title => <<>>},
         data =>
             #{'__meta__' =>
                   #{schema => post,source => posts,state => built},
               blog_id => 1,body => undefined,created_at => undefined,
               id => 1,title => <<"my first post">>,
               updated_at => undefined},
         errors =>
             [{title,{<<"should be at least 4 character(s)">>,
                      [{validation,length}]}}],
         filters => #{},is_valid => false,
         params => #{title => <<>>},
         repo => blog_repo,
         required => [id,blog_id,title],
         schema => post,
         types =>
             #{blog_id => integer,body => string,created_at => datetime,
               id => integer,title => string,updated_at => datetime}}}
```

So we can also use a `case` statement to do different things depending on the
outcome of the update function:

```erlang
case blog_repo:update(Changeset) of
  {ok, Post} ->
    % do something with post
  {error, ChangesetWithErrors} ->
    % do something with changeset
end,
```

## Deleting records

Similar to updating, we must first fetch a record from the database and then
call `blog_repo:delete` to delete that record:

```erlang
> Post = blog_repo:get(post, 1).
#{'__meta__' =>
      #{schema => post,source => posts,state => built},
  blog_id => 1,body => undefined,created_at => undefined,
  id => 1,title => <<"my first post">>,
  updated_at => undefined}

> blog_repo:delete(Post).
{ok,#{'__meta__' =>
          #{schema => post,source => posts,state => loaded},
      blog_id => 1,body => undefined,created_at => undefined,
      id => 1,title => <<"my first post">>,
      updated_at => undefined}}

> blog_repo:get(post, 1).
undefined
```

Similar to `insert` and `update`, `delete` returns a tuple. If the deletion
succeeds, then the first element in the tuple will be `ok`, but if it fails
then it will be an `error`.
