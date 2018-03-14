-module(post).

-export([changeset/2]).

-include_lib("cross_db/include/xdb.hrl").
-schema({posts, [
  {id,         integer,  [primary_key]},
  {blog_id,    integer},  % by default the options are []
  {title,      string},
  {body,       string},
  {created_at, datetime, [{setter, false}]},
  {updated_at, datetime}
]}).

-spec changeset(t(), xdb_schema:fields()) -> xdb_changeset:t().
changeset(Post, Params) ->
  [pipe](
    Post,
    post:schema(_),
    xdb_changeset:cast(_, Params, [id, blog_id, title, body, created_at, updated_at]),
    xdb_changeset:validate_required(_, [id, blog_id, title]),
    xdb_changeset:validate_length(_, title, [{min, 4}, {max, 256}])
  ).
