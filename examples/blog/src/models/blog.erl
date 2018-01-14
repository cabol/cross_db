-module(blog).

-include_lib("cross_db/include/xdb.hrl").
-schema({blogs, [
  {id,         integer,  [primary_key]},
  {name,       string},  % by default the options are []
  {status,     string},
  {created_at, datetime, [{setter, false}]},
  {updated_at, datetime}
]}).
