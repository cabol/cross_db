-module(account).

-include_lib("cross_db/include/xdb.hrl").
-schema({account, [
  {id,       integer, [primary_key]},
  {username, string,  [primary_key]},
  {password, string}
]}).
