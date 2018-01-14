-module(blog_repo).

-include_lib("cross_db/include/xdb.hrl").
-repo([{otp_app, blog}, {adapter, xdb_mnesia_adapter}]).

%% Inherit the default repo `init/1` callback from `xdb_mnesia_boot_repo`
-include_lib("mixer/include/mixer.hrl").
-mixin([xdb_mnesia_boot_repo]).
