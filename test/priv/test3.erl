-module(test3).
-export([reload/0]).
-on_reload(reload/0).

reload() ->
	file:write_file("reloaded", "true").
