-module(reload_mk).
-export([reload/0]).

reload() ->
	Apps = [App || {App, _, _} <- application:which_applications()],
	AppResourceFiles =
		[code:where_is_file(lists:flatten(io_lib:format("~s.app", [App])))
		 || App <- Apps],
	AppProperties =
		[file:consult(ResourceFile) || ResourceFile <- AppResourceFiles],
	Mods = lists:flatten(
		[proplists:get_value(modules, Properties)
		 || {ok, [{application, _, Properties}]} <- AppProperties]
	),
	_ = [maybe_reload(Mod) || Mod <- Mods],
	ok.

maybe_reload(Mod) ->
	Path = code:which(Mod),
	case is_list(Path) andalso filelib:is_file(Path) of
		true -> maybe_reload(Mod, Path);
		false -> ignore
	end.

maybe_reload(Module, Path) ->
	case code:get_object_code(Module) of
		{_, ObjectCode, _} ->
			case code:is_loaded(Module) of
				{file, _} ->
					reload_if_changed(Module, Path, ObjectCode);
				false ->
					try_load_module(Module, Path, ObjectCode, false)
			end;
		error ->
			error_logger:warning_report([
				{?MODULE, "Could not get module's object code"},
				{module, Module},
				{path, Path}
			])
	end.

reload_if_changed(Module, Path, ObjectCode) ->
	case beam_lib:md5(ObjectCode) of
		{ok, {_, MD5}} ->
			case Module:module_info(md5) of
				MD5 -> ignore;
				_ ->
					try_load_module(Module, Path, ObjectCode, true)
			end;
		{error, beam_lib, Reason} ->
			error_logger:warning_report([
				{?MODULE, "Could not get module's checksum"},
				{module, Module},
				{path, Path},
				{reason, Reason}
			])
	end.

try_load_module(Module, Path, ObjectCode, IsReload) ->
	case code:load_binary(Module, Path, ObjectCode) of
		{module, _} ->
			Msg =
				case IsReload of
					true -> "Reloaded";
					false -> "Loaded"
				end,
			error_logger:info_report([
				{?MODULE, Msg},
				{module, Module},
				{path, Path}
			]);
		{error, Reason} ->
			Msg =
				case IsReload of
					true -> "Could not reload module";
					false -> "Could not load module"
				end,
			error_logger:info_report([
				{?MODULE, Msg},
				{module, Module},
				{path, Path},
				{reason, Reason}
			])
	end.
