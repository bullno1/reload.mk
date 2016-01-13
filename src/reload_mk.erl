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
	case compare_modules(Module, ObjectCode) of
		true ->
			ingore;
		false ->
			try_load_module(Module, Path, ObjectCode, true);
		{error, Msg, Reason} ->
			error_logger:warning_report([
				{?MODULE, Msg},
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
			error_logger:error_report([
				{?MODULE, Msg},
				{module, Module},
				{path, Path},
				{reason, Reason}
			])
	end.

compare_modules(Module, ObjectCode) ->
	ModuleInfo = Module:module_info(),
	case lists:keyfind(md5, 1, ModuleInfo) of
		{md5, ModuleMD5} -> % Modules in OTP 18.0+ has "md5" in their info
			compare_md5(ModuleMD5, ObjectCode);
		false -> % Modules in earlier version do have this info
			CompileInfo = proplists:get_value(compile, ModuleInfo),
			compare_compile_info(CompileInfo, ObjectCode)
	end.

compare_md5(ModuleMD5, ObjectCode) ->
	case beam_lib:md5(ObjectCode) of
		{ok, {_, ObjectCodeMD5}} -> ObjectCodeMD5 =:= ModuleMD5;
		{error, beam_lib, Reason} ->
			{error, "Could not get module's md5", Reason}
	end.

compare_compile_info(ModuleCompileInfo, ObjectCode) ->
	case beam_lib:chunks(ObjectCode, [compile_info]) of
		{ok, {_, [{_, ObjectCodeCompileInfo}]}} ->
			compare(ModuleCompileInfo, ObjectCodeCompileInfo);
		{error, beam_lib, Reason} ->
			{error, "Could not get module's compile info", Reason}
	end.

compare(Lhs, Rhs) -> normalize(Lhs) =:= normalize(Rhs).

normalize([{_, _} | _] = Item) ->
	lists:sort([{K, normalize(V)} || {K, V} <- Item]);
normalize(Item) ->
	Item.
