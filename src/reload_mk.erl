-module(reload_mk).
-export([reload/0]).

reload() ->
	_ = [maybe_reload(Module, Path) ||
		 {Module, Path} <- code:all_loaded(),
		 is_list(Path), filelib:is_file(Path)],
	ok.

maybe_reload(Module, Path) ->
	case code:get_object_code(Module) of
		{_, ObjectCode, _} ->
			case beam_lib:md5(ObjectCode) of
				{ok, {_, MD5}} ->
					case Module:module_info(md5) of
						MD5 -> ignore;
						_ ->
							case code:load_binary(Module, Path, ObjectCode) of
								{module, _} ->
									error_logger:info_report([
										{?MODULE, "Reloaded"},
										{module, Module},
										{path, Path}
									]);
								{error, Reason} ->
									error_logger:info_report([
										{?MODULE, "Could not reload module"},
										{module, Module},
										{path, Path},
										{reason, Reason}
									])
							end
					end;
				{error, beam_lib, Reason} ->
					error_logger:warning_report([
						{?MODULE, "Could not get module's checksum"},
						{module, Module},
						{path, Path},
						{reason, Reason}
					])
			end;
		error ->
			error_logger:warning_report([
				{?MODULE, "Could not get module's object code"},
				{module, Module},
				{path, Path}
			])
	end.
