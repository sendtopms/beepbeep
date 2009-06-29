-define(TXT, 
	fun(K) ->
		_Session = beepbeep_args:get_session_data(Env),
		_Language = case proplists:get_value("language", _Session) of
			undefined ->
				"en-US";
			[A, B, $-, C, D] ->
				[A, B, $_, C, D];
			Any ->
				Any
		end,
		{list_to_atom(K), rberl_server:get(K, _Language)}
	end).


-define(LANGUAGES, ["en-US", "ru-RU", "tr-TR", "ro-RO"]).