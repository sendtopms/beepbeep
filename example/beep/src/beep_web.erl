-module(beep_web).
-author('Dave Bryson <http://weblog.miceda.org>').
-export([start/1, stop/0, loop/1, before_filter/1, preprocess/1, before_render/2]).

-include("rberl.hrl").


start(Options) ->
	application:start(ssl),
	rberl_server:start(),
	rberl_server:load("priv/", "lang"),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
	beepbeep:loop(Req, ?MODULE).

%% If necessary, add these hooks:
%% *DON'T FORGET TO EXPORT THEM AS NECESSARY*

%% preprocess/1
%%
%% should return a new Environment
%% or
%% {redirect, Url}

preprocess(Env) ->
	%% do some custom routing
	%% detect language we need to process

	PathInfo = beepbeep_args:path_components(Env),
	Path = list_to_binary(PathInfo),

	Session = beepbeep_args:get_session_data(Env),


	Language = case proplists:get_value("language", Session) of
		undefined ->
			"en-US";
		Any ->
			Any
	end,

	beepbeep_args:set_session_data(Env, "language", Language),

	case Path of
		<<>> ->
			{redirect , "/"++Language++"/"};
		<<A,B,$-,C,D>> ->
			%% we have a language set it
			beepbeep_args:set_session_data(Env, "language", [A,B,$-,C,D]),
			beepbeep_args:set_value("PATH_INFO", "/", Env);
		<<A,B,$-,C,D,Rest/binary>> ->
			beepbeep_args:set_session_data(Env, "language", [A,B,$-,C,D]),
			beepbeep_args:set_value("PATH_INFO", "/" ++ string:join(tl(PathInfo), "/"), Env);
		_ ->
			{redirect , "/"++Language++"/" ++ string:join(PathInfo, "/")}
	end.


%% before_filter/1
%%
%% Should return one of:
%% ok
%% {render, View, Data}
%% {render, View, Data, Options}
%% {static, File}
%% {text, Data}
%% {redirect, Url}
%% {controller, ControllerName}
%% {controller, ControllerName, ActionName}
%% {controller, ControllerName, ActionName, Args}
%%
before_filter(Env) ->
	ok.

%% before_render/2
%%
%% This hook accepts one of these tuples:
%% {render, View, Data}
%% {render, View, Data, Options}
%% {static, File}
%% {text, Data}
%%
%% Should return one of:
%% {render, View, Data}
%% {render, View, Data, Options}
%% {static, File}
%% {text, Data}
%% {redirect, Url}
%%
before_render({render, View, Data, Options}, Env) ->
	Session = beepbeep_args:get_session_data(Env),
	Language = proplists:get_value("language", Session),
	PathInfo = string:join(beepbeep_args:path_components(Env), "/"),

	Languages = lists:filter(fun(T) -> T =/= Language end, ?LANGUAGES),

	{render, View, Data ++ [
		 {list_to_atom("menu_link_" ++ beepbeep_args:get_controller(Env)), true}
		,{language, Language}
		,{languages, Languages}
		,{path, PathInfo}
		,?TXT("menu_home")
		,?TXT("menu_download")
		,?TXT("menu_documentation")
		,?TXT("menu_faq")
	], Options}.
%%
%% error/2
%%
%% Catch some errors:
%%
%% Should return one of:
%% {error, Reason}
%% {render, View, Data}
%% {render, View, Data, Options}
%% {static, File}
%% {text, Data}
%% {redirect, Url}
%%
%% error({error, _Reason} = Error, _Env) ->
%%	Error.