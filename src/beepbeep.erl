%%
%% Dispatcher called from the mochiweb server
%% Maps Urls to controllers and their views
%%
-module(beepbeep).
-author('Dave Bryson <http://weblog.miceda.org>').

-export([loop/2,dispatch/2,get_view_file/1]).
-include("beepbeep.hrl").

loop(Req, AppWebModule) ->
	%% Setup env...
    InitialEnv = mochiweb_env:setup_environment(Req),
    Env = setup_session(Req,InitialEnv),

    %%error_logger:info_report(Env),

    case dispatch(Env, AppWebModule) of
	{ok,Status,ContentType,H,Content} ->
	    Cookie = get_cookie(Env),
	    Headers = [Cookie|H],
	    Req:respond({Status,[{"Content-Type",ContentType}|Headers],Content});
	    %%Req:ok({"text/html",Headers,Content});
	{redirect,Url} ->
	    Req:respond({302,
                         [{"Location", Url},
                          {"Content-Type", "text/html; charset=UTF-8"}],
                         ""});
	{static, File} ->
	    "/" ++ StaticFile = File,
		Tokens = string:tokens(atom_to_list(AppWebModule), "_"),
		AppNameTokens = lists:flatten(lists:sublist(Tokens, length(Tokens) - 1)),
		Deps = list_to_atom(lists:flatten(AppNameTokens ++ "_deps")),
	    Req:serve_file(StaticFile,Deps:local_path(["www"]));
	{error,_} ->
	    Req:respond({500,[],"Server Error"})
    end.


get_cookie(Env) ->
    mochiweb_cookies:cookie(?BEEPBEEP_SID,beepbeep_args:get_session_id(Env),[{path, "/"}]).

setup_session(Req,Env) ->
    SessionKey = beepbeep_session_server:new_session(Req:get_cookie_value(?BEEPBEEP_SID)),
    beepbeep_args:set_session_id(SessionKey,Env).


%% @private
%% Performs actual magic of rerouting/rendering etc.
%% see http://files.dmitriid.com/beepbeep/beepbeep.png for actual flow

dispatch(Env, AppWebModule) ->
    PathComponents = beepbeep_args:path_components(Env),
    %% Map the request to our app
    {ControllerName,ActionName,Args}  = case PathComponents of
					    [] ->
						{"home","index",[]};
					    [C] ->
						{C,"index",[]};
					    [C,A | Params]  ->
						{C,A,Params}
					end,
    case beepbeep_router:get_controller(ControllerName) of
	{ok,Controller} ->
		case try_app_filter(AppWebModule, Env) of
			ok ->
				process_request(AppWebModule,Env,Controller,ActionName,Args);
			Response ->
				preprocess_request(Response, AppWebModule,Env,Controller,ActionName,Args)
		end;
	no_controller ->
	    %% Try static
	    F = beepbeep_args:path(Env),
		preprocess_request({static,  F}, AppWebModule,Env,undefined,ActionName,Args)
    end.



get_view_file(ViewFile) ->
    %%filename:join([get_base_dir()|["views",ViewFile]]),
    beepbeep_router:get_view(ViewFile).


%%get_static_path() ->
%%    filename:join([get_base_dir()|["www"]]).


%%% Internal below
%%get_base_dir() ->
%%    {file, Here} = code:is_loaded(?MODULE),
%%    filename:dirname(filename:dirname(Here)).

process_request(AppWebModule,Env,ControllerName,ActionName,Args) ->
    Env1 = beepbeep_args:set_action(Env,ActionName),
    error_logger:info_report(Env1),
    Controller = ControllerName:new(Env1),
    case try_filter(Controller) of
	ok ->
	    case catch(Controller:handle_request(ActionName,Args)) of
		{'EXIT',_} ->
			case try_app_error(AppWebModule, {error, no_action}, Env) of
				{error, Reason} ->
					{error, Reason};
				Response ->
					preprocess_request(Response, AppWebModule,Env,Controller,ActionName,Args)
			end;
		Response ->
			preprocess_request(Response, AppWebModule,Env,Controller,ActionName,Args)
	    end;
	Any ->
	    preprocess_request(Any, AppWebModule,Env,Controller,ActionName,Args)
    end.

%% handle renderers
preprocess_request({render, View, Data} = _Request, AppWebModule,Env,Controller,ActionName,Args) ->
	preprocess_request({render, View, Data, []}, AppWebModule,Env,Controller,ActionName,Args);
preprocess_request({render, _View, _Data, _Options} = Request, AppWebModule,Env,Controller,ActionName,_Args) ->
    Env1 = beepbeep_args:set_action(Env,ActionName),
	NewRequest = pre_render(Request, AppWebModule, Env1, Controller),
	handle_response(NewRequest);
preprocess_request({text, _Data} = Request, AppWebModule,Env,Controller,ActionName,_Args) ->
    Env1 = beepbeep_args:set_action(Env,ActionName),
	NewRequest = pre_render(Request, AppWebModule, Env1, Controller),
	handle_response(NewRequest);
preprocess_request({static, _File} = Request, AppWebModule,Env,Controller,ActionName,_Args) ->
    Env1 = beepbeep_args:set_action(Env,ActionName),
	NewRequest = pre_render(Request, AppWebModule, Env1, Controller),
	handle_response(NewRequest);

%% handle internal redirects
preprocess_request({controller, ControllerName} = _Request, AppWebModule,Env,Controller,ActionName,Args) ->
	preprocess_request({controller, ControllerName, "index", []}, AppWebModule,Env,Controller,ActionName,Args);
preprocess_request({controller, ControllerName, MethodName} = _Request, AppWebModule,Env,Controller,ActionName,Args) ->
	preprocess_request({controller, ControllerName, MethodName, []}, AppWebModule,Env,Controller,ActionName,Args);
preprocess_request({controller, ControllerName, MethodName, NewArgs} = _Request, AppWebModule,Env,_Controller,_ActionName,_Args) ->
    case beepbeep_router:get_controller(ControllerName) of
	{ok,Controller} ->
		process_request(AppWebModule,Env,Controller,MethodName,NewArgs);
	no_controller ->
	    %% Try static
	    F = beepbeep_args:path(Env),
		preprocess_request({static,  F}, AppWebModule,Env,undefined,MethodName,NewArgs)
    end;

%% process all other requests as is
preprocess_request(Request, _AppWebModule,_Env,_Controller,_ActionName,_Args) ->
	handle_response(Request).

pre_render(Request, AppWebModule,Env,Controller) ->
	NewRequest = try_render(Controller, Request),
	try_app_render(AppWebModule, NewRequest, Env).

try_app_error(AppWebModule,Error, Env) ->
    case catch(AppWebModule:error(Error, Env)) of
	{'EXIT', {undef,_}} ->
	    Error;
	Any ->
	    Any
    end.
try_app_filter(AppWebModule, Env) ->
    case catch(AppWebModule:before_filter(Env)) of
	{'EXIT', {undef,_}} ->
	    ok;
	Any ->
	    Any
    end.
try_filter(ControllerName) ->
    case catch(ControllerName:before_filter()) of
	{'EXIT', {undef,_}} ->
	    ok;
	Any ->
	    Any
    end.
try_render(ControllerName, Response) ->
    case catch(ControllerName:before_render(Response)) of
	{'EXIT', {undef,_}} ->
	    Response;
	Any ->
	    Any
    end.
try_app_render(AppWebModule, Response, Env) ->
    case catch(AppWebModule:before_render(Response, Env)) of
	{'EXIT', {undef,_}} ->
	    Response;
	Any ->
	    Any
    end.

%% Handle all responses from controller
handle_response({render,View,Data}) ->
    {ok,Content} = render_template(View,Data),
    {ok,200,"text/html",[],Content};

handle_response({render,View,Data,Options}) ->
    {ok,Content} = render_template(View,Data),
    {ok,
     proplists:get_value(status,Options,200),
     proplists:get_value(content_type,Options,"text/html"),
     proplists:get_value(headers,Options,[]),
     Content};

handle_response({text,Data}) ->
    {ok,200,"text/plain",[],Data};

%% This seems stupid...better way??
handle_response({redirect,Url}) ->
    {redirect,Url};

handle_response({static,File}) ->
    {static,File}.

render_template(ViewFile,Data) ->
    FullPathToFile = get_view_file(ViewFile),
    error_logger:info_msg("Trying file: ~s~n",[FullPathToFile]),
    Pieces = string:tokens(ViewFile,"/"),
    Name = string:join(Pieces,"_"),
    Name1 = filename:basename(Name,".html"),
    ModName = list_to_atom(Name1 ++ "_view"),

    erlydtl:compile(FullPathToFile,ModName),
    ModName:render(Data).


