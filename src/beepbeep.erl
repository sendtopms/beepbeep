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
	    Req:serve_file(StaticFile,bbb_deps:local_path(["www"]));
	{error,_} ->
	    Req:respond({500,[],"Server Error"})
    end.


get_cookie(Env) ->
    mochiweb_cookies:cookie(?BEEPBEEP_SID,beepbeep_args:get_session_id(Env),[{path, "/"}]).

setup_session(Req,Env) ->
    SessionKey = beepbeep_session_server:new_session(Req:get_cookie_value(?BEEPBEEP_SID)),
    beepbeep_args:set_session_id(SessionKey,Env).


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
		case try_app_filter(AppWebModule) of
			ok ->
				process_request(AppWebModule,Env,Controller,ActionName,Args);
			Response ->
				NewResponse = try_render(Controller, Response),
				FinalResponse = try_app_render(AppWebModule, NewResponse),
				handle_response(FinalResponse)
		end;
	no_controller ->
	    %% Try static
	    F = beepbeep_args:path(Env),
	    {static,  F}
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
			case try_app_error(AppWebModule, {error, no_action}) of
				{error, Reason} ->
					{error, Reason};
				Response ->
					NewResponse = try_render(Controller, Response),
					FinalResponse = try_app_render(AppWebModule, NewResponse),
					handle_response(FinalResponse)
			end;
		Response ->
			NewResponse = try_render(Controller, Response),
			FinalResponse = try_app_render(AppWebModule, NewResponse),
		    handle_response(FinalResponse)
	    end;
	Any ->
	    Any
    end.

try_app_error(AppWebModule,Error) ->
    case catch(AppWebModule:error(Error)) of
	{'EXIT', {undef,_}} ->
	    Error;
	Any ->
	    Any
    end.
try_app_filter(AppWebModule) ->
    case catch(AppWebModule:before_filter()) of
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
try_app_render(AppWebModule, Response) ->
    case catch(AppWebModule:before_render(Response)) of
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
	    
	    
