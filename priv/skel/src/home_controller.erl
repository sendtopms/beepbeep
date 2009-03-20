%%
%% Sample default controller
%%
-module(home_controller,[Env]).

-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    {render,"home/index.html",[{data,"Hello There From BeepBeep!"}]};

handle_request("show",[Year]) ->
    Sid = beepbeep_args:get_session_id(Env),
    Name = beepbeep_args:get_param("name",Env),
    {render,"home/show.html",[{year,Year},{sid,Sid},{name,Name}]}.


before_filter() ->
    FilterOnly = ["show"],
    case lists:member(beepbeep_args:get_action(Env),FilterOnly) of
	true ->
	    error_logger:info_report("Doing the filter for SHOW~n"),
	    ok;
	false ->
	    ok
    end.

    
%%
%% This hook accepts any tuple that BeepBeep understands:
%% {render, View, Data}
%% {render, View, Data, Options}
%% {text, Data}
%% and so on. See beepbeep.erl
%% It should also return a tuple that BeepBeep understands
%%
%% General hook:
%% before_render(Response) ->
%%	ok.
%%
%% Specific hook:
%% before_render({render, View, Data, Options}) ->
%%	ok.