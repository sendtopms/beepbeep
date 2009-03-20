-module(skel_web).
-author('Dave Bryson <http://weblog.miceda.org>').

-export([start/1, stop/0, loop/1]).

start(Options) ->
    Loop = fun (Req) ->
                   ?MODULE:loop(Req)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
	beepbeep:loop(Req, ?MODULE).

%% If necessary, add these filters:
%% *DON'T FORGET TO EXPORT THEM AS NECESSARY*
%%
%%
%% before_filter() ->
%%	ok. %% or any tuple that beepbeep understands, similar to controllers'
%%      %% before_filter/1
%%
%%

%%
%% This hook accepts any tuple that BeepBeep understands:
%% {render, View, Data}
%% {render, View, Data, Options}
%% {text, Data}
%% and so on. See beepbeep.erl
%%
%% General hook:
%% before_render(Response) ->
%%	ok. 
%%
%% Specific hook:
%% before_render({render, View, Data, Options}) ->
%%	ok.


%%
%% Catch some errors:
%%
%% error({error, _Reason} = Error) ->
%%	Error.