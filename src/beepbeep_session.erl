-module(beepbeep_session).

-export([run/2]).
-include("beepbeep.hrl").

run(Ctx, App) ->
	Cookies = ewgi_api:get_header_value("cookie", Ctx),
	SessionKey = case Cookies of
		undefined -> beepbeep_session_server:new_session(undefined);
		_ ->
			Cookie = proplists:get_value(?BEEPBEEP_SID, beepbeep_cookies:parse_cookie(Cookies)),
			beepbeep_session_server:new_session(Cookie)
	end,

	Ctx1 = ewgi_api:store_data("beep.session_id", SessionKey, Ctx),
	{ewgi_context, Request, Response} = App(Ctx1),

	SetCookieHeader = beepbeep_cookies:cookie(?BEEPBEEP_SID, SessionKey, [{path, "/"}]),
	Ctx2 = ewgi_api:response_headers(SetCookieHeader, Ctx1),
	Ctx2.
