%%
%% adopted from
%% http://21ccw.blogspot.com/2009/05/how-to-send-email-via-gmail-using.html
%%

-module(smtp).
-export([send/1]).

send(Data) ->
	case ssl:connect("smtp.gmail.com", 465, [{active, false}], 1000) of
		{ok, Socket} ->
			send1(Socket, Data);
		{error, Reason} ->
			io:format("Error: ~p~n", [Reason]),
			spawn(
				fun() ->
						receive
							after 1000 -> ?MODULE:send(Data)
						end
				end
			)
	end.

send1(Socket, Data) ->
    recv(Socket),
    send(Socket, "HELO localhost"),
    send(Socket, "AUTH LOGIN"),
    send(Socket, binary_to_list(base64:encode("___@gmail.com"))),
    send(Socket, binary_to_list(base64:encode("___password"))),
    send(Socket, "MAIL FROM: <___@gmail.com>"),
    send(Socket, "RCPT TO:<___@gmail.com>"),
    send(Socket, "DATA"),
    send_no_receive(Socket, "From: <___@gmail.com>"),
    send_no_receive(Socket, "To: <___@gmail.com>"),
    send_no_receive(Socket, "Date: Tue, 15 Jan 2008 16:02:43 +0000"),
    send_no_receive(Socket, "Subject: BeepBeep - Demo site"),
    send_no_receive(Socket, ""),
    send_no_receive(Socket, Data),
    send_no_receive(Socket, ""),
    send(Socket, "."),
    send(Socket, "QUIT"),
    ssl:close(Socket).

send_no_receive(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n").


send(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n"),
    recv(Socket).

recv(Socket) ->
    case ssl:recv(Socket, 0, 1000) of
 {ok, Return} -> io:format("~p~n", [Return]);
 {error, Reason} -> io:format("ERROR: ~p~n", [Reason])
    end.