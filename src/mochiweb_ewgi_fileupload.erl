-module(mochi_fileupload_curry, [Ctx]).
-author('Senthilkumar Peelikkampatti <http://pmsenthilkumar.blogspot.com/>').

-export([recv/1, get_header_value/1]).



%%====================================================================
%% It is adapter methor for mochiweb_multipart
%%====================================================================

recv(Length) ->
%% 	io:format("Curryyyyyyy in recv ~p~n", [Length]),
	
	Bin = iolist_to_binary(ewgi_api:read_input_string(Length, Ctx)),
%% 		io:format(" Bin in recv ~p~n", [Bin]),
	 Bin.


%%====================================================================
%% It is adapter methor for mochiweb_multipart
%%====================================================================
get_header_value ("content-length") ->
%% 	io:format("mochi_fileupload_curry content-length ~p~n", [ewgi_api:content_length(Ctx)]),
%% 	io:format("mochi_fileupload_curry content-length ~p~n", [list_to_integer(integer_to_list(ewgi_api:content_length(Ctx)))]),
	integer_to_list(ewgi_api:content_length(Ctx));


%%====================================================================
%% It is adapter methor for mochiweb_multipart
%%====================================================================
get_header_value ("content-type") ->
%% 	io:format("mochi_fileupload_curry content-type in recv~n"),
	ewgi_api:content_type(Ctx).


