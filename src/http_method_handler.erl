%% Author: Senthilkumar Peelikkampatti
%% Created: Aug 16, 2009
%% 
%% 
%%====================================================================
%% http_method_handler -- It is a hack for mochiweb_multipar API.
%%====================================================================

-module(http_method_handler).


-export([default_file_handler/2, default_file_handler_1/3, parse_post/2, parse_data/1]).


%%====================================================================
%% It is file hadler function for mochi but it is exactly copy for 
%% Mochiweb, May need specialized handler for big files.
%%====================================================================
default_file_handler(Filename, ContentType) ->
%% 	io:format(" default_file_handler Filename is ~p  and ContentType is ~p~n", [Filename, ContentType]),
	default_file_handler_1(Filename, ContentType, []).


%%====================================================================
%% It is file hadler function for mochi but it is exactly copy for 
%% Mochiweb, May need specialized handler for big files.
%%====================================================================
default_file_handler_1(Filename, ContentType, Acc) ->
    fun(eof) ->
            Value = iolist_to_binary(lists:reverse(Acc)),
%% 			io:format(" My File handler and EOF is ~p ~n", [Value]),
            {Filename, ContentType, Value};
       (Next) ->
%% 			io:format(" My File handler and Next is ~p ~n", [Next]),
            default_file_handler_1(Filename, ContentType, [Next | Acc])
    end.


%%====================================================================
%% It is helper method for controllers,
%% This can be reused whenever there is a req for handling POST and 
%%  which expects 
%%   1. multipart/form-data
%% 	 2. application/x-www-form-urlencoded
%%====================================================================
parse_post(Ctx, "multipart/form-data") ->
	Content_length= ewgi_api:content_length(Ctx),
%% 	io:format("multipart/form-data Content_length is ~p~n", [Content_length]),
	Req = mochi_fileupload_curry:new(Ctx),
%% 	io:format("Hi ....~n"),
	RequestContent = mochiweb_multipart:parse_form(Req, fun default_file_handler/2);
%% 	io:format(" Filename Content is =~p~n", [RequestContent]), RequestContent;


%%====================================================================
%% See above
%%====================================================================
parse_post(Ctx, "application/x-www-form-urlencoded") ->
	Content_length= ewgi_api:content_length(Ctx),
	
%% 	io:format("Content_length is ~p~n", [Content_length]),
	Vals = ewgi_api:read_input_string(Content_length, Ctx),
	ewgi_api:parse_post(Vals).


%%====================================================================
%% This is the method indented to handle different type of http method
%% This method returns the FUN which later takes key to return value.
%% 
%%====================================================================

parse_data(Ctx) ->
	   Data = case ewgi_api:request_method(Ctx) of
               'GET' -> 
				   ewgi_api:parse_qs(ewgi_api:query_string(Ctx));
               _     -> 
%% 				   io:format("In POST MEthod ~p~n", [ewgi_api:remote_user_data(Ctx)]),
						case ewgi_api:remote_user_data(Ctx) of
							undefined ->
%% 								io:format("In undefined~n"),
                            	%% Check content-type first
                            	Ct = ewgi_api:content_type(Ctx),
%% 								io:format("11Ct ~p~n", [Ct]),							
								Vals= parse_post(Ctx, parse_ct(Ct)),
%% 	 							io:format("Vals ~p~n", [Vals]),
								Vals;
							_->
								ok
						end
			  
           end,
	   fun (Key) when Data =/= ok ->
				proplists:get_value(Key, Data) end.


%% Parse content-type (ignoring additional vars for now)
%% Should look like "major/minor; var=val"
parse_ct(L) when is_list(L) ->
    case string:tokens(L, ";") of
        [H|_] ->
            H;
        _ ->
            undefined
    end.
	
