%%%------------------------------------------------------------------------
%%% File:      debris_handler.erl
%%% @author    Eric Pailleau <debris@crownedgrouse.com>
%%% @copyright 2015 Eric Pailleau 
%%% @doc  
%%% debris' handler for simple_bridge
%%% @end  
%%% The MIT License (MIT):
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-------------------------------------------------------------------------
-module(debris_handler).

-behaviour(simple_bridge_handler).

-export([run/1,
        ws_init/1,
        ws_message/3,
        ws_info/3,
        ws_terminate/3]).

-define(NOTFOUND, "<html><body><h1>Not found.</h1></body></html>").

%%-------------------------------------------------------------------------
%% @doc Main handler entry point
%% @end
%%-------------------------------------------------------------------------

run(Bridge) ->
    % Should index.html be created ?
    Index = application:get_env(debris, index, true),
    % Test if a file is requested, otherwise return index.html if allowed otherwise return 404 Not Found
    {ok, DocRoot} =  application:get_env(simple_bridge, document_root),
    Url = case filename:split(sbw:path(Bridge)) of
                ["/"]       -> "index.html" ;
                ["/", Repo] -> Repo ++ "/index.html" ;
                [_ | Frags] -> filename:join(Frags)
          end,
    Target = filename:join([DocRoot, Url]),
    BridgeRet = case filelib:is_dir(Target) of
                     true  -> case Index of
                                  false -> Bridge2 = sbw:set_status_code(404, Bridge),
                                           sbw:set_response_data(?NOTFOUND, Bridge2) ;
                                  true  -> Bridge2 = sbw:set_header("Content-Type", "text/html", Bridge),
                                           Bridge3 = sbw:set_status_code(200, Bridge2),
                                           sbw:set_response_data([debris_index:get_index(debris_index_tpl, Url, DocRoot, sbw:query_params(Bridge))], Bridge3)
                              end;                              
                     false -> % Specify mimetype
                              Mime = case filename:extension(Target) of
                                          ".gpg" -> "application/pgp-keys; charset=us-ascii";
                                          ".asc" -> "application/pgp-keys; charset=us-ascii";
                                          ".deb" -> "application/x-debian-package; charset=binary" ;
                                          ".gz"  -> "application/gzip; charset=binary" ;
                                          ".dsc" -> "text/plain; charset=us-ascii" ;
                                          ".htm" -> "text/html; charset=us-ascii" ;
                                          ".html"-> "text/html; charset=us-ascii" ;
                                          _      -> "text/plain; charset=us-ascii" 
                                     end,
                              case Mime of
                                   "text/html; charset=us-ascii" ->
                                        Bridge2 = sbw:set_header("Content-Type", "text/html", Bridge),
                                        Bridge3 = sbw:set_status_code(200, Bridge2),
                                        {ok, HTML} = file:read_file(Target),
                                        IndexHtml = case Index of
                                                         true  -> debris_index:get_index(debris_index_tpl, Url, DocRoot, sbw:query_params(Bridge)) ;
                                                         false -> []
                                                    end,
                                        sbw:set_response_data([IndexHtml] ++ [binary_to_list(HTML)], Bridge3);
                                   _ -> Bridge2 = sbw:set_header("Content-Type", Mime, Bridge),
                                        Bridge3 = sbw:set_status_code(200, Bridge2),
                                        sbw:set_response_file(Target, Bridge3)
                             end
                end,
    BridgeRet:build_response().

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


ws_init(_Bridge) -> ok.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


ws_message({text, Data}, _State, _Bridge) ->
    {reply, {text, Data}};
ws_message({binary, Data}, _State, _Bridge) ->
    {reply, {binary, Data}}.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


ws_info(Data, _Bridge, _State) ->
    Reply = {text, io_lib:format("~s", [Data])},
    {reply, Reply}.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


ws_terminate(_Reason, _Bridge, _State) ->
    ok.





