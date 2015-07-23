-module(debris_handler).
-behaviour(simple_bridge_handler).
-export([run/1,
        ws_init/1,
        ws_message/3,
        ws_info/3,
        ws_terminate/3]).

-define(NOTFOUND, "<html><body><h1>Not found</h1></body></html>").
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
run(Bridge) ->
    % Test if a file is requested, otherwise return 404 Not Found
    {ok, DocRoot} =  application:get_env(simple_bridge, document_root),
    Url = case filename:split(sbw:path(Bridge)) of
                ["/"]       -> "index.html" ;
                ["/", Repo] -> Repo ++ "/index.html" ;
                [_ | Frags] -> filename:join(Frags)
          end,
    Target = filename:join([DocRoot, Url]),
    BridgeRet = case filelib:is_dir(Target) of
                     true  -> Bridge2 = sbw:set_status_code(404, Bridge),
                              sbw:set_response_data(?NOTFOUND, Bridge2);
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
                                        sbw:set_response_data([binary_to_list(HTML)], Bridge3);
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

ws_message({text, Data}, _State, _Bridge) ->
    {reply, {text, Data}};
ws_message({binary, Data}, _State, _Bridge) ->
    {reply, {binary, Data}}.

ws_info(Data, _Bridge, _State) ->
    Reply = {text, io_lib:format("~s", [Data])},
    {reply, Reply}.

ws_terminate(_Reason, _Bridge, _State) ->
    ok.

