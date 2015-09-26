%%%------------------------------------------------------------------------
%%% File:      debris_http_fsm.erl
%%% @author    Eric Pailleau <debris@crownedgrouse.com>
%%% @copyright 2015 Eric Pailleau 
%%% @doc  
%%% debris' http start/stop finite state machine
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

-module(debris_http_fsm).

-behaviour(gen_fsm).

-export([start_link/0]).
-export([http_start/2, http_stop/2]).
-export([init/1, terminate/3]).

-record(http, { backend
               ,address
               ,port
               ,document_root
}).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


init(_) ->
    Address       = application:get_env(debris, address, "0.0.0.0"),
    Port          = application:get_env(debris, port, 8000),
    DefRootDir    = filename:join(code:priv_dir(debris), "www"),
    DocRoot       = application:get_env(debris, document_root, DefRootDir),
    ok            = filelib:ensure_dir(filename:join(DocRoot, "fakedir")),
    Backend       = application:get_env(debris, backend, inets),
    %Statics       = application:get_env(debris, static_paths, []),
    %Statics = application:get_env(debris, cowboy_dispatch, []),

    Ret = case Backend of
           none -> ignore ;
           _    -> 
                    % Override simple_bridge config variables
                    application:load(simple_bridge),
                    lists:foreach(fun({Par, Val}) -> ok = application:set_env(simple_bridge, Par, Val, [{persistent, true}]) end,
                                [{address, Address}, 
                                 {port, Port}, 
                                 {document_root, DocRoot}, 
                                 {backend, Backend}, 
                                 %{static_paths, Statics}, 
                                 {handler, debris_handler}]),
                    ok = application:start(simple_bridge), 
                    {ok, http_start, #http{ backend=Backend
                                           ,address=Address
                                           ,port=Port
                                           ,document_root=DocRoot}}
    end,
    Ret.

%%-------------------------------------------------------------------------
%% @doc Stop http backend
%% @end
%%-------------------------------------------------------------------------


http_start(http_stop, StateData) -> 
                % Stop
                ok = application:stop(simple_bridge), 
                {next_state, http_stop, StateData}.

%%-------------------------------------------------------------------------
%% @doc Start http backend
%% @end
%%-------------------------------------------------------------------------


http_stop(http_start, StateData) -> 
                % Start
                ok = application:start(simple_bridge), 
                {next_state, http_start, StateData}.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


handle_event(_, StateName, StateData) -> {next_state, StateName, StateData}.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


handle_sync_event(_, _From, StateName, StateData) -> {reply, unknown_event, StateName, StateData}.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


handle_info(_Info, StateName, StateData) -> {next_state, StateName, StateData}.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


terminate(Reason, StateName, StateData) ->
          application:stop(simple_bridge), % Ensure http is stopped
          Reason.



