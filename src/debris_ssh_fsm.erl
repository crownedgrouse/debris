%%%------------------------------------------------------------------------
%%% File:      debris_ssh_fsm.erl
%%% @author    Eric Pailleau <debris@crownedgrouse.com>
%%% @copyright 2015 Eric Pailleau 
%%% @doc  
%%% debris' ssh fsm
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

-module(debris_ssh_fsm).

-behaviour(gen_fsm).

-export([start_link/0]).
-export([ssh_start/2, ssh_stop/2]).
-export([init/1, terminate/3]).

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


init(_) -> gen_fsm:send_event(self(), ssh_start),
           {ok, ssh_stop, []}.


%%-------------------------------------------------------------------------
%% @doc Stop http backend
%% @end
%%-------------------------------------------------------------------------


ssh_start(ssh_stop, StateData) -> 
                ssh:stop(),
                {next_state, ssh_stop, StateData}.

%%-------------------------------------------------------------------------
%% @doc Start http backend
%% @end
%%-------------------------------------------------------------------------


ssh_stop(ssh_start, StateData) -> 
    % Starting ssh ?
    State = case application:get_env(debris, ssh) of
                undefined -> Ref= [], ssh_stop ;
                {ok, true} -> 
                       ssh:start(),
                       SDir = application:get_env(debris, ssh_system_dir, filename:join(code:priv_dir(debris),"etc/ssh")),
                       %
                       application:set_env(stdlib, shell_catch_exception, false),
                       application:set_env(stdlib, restricted_shell, debris_restshell),

                       filelib:ensure_dir(filename:join(SDir, "fakedir")),
                       Sshopt = [{auth_methods, "publickey"}
                                ,{system_dir, SDir}
                                ,{preferred_algorithms, ssh:default_algorithms()}
                                %,{shell, {debris_restshell, start, []}}
                                ], % TODO
                       % TODO verify that port is free
                       {ok, Ref} = ssh:daemon(application:get_env(debris, ssh_port, 2222), Sshopt),
                       ssh_start ;
                {ok,_} -> Ref= [], ssh_stop
            end,
    {next_state, State, Ref}.

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
          ssh:stop(), % Ensure ssh is stopped
          Reason.

