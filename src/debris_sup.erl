-module(debris_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Start children
	Procs = [
               {debris_srv, {debris_srv, start_link, []}, permanent, brutal_kill, worker, [debris_srv]}
              ,{debris_http_fsm, {debris_http_fsm, start_link, []}, permanent, brutal_kill, worker, [debris_http_fsm]}
            ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
