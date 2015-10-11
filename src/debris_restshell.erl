-module(debris_restshell).

%-export([start/0, stop/0]).
-export([local_allowed/3, non_local_allowed/3]).

local_allowed(_,_,State) ->
    {false,State}.

non_local_allowed({gen_server, 'call'},[debris_srv, _],State) ->
    {true,State};
non_local_allowed(_,_,State) ->
    {false,State}.

%start() -> Pid = shell:start(true, true),
%           group_leader(Pid, Pid),
%           shell:start_restricted(?MODULE),
%           Pid.


%stop() -> shell:stop_restricted().
