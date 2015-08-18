-module(debris_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Ret = debris_sup:start_link(),
    % init and Update repo 
    debris_index:compile(debris_index_tpl, filename:join([code:priv_dir(debris),"www/index.html"])),
    timer:sleep(500),
    debris_lib:init_repo(),
    debris_lib:update_repo(),
    Ret.

stop(_State) ->
	ok.
