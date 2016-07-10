%%%------------------------------------------------------------------------
%%% File:      debris_app.erl
%%% @author    Eric Pailleau <debris@crownedgrouse.com>
%%% @copyright 2015 Eric Pailleau 
%%% @doc  
%%% Main application entry point
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
-module(debris_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([config_change/3]).

start(_Type, _Args) ->
    % Replace admin_passwd by its SHA256 hash
    application:load(debris),
    ok = application:set_env(debris, admin_passwd, debris_lib:sha256_string(application:get_env(debris, admin_passwd,"")), [{persistent, true}]),
    % Register
    erlang:register(debris_app, self()),
	Ret = debris_sup:start_link(),
    % init and Update repo 
    debris_index:compile(debris_index_tpl, filename:join([code:priv_dir(debris),"web/index.html"])),
    % Wait for debris_srv to start
    receive
        debris_srv -> ok 
    after 60000    -> exit("debris_srv not started after timeout")
    end,
    debris_lib:init_repo(),
    debris_lib:update_repo(),
    Ret.

stop(_State) ->
	ok.

% TODO
config_change(Changed, New, Removed) ->
	% Check if priv_dir is changed, added or removed
    _C = lists:keymember(priv_dir, 1, Changed),
	_A = lists:keymember(priv_dir, 1, New),
	_R = lists:keymember(priv_dir, 1, Removed),
	% If added, check if 'legacy' or path different than ~/.debris

    % If removed

	% If changed
	
	ok.

% TODO
migrate_data(_From, _To) ->  
	% Do a tar from old version data
	% Extract to new directory
	ok.

