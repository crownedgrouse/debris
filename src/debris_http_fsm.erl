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

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    Address       = application:get_env(debris, address, "0.0.0.0"),
    Port          = application:get_env(debris, port, 8000),
    DefRootDir    = filename:join(code:priv_dir(debris), "www"),
    DocRoot       = application:get_env(debris, document_root, DefRootDir),
    ok            = filelib:ensure_dir(filename:join(DocRoot, "fakedir")),
    Backend       = application:get_env(debris, backend, inets),
    Repos         = case Backend of
                         cowboyxxx ->  error_logger:info_msg("Forcing static directories for cowboy~n~n", []),
                                    application:get_env(debris, repositories, ["debian"]) ;
                         _      -> []
                    end,

    % Create static_paths mainly for cowboy dispatch rules to work well
    Static        = lists:flatten(lists:map(fun(X) -> 
                                        lists:map(fun(Y) -> {a, filename:join([X, Y]) ++ "/"}  % keep trailing / !
                                                  end, ["dists","pool"])                                                  
                                  end, Repos)),
    Statics       = lists:map(fun({a, Z}) -> Z end, Static),
    % Should the http server be started ?
    Ret = case Backend of
           none -> ignore ;
           _    -> 
                    % Override simple_bridge config variables
                    ok = application:load(simple_bridge),
                    lists:foreach(fun({Par, Val}) -> ok = application:set_env(simple_bridge, Par, Val, [{persistent, true}]) end,
                                [{address, Address}, 
                                 {port, Port}, 
                                 {document_root, DocRoot}, 
                                 {backend, Backend}, 
                                 {static_paths, Statics},
                                 {handler, debris_handler}]),
                    ok = application:start(simple_bridge), 
                    {ok, http_start, #http{ backend=Backend
                                           ,address=Address
                                           ,port=Port
                                           ,document_root=DocRoot}}
    end,
    Ret.

http_start(http_stop, StateData) -> 
                % Start
                ok = application:stop(simple_bridge), 
                {ok, http_stop, StateData}.

http_stop(http_start, StateData) -> 
                % Stop
                ok = application:start(simple_bridge), 
                {ok, http_start, StateData}.

handle_event(_, StateName, StateData) -> {next_state, StateName, StateData}.

handle_sync_event(_, _From, StateName, StateData) -> {reply, unknown_event, StateName, StateData}.

handle_info(_Info, StateName, StateData) -> {next_state, StateName, StateData}.

terminate(Reason, StateName, StateData) ->
          application:stop(simple_bridge), % Ensure http is stopped
          Reason.



