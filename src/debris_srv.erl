-module(debris_srv).

-export([start_link/0,
         start_link/1,
         init/1,
         terminate/2,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         code_change/3]).

-behaviour(gen_server).

-define(PRINT(X), io:format("~p~n",[X])).

start_link() -> start_link(undefined).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(_Args) -> {ok, _Args}.

terminate(_Reason, _Data) -> ok.

handle_info(_Info, State) -> {noreply, State}.

handle_cast(Msg, State) -> {noreply, State}.
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
handle_call({add, Args}, _From, State) -> 
                        DebFile = proplists:get_value(debfile, Args),
                        Component = proplists:get_value(component, Args, "main"),
                        Rep  = debris_lib:add2pool(DebFile, Component),
                        {reply, Rep, State, hibernate};

handle_call({add_update, Args}, _From, State) -> 
                        DebFile = proplists:get_value(debfile, Args),
                        Component = proplists:get_value(component, Args, "main"),
                        ok  = debris_lib:add2pool(DebFile, Component),
                        Rep = debris_lib:update_repo(),
                        {reply, Rep, State, hibernate};

handle_call(update, _From, State) -> 
                        Rep = debris_lib:update_repo(),
                        {reply, Rep, State, hibernate};

handle_call({modify, Args}, _From, State) -> 
                                        {reply, ok, State, hibernate};
handle_call({enable, Args}, _From, State) -> 
                                        {reply, ok, State, hibernate};
handle_call({disable, Args}, _From, State) -> 
                                        {reply, ok, State, hibernate};
handle_call({delete, Args}, _From, State) -> 
                                        {reply, ok, State, hibernate};

handle_call({sign_detached, Source, Target}, _From, State)-> 
        {ok, Gpg_binary} = application:get_env(debris, gpg_binary),
        {ok, Gpg_user} = application:get_env(debris, gpg_user),
        {ok, Gpg_passphase_file} = application:get_env(debris, gpg_passphase_file),
        Vars = [{gpg_binary, Gpg_binary}, 
                {gpg_user, Gpg_user}, 
                {gpg_passphase_file,Gpg_passphase_file},
                {source, Source}, {target, Target}],
        {ok, IOList} = debris_gpg_armor_detached_sign_dtl:render(Vars),
        % Execute
        os:cmd(string:strip(binary_to_list(iolist_to_binary(IOList)), right, $\n)),
        {reply, ok, State};

handle_call({sign_attached, Source, Target}, _From, State)-> 
        {ok, Gpg_binary} = application:get_env(debris, gpg_binary),
        {ok, Gpg_user} = application:get_env(debris, gpg_user),
        {ok, Gpg_passphase_file} = application:get_env(debris, gpg_passphase_file),
        Vars = [{gpg_binary, Gpg_binary}, 
                {gpg_user, Gpg_user}, 
                {gpg_passphase_file,Gpg_passphase_file},
                {source, Source}, {target, Target}],
        {ok, IOList} = debris_gpg_armor_clear_sign_dtl:render(Vars),
        % Execute
        os:cmd(string:strip(binary_to_list(iolist_to_binary(IOList)), right, $\n)),
        {reply, ok, State};

handle_call({export_pubkey, Target}, _From, State)-> 
        {ok, Gpg_binary} = application:get_env(debris, gpg_binary),
        {ok, Gpg_user} = application:get_env(debris, gpg_user),
        {ok, Gpg_passphase_file} = application:get_env(debris, gpg_passphase_file),
        Vars = [{gpg_binary, Gpg_binary}, 
                {gpg_user, Gpg_user}, 
                {gpg_passphase_file,Gpg_passphase_file},
                {target, Target}],
        {ok, IOList} = debris_gpg_export_pubkey_dtl:render(Vars),
        % Execute
        os:cmd(string:strip(binary_to_list(iolist_to_binary(IOList)), right, $\n)),
        {reply, ok, State};
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
handle_call(signature_needed, _From, State)-> 
                try
                      % Return cached value if exists
                      case erlang:get(signature_needed) of
                           undefined -> skip ; % i.e check at each start
                           false     -> throw(false) ;
                           true      -> throw(true)
                      end,
                      DocRoot = application:get_env(debris, document_root, ""),
                      Gpg_binary = application:get_env(debris, gpg_binary, ""),
                      Gpg_user = application:get_env(debris, gpg_user, ""),
                      Gpg_passphase_file = application:get_env(debris, gpg_passphase_file, ""),
                      % 
                      case Gpg_binary of
                           "" -> erlang:put(signature_needed, false),
                                 throw(false) ;
                           _  -> skip
                      end,
                      % Check it is an absolute path
                      case filename:pathtype(Gpg_binary) of
                           absolute -> ok ;
                           _        -> exit("gpg_binary must be an absolute path !")
                      end,
                      % Check if gpg_binary exists
                      case filelib:is_regular(Gpg_binary) of
                           true  -> ok ;
                           false -> exit("gpg_binary not found or not a regular file !")
                      end,
                      % Check other variables 
                      case Gpg_user of
                           "" -> exit("gpg_user missing or empty. A user/keyid is needed !");
                           _  -> ok
                      end, 
                      case filelib:is_regular(Gpg_passphase_file) of
                           true  -> ok ;
                           false -> exit("gpg_passphase_file not found or not a regular file !")
                      end,
                      % Check if passphrase is under document_root (fatal !)
                      % Using absolute names, not a total guarantee, but ...
                      DC = filename:split(filename:absname(DocRoot)),
                      PP = filename:split(filename:absname(Gpg_passphase_file)),
                      case lists:prefix(DC, PP) of
                           false -> ok ;
                           true  -> exit("gpg_passphase_file MUST not be under document_root for obvious security reason !")
                      end,
                      erlang:put(signature_needed, true),
                      throw(true)
                catch 
                      throw:Term  -> {reply, Term, State};
                      exit:Reason -> application:stop(simple_bridge),
                                     {stop, {security, Reason}, false, State}
                end;

handle_call(_, _From, State) -> {reply, {error, function_clause}, State, hibernate}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

