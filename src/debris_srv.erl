%%%------------------------------------------------------------------------
%%% File:      debris_srv.erl
%%% @author    Eric Pailleau <debris@crownedgrouse.com>
%%% @copyright 2015 Eric Pailleau 
%%% @doc  
%%% debris' gen_server
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

check_admin_password(P, Seed) -> 
		{ok, AdminPasswdHash} = application:get_env(debris, admin_passwd) ,		
		Target = debris_lib:sha256_string(AdminPasswdHash ++ integer_to_list(Seed)),
%erlang:display(AdminPasswdHash),
%erlang:display({P, Seed}),
%erlang:display(Target),
		case (Target == P) of
			true -> true ;
			_    -> false
		end.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


start_link() -> start_link(undefined).

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


init(_Args) ->  % Signal to debris_app that he can continue
			    global:register_name(debris_srv, self()),
                debris_app ! debris_srv, 
                {ok, _Args}.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


terminate(_Reason, _Data) -> ok.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


handle_info(_Info, State) -> {noreply, State}.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
handle_cast({permit, P, Seed, '_', R, A, Perm}, State)-> 
		Name = debris_lib:open_dets(R),
		Users = dets:match(Name, {{user,'$1'},'_','_'}),
		dets:close(Name),
		lists:foreach(fun([U]) ->  gen_server:cast(self(), {permit, P, Seed, U, R, A, Perm}) end, Users),
		{noreply, State};		

handle_cast({permit, P, Seed, U, R, A, Perm}, State)-> 
		permit(P, Seed, U, R, A, Perm),
		{noreply, State};

handle_cast(Msg, State) -> {noreply, State}.

%%-------------------------------------------------------------------------
%% @doc Debris' gen_server call handling
%% @end
%%-------------------------------------------------------------------------
-spec handle_call({delete, list(), list() | {version, list()} | {codename, list()} }
                  | _
                 , _, _) -> tuple().
%%-------------------------------------------------------------------------
%% Administration
%%-------------------------------------------------------------------------

%% Set admin password (once). Will be compared to hash every time it is needed
handle_call({admin_passwd, Passwd}, {From, _}, State) -> 
        erlang:put(admin_passwd, debris_lib:sha256_string(Passwd)),
        {reply, ok, State};

%%-------------------------------------------------------------------------
%% Package addition
%%-------------------------------------------------------------------------

%% Add but do not update repo (usefull when needing to add many packages)
handle_call({add, Args}, _From, State) -> 
                        DebFile = proplists:get_value(debfile, Args),
                        Component = proplists:get_value(component, Args, "main"),
                        Rep  = debris_lib:add2pool(DebFile, Component),
                        {reply, Rep, State, hibernate};

%% Add and update repo (use for a single addition or last addition of many package)
handle_call({add_update, Args}, _From, State) -> 
                        DebFile = proplists:get_value(debfile, Args),
                        Component = proplists:get_value(component, Args, "main"),
                        ok  = debris_lib:add2pool(DebFile, Component),
                        Rep = debris_lib:update_repo(),
                        {reply, Rep, State, hibernate};

%%-------------------------------------------------------------------------
%% Repository update
%%-------------------------------------------------------------------------

handle_call(update, _From, State) -> 
                        Rep = debris_lib:update_repo(),
                        {reply, Rep, State, hibernate};

%%-------------------------------------------------------------------------
%%
%%-------------------------------------------------------------------------

handle_call({modify, Args}, _From, State) -> 
            {reply, ok, State, hibernate};

%%-------------------------------------------------------------------------
%% Enable / Disable HTTP access
%%    Start / Stop http layer, but not package management
%%    Usefull when many modification have to be done and 
%%    can result in inconsistencies for clients.
%%    TODO : instead stop http, sending a 'Under maintenance' page ?
%% 
%%-------------------------------------------------------------------------

handle_call({enable, http}, _From, State) -> 
            {reply, ok, State, hibernate};
handle_call({disable, http}, _From, State) -> 
            {reply, ok, State, hibernate};

%%-------------------------------------------------------------------------
%% Delete entry in dets' repo
%%    Any entry, either package, version or codename.
%%      Delete version only if you know what your are doing !
%%-------------------------------------------------------------------------

handle_call({delete, Repo, Key}, _From, State) -> 
            debris_lib:delete_deb(Repo, Key),
            {reply, ok, State, hibernate};

%%-------------------------------------------------------------------------
%% Sign detached
%%-------------------------------------------------------------------------

handle_call({sign_detached, Repo, Source, Target}, _From, State)-> 
        RepoA = case is_atom(Repo) of
                     true  -> Repo ;
                     false -> list_to_atom(Repo)
                end,
        Gpg_binary         = debris_lib:get_conf(RepoA, gpg_binary, "echo"),
        Gpg_user           = debris_lib:get_conf(RepoA, gpg_user, ""),
        Gpg_passphase_file = debris_lib:get_conf(RepoA, gpg_passphase_file, ""),
        Vars = [{gpg_binary, Gpg_binary}, 
                {gpg_user, Gpg_user}, 
                {gpg_passphase_file,Gpg_passphase_file},
                {source, Source}, {target, Target}],
        {ok, IOList} = debris_gpg_armor_detached_sign_dtl:render(Vars),
        % Execute
        os:cmd(string:strip(binary_to_list(iolist_to_binary(IOList)), right, $\n)),
        {reply, ok, State};

%%-------------------------------------------------------------------------
%% Sign attached
%%-------------------------------------------------------------------------

handle_call({sign_attached, Repo, Source, Target}, _From, State)-> 
        RepoA = case is_atom(Repo) of
                     true  -> Repo ;
                     false -> list_to_atom(Repo)
                end,
        Gpg_binary         = debris_lib:get_conf(RepoA, gpg_binary, "echo"),
        Gpg_user           = debris_lib:get_conf(RepoA, gpg_user, ""),
        Gpg_passphase_file = debris_lib:get_conf(RepoA, gpg_passphase_file, ""),
        Vars = [{gpg_binary, Gpg_binary}, 
                {gpg_user, Gpg_user}, 
                {gpg_passphase_file,Gpg_passphase_file},
                {source, Source}, {target, Target}],
        {ok, IOList} = debris_gpg_armor_clear_sign_dtl:render(Vars),
        % Execute
        os:cmd(string:strip(binary_to_list(iolist_to_binary(IOList)), right, $\n)),
        {reply, ok, State};

%%-------------------------------------------------------------------------
%% Export GPG public key
%%-------------------------------------------------------------------------

handle_call({export_pubkey, Repo, Target}, _From, State)-> 
        RepoA = case is_atom(Repo) of
                     true  -> Repo ;
                     false -> list_to_atom(Repo)
                end,
        Gpg_binary         = debris_lib:get_conf(RepoA, gpg_binary, "echo"),
        Gpg_user           = debris_lib:get_conf(RepoA, gpg_user, ""),
        Gpg_passphase_file = debris_lib:get_conf(RepoA, gpg_passphase_file, ""),
        Vars = [{gpg_binary, Gpg_binary}, 
                {gpg_user, Gpg_user}, 
                {gpg_passphase_file,Gpg_passphase_file},
                {target, Target}],
        {ok, IOList} = debris_gpg_export_pubkey_dtl:render(Vars),
        % Execute
        os:cmd(string:strip(binary_to_list(iolist_to_binary(IOList)), right, $\n)),
        {reply, ok, State};

%%-------------------------------------------------------------------------
%% Check if GPG signature is needed in repository
%%      A cache is used for performance.
%%      Cache is cleared each time current process restart.
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
                           true  -> exit("gpg_passphase_file MUST NOT be under document_root for obvious security reason !")
                      end,
                      erlang:put(signature_needed, true),
                      throw(true)
                catch 
                      throw:Term  -> {reply, Term, State};
                      exit:Reason -> application:stop(simple_bridge),
                                     {stop, {security, Reason}, false, State}
                end;

%%-------------------------------------------------------------------------
%% Manage users access and rights
%%-------------------------------------------------------------------------
% Loop over each repository
handle_call({permit, P, Seed, U, '_', A, Perm}, _From, State)-> 
		Repos = application:get_env(debris, repositories, []),
		lists:foreach(fun(R) ->  gen_server:cast(self(), {permit, P, Seed, U, R, A, Perm}) end, Repos),
		{reply, true, State};
% Loop over each user of a repository
handle_call({permit, P, Seed, '_', R, A, Perm}, _From, State)-> 
		Name = debris_lib:open_dets(R),
		Users = dets:match(Name, {{user,'$1'},'_','_'}),
		dets:close(Name),
		lists:foreach(fun([U]) ->  gen_server:cast(self(), {permit, P, Seed, U, R, A, Perm}) end, Users),
		{reply, true, State};
% Treat access and rights for one user and one repository
handle_call({permit, P, Seed, U, R, A, Perm}, _From, State)-> 
		Res = permit(P, Seed, U, R, A, Perm),
		{reply, Res, State};
%%-------------------------------------------------------------------------
%% Get info on users
%%-------------------------------------------------------------------------
% get info on any user of any repo
handle_call({info, P, Seed, '_', '_'}, _From, State)-> 
		Repos = application:get_env(debris, repositories, []),
		Res = lists:map(fun(R) -> Name = debris_lib:open_dets(R),
								  L1 = dets:match(Name, {{user, '$1'}, '$2', '$3'}),
                                  L = lists:map(fun([A, B, C]) -> {user, A, B, C} end, L1),
								 {repository, R, L } end, Repos),
		{reply, Res, State};
% get info on a user of any repo
handle_call({info, P, Seed, U, '_'}, _From, State)-> 
		Repos = application:get_env(debris, repositories, []),
		Res = lists:map(fun(R) -> Name = debris_lib:open_dets(R),
								  L1 = dets:match(Name, {{user, U}, '$1', '$2'}),
                                  L = lists:map(fun([A, B]) -> {user, U, A, B} end, L1),
								 {repository, R, L } end, Repos),
		{reply, Res, State};
% get info on any user of a repo
handle_call({info, P, Seed, '_', R}, _From, State)-> 
		Name = debris_lib:open_dets(R),
		Res = dets:match(Name, {{user, '$1'}, '$2', '$3'}),
        Res2 = lists:map(fun([A, B, C]) -> {user, A, B, C} end, Res),
		{reply, Res2, State};
% get info on a user of a repo
handle_call({info, P, Seed, U, R}, _From, State)-> 
		Name = debris_lib:open_dets(R),
		[[C, L]] = dets:match(Name, {{user, U}, '$1', '$2'}),
		{reply, [{user, U, C, L}], State};
%%-------------------------------------------------------------------------
%% Fallback
%%-------------------------------------------------------------------------

handle_call(_, _From, State) -> {reply, {error, function_clause}, State, hibernate}.

%%-------------------------------------------------------------------------
%% @doc Code change
%% @end
%%-------------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%********************* PRIVATE FUNCTIONS *********************************

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
permit(P, Seed, U, R, A, Perm) -> 
		% Verify password
		case check_admin_password(P, Seed) of
				true -> Name = debris_lib:open_dets(R),
						PermX = case Perm of
									 [] -> % Get current perms
										   [[PermXX]] = dets:match(Name, {{user, U}, '_', '$1'}),
										   case PermXX of
												[] -> "r" ;
										   		_  -> PermXX 
										   end;
									 _  -> Perm
								end,
						AX = case A of
									 [] -> % Get current access status
										   [[AXX]] = dets:match(Name, {{user, U}, '$1', '_'}),
										   case AXX  of
												[] -> false ;
										   		_  -> AXX 
										   end;
									 _  -> A
							 end,
						ok = dets:delete(Name, {user, U}),
						ok = dets:insert(Name, {{user, U}, AX, PermX}),
                        dets:sync(Name),
						dets:close(Name),
						true;
				_ -> false 
		  end.


