-module(debris_lib).

-export([extract_control/1, search_field/2, init_repo/0,init_repo/1,init_repo/2, 
         update_repo/0, update_repo/1, update_repo/2, add2pool/2, add2pool/3]).

-define(JOIN(X,Y), filename:join(X, Y)).

-define(PRINT(X), io:format("~p~n",[X])).

-define(DEBUG(X), io:format("~p~n",[X]), X).

-define(DEFAULT_REPO, ["debian"]).

-define(DEFAULT_SUITES, ["stable", "testing", "unstable"]).

%% Create default repos
% dists/stable/
% dists/testing/
% dists/unstable/

%% Content of dists/x/    x=Archive
% Contents-amd64.gz                                
% Contents-i386.gz                              
% Contents-powerpc.gz                             
% Contents-sparc.gz                                    
% InRelease                                           
% Release                                             
% Release.gpg  
% main/

%% Content of dists/x/y/   y=component  
%                  -
% binary-all/                                        
% binary-amd64/                                    
% binary-i386/                                    
% binary-powerpc/                                    
% binary-ppc64el/                                    
% binary-sparc/ 

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
init_repo() -> RootDir = get_rootdir(),
               init_repo(RootDir).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
init_repo(RootDir, Repo) -> PrivDir = code:priv_dir(debris),
                            RepoDir = filename:join([PrivDir, "repos", Repo]),
                            ok = filelib:ensure_dir(?JOIN(RepoDir, "fakedir")), 
                            % Attic directories for very old packages not anymore exposed - debris never remove a package !
                            ok = filelib:ensure_dir(filename:join([RepoDir, "attic", "fakedir"])),
                            init_repo(?JOIN(RootDir, Repo)).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
init_repo(RootDir) when is_tuple(RootDir) -> {DocRoot, List} = RootDir,
                                             lists:foreach(fun(R) -> init_repo(DocRoot, R) end, List);

init_repo(RootDir) when is_list(RootDir) ->
         % Ensure directory
         ok = filelib:ensure_dir(?JOIN(RootDir,"fakedir")),
         Repo = list_to_atom(filename:basename(RootDir)),
         % Create dists and pool
         Dists = ?JOIN(RootDir, "dists"),
         Pool  = ?JOIN(RootDir, "pool"),
         ok = filelib:ensure_dir(?JOIN(Dists,"fakedir")),
         ok = filelib:ensure_dir(?JOIN(Pool,"fakedir")),
         % Suites
         Suites = get_conf(Repo, suites, ?DEFAULT_SUITES),
         DSuites = dir_combine({dir, Dists}, dir_escape(Suites)),
         % Components 
         Components = get_conf(Repo, components, ["main"]),
         % Create in pool/
         lists:foreach(fun(C) -> ok = filelib:ensure_dir(filename:join([Pool, C, "fakedir"])) end , Components),
         % Create in dists/
         Dirs = dir_combine(dir_escape(DSuites), dir_escape(Components)),
         Subs = get_subs(),
         Finals = dir_combine(dir_escape(Dirs), dir_escape(Subs)),
         lists:foreach(fun(X) -> ok = filelib:ensure_dir(?JOIN(X, "fakedir")),
                                 create_arch_release_file(RootDir, X)
                       end , Finals),
         lists:foreach(fun(D) ->  create_archive_release_file(RootDir, D) end,  Suites),
         % Create css for repo
         do_repo_index_css(RootDir),         
         % Create index.html for repo
         do_repo_index_html(RootDir),
         ok.

%%-------------------------------------------------------------------------
%% @doc Find which css to create in top of repo
%% @end
%%-------------------------------------------------------------------------
do_repo_index_css(RootDir)  -> % Check if there is a custom repo.css in private repo directory
                               Repo    = filename:basename(RootDir),
                               PrivDir = code:priv_dir(debris),
                               RepoDir = filename:join([PrivDir, "repos", Repo]),
                               Base    = filename:join([PrivDir, "www", "css", "base.css"]),
                               Custom  = ?JOIN(RepoDir, Repo ++ ".css"),
                               case filelib:is_regular(Custom) of
                                    true  -> create_repo_index_css(Custom, RootDir) ;
                                    false -> create_repo_index_css(Base, RootDir)
                               end.
%%-------------------------------------------------------------------------
%% @doc Copy css
%% @end
%%-------------------------------------------------------------------------
create_repo_index_css(Source, RootDir) -> 
                               DocRoot = filename:dirname(RootDir),
                               Repo    = filename:basename(RootDir),
                               Dir     = filename:join([DocRoot, "css"]),
                               ok      = filelib:ensure_dir(?JOIN(Dir, "fakedir")), 
                               Target  = filename:join([DocRoot, "css", Repo ++ ".css"]),
                               {ok, _} = file:copy(Source, Target).


%%-------------------------------------------------------------------------
%% @doc Find which index.html to create in top of repo
%% @end
%%-------------------------------------------------------------------------
do_repo_index_html(RootDir) -> % Check if there is a custom index.html in private repo directory
                               Repo    = filename:basename(RootDir),
                               PrivDir = code:priv_dir(debris),
                               RepoDir = filename:join([PrivDir, "repos", Repo]),
                               Base    = filename:join([PrivDir, "www", "base.html"]),
                               Custom  = ?JOIN(RepoDir, "index.html"),
                               case filelib:is_regular(Custom) of
                                    true  -> create_repo_index_html(Custom, RootDir) ;
                                    false -> create_repo_index_html(Base, RootDir)
                               end.


%%-------------------------------------------------------------------------
%% @doc Create index.html in top of repo
%% @end
%%-------------------------------------------------------------------------
create_repo_index_html(Source, RootDir) -> 
            Target  = ?JOIN(RootDir, "index.html"),
            Repo    = filename:basename(RootDir),
            Module  = list_to_atom(Repo ++ "_index_html"),
            Suites  = get_conf(list_to_atom(Repo), suites, ?DEFAULT_SUITES),
            Compos  = get_conf(list_to_atom(Repo), components, ["main"]),

            {ok, Module} = erlydtl:compile_file(Source, Module),
            Vars = get_template_vars() ++ [ {repository, Repo}
                                           ,{pubkey, Repo ++ ".asc"}
                                           ,{suites, Suites}
                                           ,{components, Compos}
                                          ],
            case Module:render(Vars) of
                 {ok, IOList} -> HTML = binary_to_list(iolist_to_binary(IOList)),
                                 ok = file:write_file(Target, HTML) ;
                 {error, Err} ->  exit("Unable to compile "++ Source ++ " due to errors : " ++ Err)
            end.
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_template_vars() -> {ok, Host} = inet:gethostname(),
                       {ok, FQDN} = net_adm:dns_hostname(Host),
                        URL = "http://" ++ application:get_env(debris, fqdn, FQDN),
                       [{url, URL}].

%%-------------------------------------------------------------------------
%% @doc Get the document_root
%% @end
%%-------------------------------------------------------------------------
get_docroot() -> % Use Debris Private Dir by default
                 application:load(debris),
                 RootDir1 = case code:priv_dir(debris) of
                              {error, bad_name} -> "." ;
                              P -> ?JOIN(P, "www")
                           end,
                 % Get rootdir from config
                 case application:get_env(debris, document_root) of
                    {ok, RootDir2} -> RootDir2 ;
                    undefined      -> RootDir1
                 end.
%%-------------------------------------------------------------------------
%% @doc Get the root directory of a repo (one level more than document_root)
%% @end
%%-------------------------------------------------------------------------
get_rootdir() -> add_repo(get_docroot()).

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
add_repo(RootDir) ->  case application:get_env(debris, repositories, "debian") of
                           "debian" -> ?JOIN(RootDir, "debian") ;
                            List    -> {RootDir, List}
                      end.
                        

%%-------------------------------------------------------------------------
%% @doc Add Deb file to the default pool
%% @end
%%-------------------------------------------------------------------------  
add2pool(DebFile, Component) -> add2pool(DebFile, "debian", Component).

%%-------------------------------------------------------------------------
%% @doc Add Deb file to the pool
%% @end
%%-------------------------------------------------------------------------           

add2pool(DebFile, Repo, Component) -> 
                DocRoot = get_docroot(),
                RootDir = ?JOIN(DocRoot, Repo),
                % Get the package name (BTW verify it is a valid package)
                PackName = search_field(extract_control(DebFile), "Package"),
                % Get first character of pack name
                [H | _] = PackName,
                % Compose directory name
                Dir = filename:join([RootDir, "pool", Component, [H], PackName]),
                BaseName = filename:basename(DebFile),
                ok = filelib:ensure_dir(?JOIN(Dir,"fakedir")),
                Target = ?JOIN(Dir, BaseName),
                % Delete file if already exists
                file:delete(Target),
                % Move file to pool
                file:rename(DebFile, Target).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_subs() -> ["binary-all", "binary-amd64","binary-arm64", "binary-armel",
               "binary-armhf", "binary-hurd-i386", "binary-i386", "binary-kfreebsd-amd64",
               "binary-kfreebsd-i386", "binary-mips", "binary-mipsel", "binary-powerpc", 
               "binary-ppc64el", "binary-s390x", "binary-sparc", "source"].
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_archs() -> ["amd64","arm64", "armel",
               "armhf", "hurd-i386", "i386", "kfreebsd-amd64",
               "kfreebsd-i386", "mips", "mipsel", "powerpc", 
               "ppc64el", "s390x", "sparc"].

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_archdir_name("all")  -> "binary-all";
get_archdir_name("amd64")  -> "binary-amd64";
get_archdir_name("arm64")  -> "binary-arm64";
get_archdir_name("armel")  -> "binary-armel";
get_archdir_name("armhf")  -> "binary-armhf";
get_archdir_name("hurd-i386")  -> "binary-hurd-i386";
get_archdir_name("i386") -> "binary-i386";
get_archdir_name("kfreebsd-amd64")  -> "binary-kfreebsd-amd64";
get_archdir_name("kfreebsd-i386")  -> "binary-kfreebsd-i386";
get_archdir_name("mips")  -> "binary-mips";
get_archdir_name("mipsel")  -> "binary-mipsel";
get_archdir_name("powerpc") -> "binary-powerpc";
get_archdir_name("ppc64el") -> "binary-ppc64el";
get_archdir_name("s390x")  -> "binary-s390x";
get_archdir_name("sparc") -> "binary-sparc";
get_archdir_name("source") -> "source";
get_archdir_name(_)     -> "unknown".

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_arch_name("binary-all")  -> "all";
get_arch_name("binary-amd64")  -> "amd64";
get_arch_name("binary-arm64")  -> "arm64";
get_arch_name("binary-armel")  -> "armel";
get_arch_name("binary-armhf")  -> "armhf";
get_arch_name("binary-hurd-i386")  -> "hurd-i386";
get_arch_name("binary-i386") -> "i386";
get_arch_name("binary-kfreebsd-amd64")  -> "kfreebsd-amd64";
get_arch_name("binary-kfreebsd-i386")  -> "kfreebsd-i386";
get_arch_name("binary-mips")  -> "mips";
get_arch_name("binary-mipsel")  -> "mipsel";
get_arch_name("binary-powerpc") -> "powerpc";
get_arch_name("binary-ppc64el") -> "ppc64el";
get_arch_name("binary-s390x")  -> "s390x";
get_arch_name("binary-sparc") -> "sparc";
get_arch_name("source") -> "source";
get_arch_name(_)     -> "unknown".

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
create_arch_release_file(RootDir, Dir) -> % Guess infos 
                                          N = erlang:length(filename:split(RootDir)),
                                          S = filename:split(Dir),
                                          {Left, Right} = lists:split(N, S),
                                          %?PRINT({Left, Right}),
                                          ["dists", Archive, Compo, ArchDir] = Right,
                                          Arch = get_arch_name(ArchDir),
                                          % Create content
                                          Data =  "Archive: " ++ Archive ++ "\n" ++
                                                  "Origin: " ++ "" ++ "\n" ++
                                                  "Label: " ++ "" ++ "\n" ++
                                                  "Component: " ++ Compo ++ "\n" ++
                                                  "Architecture: " ++ Arch ++ "\n",
                                          ok = file:write_file(?JOIN(Dir, "Release"), Data).
                                          
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
%% Release file content
% Origin: Debian
% Label: Debian
% Suite: unstable
% Codename: sid
% Date: Sat, 14 May 2011 08:20:50 UTC
% Valid-Until: Sat, 21 May 2011 08:20:50 UTC
% Architectures: alpha amd64 armel hppa hurd-i386 i386 ia64 kfreebsd-amd64 kfreebsd-i386 mips mipsel powerpc s390 sparc
% Components: main contrib non-free
% Description: Debian x.y Unstable - Not Released
% MD5Sum:
%  bdc8fa4b3f5e4a715dd0d56d176fc789 18876880 Contents-alpha.gz
%  9469a03c94b85e010d116aeeab9614c0 19441880 Contents-amd64.gz
%  3d68e206d7faa3aded660dc0996054fe 19203165 Contents-armel.gz
create_archive_release_file(RootDir, Dir) ->   
                   N = erlang:length(filename:split(filename:join([RootDir, "dists", Dir]))),
                   Repo = list_to_atom(filename:basename(RootDir)),
                   %Components = application:get_env(debris, components, ["main"]),
                   Components = get_conf(Repo, components, ["main"]),
                   CurDir = filename:join([RootDir, "dists", Dir]),
                   {ok, FilelistRaw} = rec_list_dir(CurDir, true),
                   % Remove any existing signatures, must not be part of content
                   lists:foreach(fun(X) -> case filename:basename(X) of
                                                "Release.gpg" -> file:delete(X) ;
                                                "InRelease"   -> file:delete(X) ;
                                                _             -> ok
                                           end end, FilelistRaw) ,
                   {ok, Filelist} = rec_list_dir(CurDir, true),
                   % Size of each file
                   Tuplelist = lists:flatmap(fun(X) -> {Left, Right} = lists:split(N , filename:split(X)),
                                                       {ok, B} = file:read_file(X),
                                                       M = md5sum_string(B),
                                                       S1 = sha1sum_string(B), 
                                                       S256 = sha256_string(B),
                                                       [{filelib:file_size(X), filename:join(Right), M, S1, S256}] end, lists:sort(Filelist)),
                   % Sort list and take the bigger one to know size string length
                   Tuplelist2 = lists:keysort(1, Tuplelist),
                   {S, _, _, _, _} = lists:last(Tuplelist2),
                   SizeSize = erlang:length(integer_to_list(S)),
                   % Create content
                   Data =  "Origin: " ++ "todo" ++ "\n" ++
                           "Label: " ++ "todo" ++ "\n" ++
                           "Suite: " ++ Dir ++ "\n" ++
                           "Codename: " ++ "todo" ++ "\n" ++
                           "Date: " ++ httpd_util:rfc1123_date(erlang:universaltime()) ++ "\n" ++
                           "Valid-Until: " ++ valid_until() ++ "\n" ++
                           "Architectures: " ++ string:join(get_archs(), " ") ++ "\n" ++
                           "Components: " ++ string:join(Components, " ") ++ "\n" ++
                           "Description: " ++ "todo" ++ "\n" ++
                           "MD5Sum: \n" ++ lists:flatmap(fun({Size, Rel, MD5, _, _}) -> 
                             [io_lib:format(" ~s ~"++integer_to_list(SizeSize)++"s ~s~n", 
                                            [MD5, integer_to_list(Size), Rel]) 
                             ]  end , Tuplelist) ++
                           "SHA1: \n" ++ lists:flatmap(fun({Size, Rel, _, SHA1, _}) -> 
                             [io_lib:format(" ~s ~"++integer_to_list(SizeSize)++"s ~s~n", 
                                            [SHA1, integer_to_list(Size), Rel]) 
                             ]  end , Tuplelist) ++
                           "SHA256: \n" ++ lists:flatmap(fun({Size, Rel, _, _, SHA256}) -> 
                             [io_lib:format(" ~s ~"++integer_to_list(SizeSize)++"s ~s~n", 
                                            [SHA256, integer_to_list(Size), Rel]) 
                             ]  end , Tuplelist),
                    Source = ?JOIN(CurDir, "Release"),
                    ok = file:write_file(Source, Data),
                    signatures(Source).

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------

valid_until() -> httpd_util:rfc1123_date({{2035,7,21},{9,29,42}}).


%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
signatures(Target) -> case gen_server:call(debris_srv, signature_needed ) of
                            true  -> detached_sign(Target),
                                     clear_sign(Target);
                            false -> ok
                      end.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
detached_sign(Source) -> gen_server:call(debris_srv, {sign_detached, Source, Source ++ ".gpg" }) .

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
clear_sign(Source)   -> gen_server:call(debris_srv, {sign_attached, Source, ?JOIN(filename:dirname(Source), "InRelease")}) .

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
update_repo() -> RootDir = get_rootdir(),
                 update_repo(RootDir).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
update_repo(RootDir, Repo) -> update_repo(?JOIN(RootDir, Repo)).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
update_repo(RootDir) when is_tuple(RootDir) -> {DocRoot, List} = RootDir,
                                               lists:foreach(fun(R) -> update_repo(?JOIN(DocRoot,R)) end, List);

%%-------------------------------------------------------------------------
%% @doc 
%% Update de whole repo from files in pool/
%% @end
%%-------------------------------------------------------------------------
update_repo(RootDir) when is_list(RootDir) -> 
                        % export pubkey in document_root
                        Repo = filename:basename(RootDir),
                        case gen_server:call(debris_srv, signature_needed ) of
                             false -> ok ;
                             true  -> gen_server:call(debris_srv, {export_pubkey, ?JOIN(RootDir, Repo ++ ".asc")} )
                        end,
                        % List all .deb in pool/x/y/z
                        {ok, L} = rec_list_dir(?JOIN(RootDir, "pool"), true),
                        Debs = lists:filter(fun(D) -> case filename:extension(D) of
                                                           ".deb" -> true ;
                                                           _      -> false
                                                      end
                                            end, lists:sort(L)),
                        % ?PRINT(Debs)
                        % For each .deb update Packages.gz by updating Packages.new
                        lists:foreach(fun(P) -> update_packages_new(RootDir, P) end, Debs),

                        % Search all Packages.new and replace Packages and Packages.gz 
                        {ok, N} = rec_list_dir(?JOIN(RootDir, "dists"), true),
                        News = lists:filter(fun(D) -> case filename:extension(D) of
                                                           ".new" -> true ;
                                                           _      -> false
                                                      end
                                            end, lists:sort(N)),
                        lists:foreach(fun(G) -> Gz = ?JOIN(filename:dirname(G), "Packages.gz"),
                                                {ok, Content} = file:read_file(G),
                                                Compressed = zlib:gzip(Content),
                                                ok = file:write_file(Gz, Compressed),
                                                ok = file:rename(G, ?JOIN(filename:dirname(G), "Packages"))
                                      end, News),
                        % Update Release file in Suites
                        %Suites = application:get_env(debris, suites, ?DEFAULT_SUITES),
                        Suites = get_conf(list_to_atom(Repo), suites, ?DEFAULT_SUITES),
                        lists:foreach(fun(D) ->  create_archive_release_file(RootDir, D) end,  Suites),
                        ok.
                        
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
update_packages_new(RootDir, P) when is_list(RootDir), is_list(P)
                                -> N = erlang:length(filename:split(RootDir)),
                                   S = filename:split(P),
                                   {Left, Right} = lists:split(N, S),
                                   ["pool", Compo, _, App, Deb] = Right,
                                   %?PRINT({Compo, App, Deb, filename:join(Right)})
                                   Repo = filename:basename(RootDir),
                                   % Search if .deb file already known in an Archive (otherwise unstable is returned)
                                   Archive = what_archive(Repo, Deb),
                                   % Update Packages.new in right Archive
                                   update_packages_new(RootDir, {Archive, Compo, App, Deb, filename:join(Right)});

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------    
update_packages_new(RootDir, P) when is_tuple(P) 
                                -> {Archive, Compo, App, Deb, File} = P ,
                                   % Get file infos
                                   Infos = packages_gz_entry(RootDir, File),
                                   Arch  = search_field(Infos, "Architecture"),
                                   ArchDir = get_archdir_name(Arch),
                                   % Compose targets
                                   Packages = filename:join([RootDir, "dists", Archive, Compo, ArchDir, "Packages"]),
                                   Target = filename:join([RootDir, "dists", Archive, Compo, ArchDir, "Packages.new"]),
                                   Gz     = filename:join([RootDir, "dists", Archive, Compo, ArchDir, "Packages.gz"]),
                                   % If .new exists and older than .gz, delete it 
                                   case filelib:last_modified(Target) < filelib:last_modified(Gz) of
                                        true  -> file:delete(Target);
                                        false -> ok
                                   end,
                                   % Append fileinfos in .new
                                   %?PRINT(Target),
                                   {ok, IoDevice} = file:open(Target, [append]),
                                   ok = file:write(IoDevice, Infos).

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
% Lookup in dets file if .deb known  (otherwise unstable is returned)
% Be aware if you do not use "unstable" in Suites config key !
what_archive(Repo, Deb) ->  Name = open_dets(Repo),
                      % Search Deb file as key and return archive value, otherwise "unstable"
                      case dets:lookup(Name, Deb) of
                            {error, _} -> "" ;
                            []         -> ok = record_deb(Repo, Deb, {"unstable", "main", calendar:local_time()}),
                                          what_archive(Repo, Deb) ;
                            [Val]      -> {_, {Archive, _Component, _Dates}} = Val,
                                           Archive
                      end.
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
record_deb(Repo, Deb, {Archive, Component, Date}) -> 
                 Name = open_dets(Repo),
                 % Insert the entry
                 insert_deb(Name, Deb, {Archive, Component, Date}),
                 % Search all same entries and delete those before Date (not those in futur)
                 Entries = dets:lookup(Name, Deb),
                 lists:foreach(fun({Deb, {A, C, D}}) -> case ( D < Date ) of
                                                             true  -> dets:delete(Name, {Deb, A, C, D});
                                                             false -> ok
                                                        end end, Entries),
                 dets:close(Name).

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
delete_deb(Repo, Deb) -> Name = open_dets(Repo),
                   dets:delete(Name, Deb),
                   dets:close(Name).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
insert_deb(Repo, Deb, Val) 
           when is_list(Repo)-> Name = open_dets(Repo),
                                insert_deb(Name, Deb, Val),
                                dets:close(Name);

insert_deb(Name, Deb, Val)
          when is_atom(Name) -> dets:insert(Name, {Deb, Val}).                
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
open_dets(Repo) ->     % Dets file must be in priv directory
                   PrivDir = case code:priv_dir(debris) of
                                    {error, bad_name} -> "." ;
                                    P -> P
                             end,
                   Dets = filename:join([PrivDir, "repos", Repo , Repo ++ ".dets"]),
                   {ok, Name} = dets:open_file(list_to_atom(Repo), [{type, bag},
                                                          {file, Dets}]),
                   Name.
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
%% Content of dists/x/y/binary-z/ 
% Packages.gz
% Release 

%% Packages.gz : Concatenated control files + below info per control file
% Filename: pool/main/0/0ad-data/0ad-data_0.0.18-1_all.deb
% Size: 575509610
% MD5sum: 39e848b72494d3335a54ab88a71ab8fc
% SHA1: 9fe32480379a6c0bd4e9d52483c02e97dce6e1e2
% SHA256: 1d349f9cd956b2f34e2bb192343b6a6cd6bb18cd465dd5c92e5ff332700811e0

packages_gz_entry(RootDir, File) -> FilePath = filename:join(RootDir, File),
                                    binary_to_list(extract_control(FilePath)) ++ control_end(RootDir, File)++ "\n".
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
control_end(RootDir, File) ->  FilePath = filename:join(RootDir, File),
                               {ok, S} = file:read_file(FilePath),
                               filename_string(File) ++ size_string(FilePath) ++ checksums(S).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
filename_string(Filename) -> "Filename: " ++ Filename ++ "\n".
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
size_string(File) -> "Size: " ++ integer_to_list(filelib:file_size(File)) ++ "\n".
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
checksums(S) -> "MD5sum: " ++ md5sum_string(S) ++ "\n"
                "SHA1: "   ++ sha1sum_string(S) ++ "\n"
                "SHA256: " ++ sha256_string(S) ++ "\n".
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
md5sum_string(S) ->  hash_string(crypto:hash(md5,S)).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
sha1sum_string(S) -> hash_string(crypto:hash(sha,S)).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
sha256_string(S) -> hash_string(crypto:hash(sha256,S)).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
hash_string(X) -> lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(X)]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
extract_control(File) ->  C = case edgar:extract(File,[memory]) of
                            {ok, [{"debian-binary   ", _},
                                  {"control.tar.gz  ", C0},
                                  {_, _}]}                   -> C0 ;
                            {ok, [{"debian-binary", _},
                                  {"control.tar.gz", C1},
                                  {_, _}]}                   -> C1 
                              end,
                          {ok, L} = erl_tar:extract({binary,C}, [memory,compressed]),
                          case proplists:is_defined("./control", L) of
                               true  -> proplists:get_value("./control", L, "") ;
                               false -> case proplists:is_defined("control", L) of
                                            true  -> proplists:get_value("control", L, "") ;
                                            false -> throw("control file not found") 
                                        end
                          end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
search_field(Control, Field) -> {ok, F} = swab:sync([{grab, escape_re(Field) ++ ":(.*)"}, {trim,both}], Control),
                                F. 

% Escape '-' character for regexp
escape_re(S) -> S. % TODO

%%------------------------------------------------------------------------------
%% @doc Return debian package name from control file
%% @end
%%------------------------------------------------------------------------------
guess_deb_name(F) -> % Extract name, version , architecture from control file
                     Control = binary_to_list(extract_control(F)),
                     Name    = search_field(Control, "Package"),
                     Version = search_field(Control, "Version"),
                     Archi   = search_field(Control, "Architecture"),
                     lists:flatten(Name ++ "_" ++ Version ++ "_" ++ Archi ++ ".deb").

%%------------------------------------------------------------------------------
%% @doc List files recursively
%% @end
%%------------------------------------------------------------------------------
-type name() :: string() | atom() | binary().

-spec rec_list_dir(Dir::name()) ->
        {ok, [string()]} | {error, atom()}.
        
rec_list_dir(Dir) ->
    rec_list_dir(Dir, false). % default : files and dirs
 
-spec rec_list_dir(Dir::name(), FilesOnly::boolean()) ->
        {ok, [string()]} | {error, atom()}.
 
rec_list_dir(Dir, FilesOnly) ->
    case filelib:is_file(Dir) of
        true ->
            case filelib:is_dir(Dir) of
                true -> {ok, rec_list_dir([Dir], FilesOnly, [])};
                false -> {error, enotdir}
            end;
        false -> {error, enoent}
    end.
 
rec_list_dir([], _FilesOnly, Acc) -> Acc;
rec_list_dir([Path|Paths], FilesOnly, Acc) ->
    rec_list_dir(Paths, FilesOnly,
        case filelib:is_dir(Path) of
            false -> [Path | Acc];
            true ->
                {ok, Listing} = file:list_dir(Path),
                SubPaths = [filename:join(Path, Name) || Name <- Listing],
                rec_list_dir(SubPaths, FilesOnly,
                    case FilesOnly of
                        true -> Acc;
                        false -> [Path | Acc]
                    end)
        end).

%%------------------------------------------------------------------------------
%% @doc Get configuration parameter
%% If Repo is an atom, a search of a custom entry is done, if not found,
%% return a global config parameter if set, otherwise Default value is returned.
%% @end
%%------------------------------------------------------------------------------
get_conf(Repo, Key, Default) when is_atom (Repo) -> 
            case application:get_env(debris, Repo) of
                 undefined -> get_conf(atom_to_list(Repo), Key, Default) ;
                 {ok, Val} -> proplists:get_value(Key, Val, get_conf(atom_to_list(Repo), Key, Default))
            end;

get_conf(Repo, Key, Default) -> application:get_env(debris, Key, Default).

%%------------------------------------------------------------------------------
%% @doc Escape directories D into {dir, D}
%% Otherwise issue with lists:flatten (all directories concatenated)
%% @end
%%------------------------------------------------------------------------------
dir_escape(D) -> lists:map(fun(Dir) -> {dir, Dir} end, D).

%%------------------------------------------------------------------------------
%% @doc Combine two lists of directories
%% First call must escape both directory lists with dir_escape/1 .
%% @end
%%------------------------------------------------------------------------------
dir_combine({dir, {dir, Left}}, Right) -> dir_combine({dir, Left}, Right) ; 

dir_combine(Left, {dir, {dir, Right}}) -> dir_combine(Left, {dir, Right}) ; 

dir_combine({dir, Left}, {dir, Right}) -> {dir, ?JOIN(Left, Right)} ;

dir_combine({dir, Left}, Right) -> lists:map(fun(R) -> dir_combine({dir, Left},  {dir, R}) end, Right) ;

dir_combine(Left, {dir, Right}) -> lists:map(fun({dir, L}) -> ?JOIN(L, Right) end, Left) ;

dir_combine(Left, Right) ->  Raw = lists:flatten(lists:map(fun(L) -> dir_combine(L, Right) end, Left)),
                             lists:map(fun({dir, D}) -> D end, Raw).






