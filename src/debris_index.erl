-module(debris_index).

-export([compile/2, get_index/3, get_index/4]).

-include_lib("kernel/include/file.hrl").

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
compile(Name, Path) -> {ok, Name} = erlydtl:compile(Path, Name),
                       Name.

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_index(Name, Path, RootPath) -> get_index(Name, Path, RootPath, []).
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_index(Name, Path, RootPath, Args) -> 
                Dir = case filename:basename(Path) of
                           "index.html" -> filename:dirname(Path) ;
                            _           -> Path
                end,
                {ok, A} = Name:render([
                                        {dir, "/" ++ Dir},
                                        {parent, "/" ++ filename:dirname(Path)},
                                        {n, 'A'},
                                        {m, 'D'},
                                        {s, 'D'},
                                        {d, 'D'},
                                        get_entries(RootPath, Path)
                                      ]),
                %io:format("~p~n",[A]),
                A.
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_entries(RootPath, Url) -> 
                     Path  = filename:join([RootPath, Url]),
                     RPath = case filelib:is_dir(Path) of
                                  true  -> Path ;
                                  false -> filename:dirname(Path) 
                             end,
                     RUrl  = case filelib:is_dir(Url) of
                                  true  -> Url ;
                                  false -> case filename:basename(Url) of
                                               "index.html" -> filename:dirname(Url) ;
                                                _           -> Url
                                           end
                             end,
                     %io:format("Url = ~p~nPath = ~p~nRPath = ~p~nRUrl = ~p~n", [Url, Path, RPath, RUrl]),
                     {ok, Filenames} = file:list_dir(RPath), 
                     E = lists:map(fun(H)-> F   = filename:join([RPath, H]),
                                                    [{name, filename:basename(H)}
                                                    ,{url, "/" ++ filename:join([RUrl, filename:basename(H)])}
                                                    ,{'size', filelib:file_size(F)}
                                                    ,{'date', format_date(filelib:last_modified(F))}
                                                    ,{symbol, get_symbol(F)}
                                                    ,{'type', get_type(F)}
                                                    ] end, Filenames),
%io:format("~p ~p~n",[Path, E]),

                     {entries, E}.
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_symbol(F) -> case filelib:is_dir(F) of
                      true  -> '&angrt;' ;
                      false -> 
                                case filename:extension(F) of
                                    ".deb" -> '&sdotb;' ;
                                    ".html"-> '&boxbox;' ;
                                    ".htm" -> '&boxbox;' ;
                                    ".gz"  -> '&zigrarr;' ;
                                    ".txt" -> '&tcy;' ;
                                    ".asc" -> '&bumpE;' ;
                                    ".gpg" -> '&bumpE;' ;
                                    _      ->  get_symbol_noext(F)
                                end
                 end.
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_symbol_noext(F) -> case filename:basename(F) of
                            "README"    -> '&telrec;' ;
                            "InRelease" -> '&circledS;' ;
                            "Release"   -> '&cir;' ;
                            "Packages"  -> '&copysr;' ;
                            _           -> '&quest;' 

                       end.
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
% 18-Aug-2015 03:52
format_date(D) ->  erlydtl_dateformat:format(D, "d-M-Y H:i").
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_type(F) -> {ok, FileInfo} = file:read_file_info(F) ,
                FileInfo#file_info.type .

 

 
