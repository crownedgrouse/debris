-module(debris_index).

-export([compile/2, get_index/3]).

-include_lib("kernel/include/file.hrl").

compile(Name, Path) -> {ok, Name} = erlydtl:compile(Path, Name),
                       Name.

get_index(Name, Path, RootPath) -> 
                {ok, A} = Name:render([
                                        {dir, "/" ++ Path},
                                        {parent, "/" ++ filename:dirname(Path)},
                                        get_entries(RootPath, Path)
                                      ]),
                %io:format("~p~n",[A]),
                A.

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

get_symbol_noext(F) -> case filename:basename(F) of
                            "README"    -> '&telrec;' ;
                            "InRelease" -> '&circledS;' ;
                            "Release"   -> '&cir;' ;
                            "Packages"  -> '&copysr;' ;
                            _           -> '&quest;' 

                       end.

% 18-Aug-2015 03:52
format_date(D) ->  erlydtl_dateformat:format(D, "d-M-Y H:i").

get_type(F) -> {ok, FileInfo} = file:read_file_info(F) ,
                FileInfo#file_info.type .

 

 
