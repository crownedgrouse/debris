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
                % Sort order
                SortRaw = case Args of
                            [{<<"C">>,C},{<<"O">>,O}] -> {C, O} ;
                            _                         -> {<<"N">>, <<"A">>}
                          end, 
                Sort = sort_keys(SortRaw),               
                {ok, A} = Name:render([
                                        {dir, "/" ++ Dir},
                                        {parent, "/" ++ filename:dirname(Path)},
                                        {n, flip(name, Sort)},
                                        {m, flip(last_m, Sort)},
                                        {s, flip('size', Sort)},
                                        {d, flip(desc, Sort)},
                                        get_entries(RootPath, Path, Sort)
                                      ]),
                %io:format("~p~n",[Args]),
                A.
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
sort_keys({A, B}) when is_binary(A) -> Bnew = list_to_atom(binary_to_list(B)),
                                      case A of
                                          <<"N">> -> {name, Bnew};
                                          <<"M">> -> {last_m, Bnew};
                                          <<"S">> -> {'size', Bnew};
                                          <<"D">> -> {desc, Bnew};
                                          _       -> {name, Bnew}
                                      end.
%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
flip(K, {K1, O1}) when (K =:= K1) -> case O1 of
                                            'A' -> 'D' ;
                                            _   -> 'A'
                                     end;
flip(_, {_, _}) -> 'A' .

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
get_entries(RootPath, Url, Sort) -> 
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
                                                    ,{'last_m', filelib:last_modified(F)}
                                                    ,{symbol, get_symbol(F)}
                                                    ,{'type', get_type(F)}
                                                    ,{desc, ""}
                                                    ] end, Filenames),
                    {K, O} = Sort,
                    S = fun(A, B) ->  % pick A, B values
                                        {K, ValA} = lists:keyfind(K, 1, A),
                                        {K, ValB} = lists:keyfind(K, 1, B),
                                        case (ValA > ValB) of
                                             false -> false ;
                                             true  -> true
                                        end
                        end,
                    %io:format("~p ~p~n",[Path, E]),
                    Es = lists:sort(S, E),
                    %io:format("Sort ~p~n",[Es]),
                    Eso = case O of
                                'D' -> lists:reverse(Es) ;
                                _   -> Es 
                          end,
                    %io:format("Sort o ~p~n",[Eso]),
                    {entries, Eso}.
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

 

 
