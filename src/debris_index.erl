%%%------------------------------------------------------------------------
%%% File:      debris_index.erl
%%% @author    Eric Pailleau <debris@crownedgrouse.com>
%%% @copyright 2015 Eric Pailleau 
%%% @doc  
%%% index.html creation library
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
-module(debris_index).

-export([compile/2, get_index/3, get_index/4]).

-include_lib("kernel/include/file.hrl").

%%-------------------------------------------------------------------------
%% @doc Compile a template and return name 
%% @end
%%-------------------------------------------------------------------------
-spec compile(atom(),binary() | string() | {'dir',atom() | binary() | [atom() | [any()] | char()]} | {'file',atom() | binary() | [atom() | [any()] | char()]} | {'template',binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | [])}) -> atom().
 
compile(Name, Path) -> %{ok, Name} = erlydtl:compile(Path, Name),
                       Name.

%%-------------------------------------------------------------------------
%% @doc Render index.html, no args
%% @end
%%-------------------------------------------------------------------------
-spec get_index(atom() | tuple(),[atom() | [any()] | char()],[atom() | [any()] | char()], _) -> any().

get_index(Name, Path, RootPath) -> get_index(Name, Path, RootPath, []).

%%-------------------------------------------------------------------------
%% @doc Render index.html with args
%% @end
%%-------------------------------------------------------------------------
-spec get_index(atom() | tuple(),[atom() | [any()] | char()],[atom() | [any()] | char()]) -> any().

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
                A.
%%-------------------------------------------------------------------------
%% @doc Transcode sort keys from binary to tuple
%% @end
%%-------------------------------------------------------------------------
-spec sort_keys(tuple()) -> tuple().

sort_keys({A, B}) when is_binary(A) -> Bnew = list_to_atom(binary_to_list(B)),
                                      case A of
                                          <<"N">> -> {name, Bnew};
                                          <<"M">> -> {last_m, Bnew};
                                          <<"S">> -> {'size', Bnew};
                                          <<"D">> -> {desc, Bnew};
                                          _       -> {name, Bnew}
                                      end.
%%-------------------------------------------------------------------------
%% @doc Flip sort order for url links in index.html
%%      'A' for ascending, 'D' for descending
%% @end
%%-------------------------------------------------------------------------
-spec flip(atom(), {atom(), atom()}) -> atom(). 

flip(K, {K1, O1}) when (K =:= K1) -> case O1 of
                                            'A' -> 'D' ;
                                            _   -> 'A'
                                     end;
flip(_, {_, _}) -> 'A' .

%%-------------------------------------------------------------------------
%% @doc Return files list infos with specified sort order
%% @end
%%-------------------------------------------------------------------------
-spec get_entries(list(), list(), tuple()) ->  {entries, list()}.

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
                    Es = lists:sort(S, E),
                    Eso = case O of
                                'D' -> lists:reverse(Es) ;
                                _   -> Es 
                          end,
                    {entries, Eso}.
%%-------------------------------------------------------------------------
%% @doc Return html5 symbols linked to file suffixes
%% @end
%%-------------------------------------------------------------------------
-spec get_symbol(list()) -> atom().

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
%% @doc Return html5 symbols linked to some file without suffixes
%% @end
%%-------------------------------------------------------------------------
-spec get_symbol_noext(list()) -> atom().

get_symbol_noext(F) -> case filename:basename(F) of
                            "README"    -> '&telrec;' ;
                            "InRelease" -> '&circledS;' ;
                            "Release"   -> '&cir;' ;
                            "Packages"  -> '&copysr;' ;
                            _           -> '&quest;' 

                       end.
%%-------------------------------------------------------------------------
%% @doc Format to usual format in index.html
%% Default format to : 18-Aug-2015 03:52
%% @end
%%-------------------------------------------------------------------------
-spec format_date(tuple()) -> list().

format_date(D) ->  erlydtl_dateformat:format(D, "d-M-Y H:i").

%%-------------------------------------------------------------------------
%% @doc Return file type
%% @end
%%-------------------------------------------------------------------------
-spec get_type(list()) -> atom().

get_type(F) -> {ok, FileInfo} = file:read_file_info(F) ,
                FileInfo#file_info.type .

 
