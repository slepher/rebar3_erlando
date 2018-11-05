%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(rebar3_erlando_file).

%% API
-export([fold_beams/3]).

%%%===================================================================
%%% API
%%%===================================================================
fold_beams(Fun, Init, Path) ->
    case file:list_dir(Path) of
        {ok, Filenames} ->
            {ok, lists:foldl(
                   fun(Filename, Acc) ->
                           case filename:extension(Filename) of
                               ".beam" ->
                                   Fun(filename:join(Path, Filename), Acc);
                               _ ->
                                   Acc
                           end
                   end, Init, Filenames)};
        {error, Reason} ->
            {error, Reason}
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
