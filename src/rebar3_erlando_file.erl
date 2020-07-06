%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(rebar3_erlando_file).

-include_lib("kernel/include/file.hrl").

%% API
-export([fold_beams/3]).
-export([ensure_dir/1]).

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

ensure_dir(Dir) ->
    ensure_dir(Dir, 5).

ensure_dir(Dir, 0) ->
    {error, {read_dir_link_exceed, Dir}};
ensure_dir(Dir, N) ->
    case file:read_file_info(Dir) of
        {ok, #file_info{type = directory}} ->
            ok;
        {ok, #file_info{type = symlink}} ->
            case file:read_link(Dir) of
                {ok, Dir1} ->
                    ensure_dir(Dir1, N - 1);
                {error, Reason} ->
                    {error, {read_link_failed, Dir, Reason}}
            end;
        {ok, #file_info{type = Type}} ->
            {error, {invalid_file_type, Dir, Type}};
        {error, enoent} ->
            file:make_dir(Dir);
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
