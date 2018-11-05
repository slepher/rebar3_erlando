-module(rebar3_erlando).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {namespace, erlando},
                                 {bare, false},
                                 {deps, ?DEPS},
                                 {example, "rebar3 erlando compile"},
                                 {short_desc, "Compile erlando style typeclasses."},
                                 {desc, "Compile erlydtl templates."},
                                 {opts, []}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

do(State) ->
    CurrentApp = rebar_state:current_app(State),
    AppInfos = rebar_state:project_apps(State),
    case is_umbrella_app(CurrentApp) or is_project_app(CurrentApp, AppInfos) of
        true ->
            rebar_api:info("Running erlando compile...", []),
            Deps = rebar_state:deps_to_build(State),
            AllAppInfos = Deps ++ AppInfos,
            case lists:filter(
                   fun(AppInfo) ->
                           Name = rebar_app_info:name(AppInfo),
                           Name == <<"erlando">>
                   end, AllAppInfos) of
                [ErlandoApp] ->
                    CompileState = 
                        lists:foldl(
                          fun(AppInfo, Acc) ->
                                  case match_modules(AppInfo) of
                                      {ok, {Typeclasses, Types, ModuleMap}} ->
                                          rebar3_erlando_compile:add_modules(Typeclasses, Types, ModuleMap, Acc);
                                      {error, _Reason} ->
                                          Acc
                                  end
                          end, rebar3_erlando_compile:new(), AllAppInfos),
                    {ok, _Module, Bin} = rebar3_erlando_compile:compile(CompileState),
                    OutDir = rebar_app_info:out_dir(ErlandoApp),
                    io:format("outdir is ~p~n", [OutDir]),
                    ok = file:write_file(filename:join(OutDir, "ebin/typeclass.beam"), Bin),
                    {ok, State};
                [] ->
                    io:format("erlando app is not included in project, why use rebar3_erlando to compile?~n"),
                    {ok, State}
            end;
        false ->
            {ok, State}
    end.

is_umbrella_app(undefined) ->
    true;
is_umbrella_app(_) ->
    false.

is_project_app(undefined, _) ->
    false;
is_project_app(AppInfo, [ProjectAppInfo]) ->
    Name = rebar_app_info:name(AppInfo),
    ProjectName = rebar_app_info:name(ProjectAppInfo),
    Name == ProjectName;
is_project_app(_AppInfo, _ProjectAppInfos) ->
    false.

match_modules(AppInfo) ->
    OutDir = rebar_app_info:out_dir(AppInfo),
    Fun = fun(Beamfile, {TypeclassesAcc, TypesAcc, ModulesAcc}) ->
                  case beam_lib:chunks(Beamfile, [attributes]) of
                      {ok, {Module, [{attributes, Attributes}]}} ->
                          AttrKeys = lists:map(fun(E) -> element(1, E) end, Attributes),
                          NTypeclassesAcc = 
                              case lists:member(superclass, AttrKeys) of
                                  true ->
                                      [Module|TypeclassesAcc];
                                  false ->
                                      TypeclassesAcc
                              end,
                          NTypesAcc = 
                              case lists:member(erlando_type, AttrKeys) of
                                  true ->
                                      [{Module, Attributes}|TypesAcc];
                                  false ->
                                      TypesAcc
                              end,
                          {NTypeclassesAcc, NTypesAcc, maps:put(Module, Beamfile, ModulesAcc)};
                      {error, _Reason} ->
                          {TypeclassesAcc, TypesAcc, ModulesAcc}
                  end
          end,
    rebar3_erlando_file:fold_beams(Fun, {[], [], maps:new()}, filename:join(OutDir, "ebin")).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
