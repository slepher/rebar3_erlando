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
    App = rebar_state:current_app(State),
    AppName = rebar_app_info:name(App),
    AppInfos = rebar_state:project_apps(State),
    Deps = rebar_state:deps_to_build(State),
    AllAppInfos = Deps ++ AppInfos,
    case lists:filter(
           fun(AppInfo) ->
                   Name = rebar_app_info:name(AppInfo),
                   Name == <<"erlando">>
           end, AllAppInfos) of
        [ErlandoApp] ->
            case App of
                undefined ->
                    clear_erlando_state();
                _ ->
                    AppName = rebar_app_info:name(App),
                    rebar_api:info("Running erlando compile for ~s...", [AppName]),
                    ErlandoState = get_erlando_state(),
                    NErlandoState = 
                        case match_modules(State, App) of
                            {ok, {Typeclasses, Types, ModuleMap}} ->
                                rebar3_erlando_compile:add_modules(Typeclasses, Types, ModuleMap, ErlandoState);
                            {error, _Reason} ->
                                ErlandoState
                        end,
                    {ok, _Module, Bin} = rebar3_erlando_compile:compile(NErlandoState),
                    update_erlando_state(NErlandoState),
                    OutDir = rebar_app_info:out_dir(ErlandoApp),
                    ok = file:write_file(filename:join(OutDir, "ebin/typeclass.beam"), Bin),
                    case is_project_app(App, AppInfos) of
                        true ->
                            clear_erlando_state();
                        false ->
                            ok
                    end,
                    {ok, State}
            end;
        [] ->
            case AppName of
                <<"astranaut">> ->
                    ok;
                _ ->
                    rebar_api:warn("erlando app is not included in project, why use rebar3_erlando to compile?", [])
            end,
            {ok, State}
    end.

get_erlando_state() ->
    StateFile =  "erlando.state",
    case filelib:is_file("erlando.state") of
        true ->
            case file:consult(StateFile) of
                {ok, [State]} ->
                    State;
                {error, Reason} ->
                    rebar_api:info("consult ~s failed ~p", [StateFile, Reason]),
                    rebar3_erlando_compile:new()
            end;
        false ->
            rebar3_erlando_compile:new()
    end.

update_erlando_state(ErlandoState) ->
    StateFile =  "erlando.state",
    Spec = io_lib:format("~p.\n", [ErlandoState]),
    ok = rebar_file_utils:write_file_if_contents_differ(StateFile, Spec, utf8).

clear_erlando_state() ->
    StateFile =  "erlando.state",
    case filelib:is_file(StateFile) of
        true ->
            file:delete(StateFile);
        false ->
            ok
    end.

is_project_app(AppInfo, [ProjectAppInfo]) ->
    Name = rebar_app_info:name(AppInfo),
    ProjectName = rebar_app_info:name(ProjectAppInfo),
    Name == ProjectName;
is_project_app(_AppInfo, _ProjectAppInfos) ->
    false.

match_modules(State, AppInfo) ->
    OutDir = rebar_app_info:out_dir(AppInfo),
    Profiles = rebar_state:current_profiles(State),
    Fun = fun(Beamfile, {TypeclassesAcc, TypesAcc, ModulesAcc}) ->
                  case beam_lib:chunks(Beamfile, [attributes]) of
                      {ok, {Module, [{attributes, Attributes}]}} ->
                          AttrKeys = lists:map(fun(E) -> element(1, E) end, Attributes),
                          ErlandoBehaviours = proplists:get_value(erlando_future_behaviour, Attributes, []),
                          NTypeclassesAcc = 
                              case lists:member(superclass, AttrKeys) of
                                  true ->
                                      [Module|TypeclassesAcc];
                                  false ->
                                      TypeclassesAcc
                              end,
                          NTypeclassesAcc1 = ErlandoBehaviours ++ NTypeclassesAcc,
                          NTypesAcc = 
                              case lists:member(erlando_type, AttrKeys) of
                                  true ->
                                      [{Module, Attributes}|TypesAcc];
                                  false ->
                                      TypesAcc
                              end,
                          {NTypeclassesAcc1, NTypesAcc, maps:put(Module, Beamfile, ModulesAcc)};
                      {error, _Reason} ->
                          {TypeclassesAcc, TypesAcc, ModulesAcc}
                  end
          end,
    case rebar3_erlando_file:fold_beams(Fun, {[], [], maps:new()}, filename:join(OutDir, "ebin")) of
        {ok, Result} ->
            case lists:member(test, Profiles) of
                true ->
                    rebar3_erlando_file:fold_beams(Fun, Result, filename:join(OutDir, "test"));
                false ->
                    {ok, Result}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
