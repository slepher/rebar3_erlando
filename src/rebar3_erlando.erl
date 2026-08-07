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
    Deps = rebar_state:all_deps(State),
    AllAppInfos = Deps ++ AppInfos,
    case lists:filter(
           fun(AppInfo) ->
                   Name = rebar_app_info:name(AppInfo),
                   Name == <<"erlando">>
           end, AllAppInfos) of
        [ErlandoApp] ->
            case App of
                undefined ->
                    ok;
                _ ->
                    rebar_api:info("Running erlando compile for ~s...", [AppName]),
                    case match_modules(State, AllAppInfos, AppInfos) of
                        {ok, {Typeclasses, Types, ModuleMap}} ->
                            ErlandoState =
                                rebar3_erlando_compile:add_modules(
                                  Typeclasses, Types, ModuleMap,
                                  rebar3_erlando_compile:new()),
                            write_beam(AppName, ErlandoState, ErlandoApp);
                        {error, _Reason} ->
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

write_beam(<<"astranaut">>, _ErlandoState, _ErlandoApp) ->
    ok;
write_beam(_AppName, ErlandoState, ErlandoApp) ->
    {ok, _Module, Bin} = rebar3_erlando_compile:compile(ErlandoState),
    OutDir = rebar_app_info:out_dir(ErlandoApp),
    ok = file:write_file(filename:join(OutDir, "ebin/typeclass.beam"), Bin).

is_project_app(AppInfo, ProjectAppInfos) ->
    Name = rebar_app_info:name(AppInfo),
    lists:any(
      fun(ProjectAppInfo) ->
              rebar_app_info:name(ProjectAppInfo) =:= Name
      end, ProjectAppInfos).

match_modules(State, AllAppInfos, ProjectAppInfos) ->
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
    lists:foldl(
      fun(AppInfo, Acc) ->
              case Acc of
                  {error, _Reason} ->
                      Acc;
                  {ok, InnerAcc} ->
                      OutDir = rebar_app_info:out_dir(AppInfo),
                      IsProjectApp = is_project_app(AppInfo, ProjectAppInfos),
                      fold_app(Fun, InnerAcc, OutDir, IsProjectApp, Profiles)
              end
      end, {ok, {[], [], maps:new()}}, AllAppInfos).

fold_app(Fun, Acc, OutDir, IsProjectApp, Profiles) ->
    case rebar3_erlando_file:fold_beams(Fun, Acc, filename:join(OutDir, "ebin")) of
        {ok, Acc1} ->
            case test_dir(OutDir, IsProjectApp, Profiles) of
                {ok, TestDir} ->
                    case rebar3_erlando_file:fold_beams(Fun, Acc1, TestDir) of
                        {ok, Acc2} ->
                            {ok, Acc2};
                        {error, enoent} ->
                            {ok, Acc1};
                        {error, _Reason} = Error ->
                            Error
                    end;
                error ->
                    {ok, Acc1}
            end;
        {error, enoent} ->
            {ok, Acc};
        {error, _Reason} = Error ->
            Error
    end.

test_dir(_OutDir, false, _Profiles) ->
    error;
test_dir(OutDir, true, Profiles) ->
    case lists:member(test, Profiles) of
        true ->
            TestDir = filename:join(OutDir, "test"),
            case filelib:is_dir(TestDir) of
                true ->
                    {ok, TestDir};
                false ->
                    error
            end;
        false ->
            error
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
