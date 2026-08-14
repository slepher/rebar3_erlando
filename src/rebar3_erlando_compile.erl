%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(rebar3_erlando_compile).

%% API
-export([new/0, add_modules/4, compile/1]).

-record(state, {
    otp_version,
    behaviour_modules = maps:new(),
    typeclasses = [],
    type_aliases = [],
    types = maps:new(),
    exported_types = sets:new(),
    mod_recs = dict:new(),
    beamfiles = maps:new()
}).

-record(cache, {
    types = maps:new(),
    mod_recs = {mrecs, dict:new()}
}).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    #state{}.

add_modules([], [], _ModuleMap, State) ->
    State;
add_modules(Typeclasses, Types, ModuleMap, State) ->
    NState = lists:foldl(fun add_typeclass/2, State, Typeclasses),
    NNState = maps:fold(fun add_module/3, NState, ModuleMap),
    lists:foldl(fun add_instance/2, NNState, Types).

add_typeclass(Module, #state{typeclasses = Typeclasses} = State) ->
    NTypeclasses = ordsets:add_element(Module, Typeclasses),
    State#state{typeclasses = NTypeclasses}.

add_module(Module, Beamfile, #state{exported_types = ETypes, mod_recs = ModRecs} = State) ->
    State1 = State#state{beamfiles = maps:put(Module, Beamfile, State#state.beamfiles)},
    case get_core_from_beam(Beamfile) of
        {ok, {OtpVersion, Core, AbsOrCore}} ->
            {NETypes, NModRecs} =
                update_types_and_rec_map(Module, Core, AbsOrCore, ETypes, ModRecs),
            State1#state{exported_types = NETypes, mod_recs = NModRecs, otp_version = OtpVersion};
        {error, _Reason} ->
            State1
    end.

% add typeclass first
% then add type
add_instance(
    {Module, Attributes},
    #state{
        behaviour_modules = BehaviourModules,
        typeclasses = Typeclasses,
        types = Types,
        exported_types = ETypes,
        mod_recs = ModRecs,
        otp_version = OtpVersion,
        beamfiles = Beamfiles
    } = State
) ->
    validate_module(Module, Attributes, Typeclasses, Beamfiles),
    {TypeInstanceMap, TypeBehaviourModuleMap} =
        module_type_info(Module, Attributes, Typeclasses, ETypes, ModRecs, OtpVersion),
    NTypes = merge_type_instance(Types, TypeInstanceMap),
    NBehaviourModules = merge_behaviour_modules(BehaviourModules, TypeBehaviourModuleMap),
    State#state{behaviour_modules = NBehaviourModules, types = NTypes}.

validate_module(Module, Attributes, Typeclasses, Beamfiles) ->
    DeclaredBehaviours = lists:usort(behaviours(Attributes) ++ erlando_behaviours(Attributes)),
    InstanceBehaviours =
        [
            Behaviour
         || Behaviour <- DeclaredBehaviours,
            ordsets:is_element(Behaviour, Typeclasses)
        ],
    validate_instance_metadata(
        Module, Attributes, Typeclasses, DeclaredBehaviours, Beamfiles
    ),
    Exports = module_exports(Module, Beamfiles),
    lists:foreach(
        fun(Behaviour) ->
            Required = required_callbacks(Behaviour, Beamfiles),
            Missing = Required -- Exports,
            case Missing of
                [] -> ok;
                _ -> validation_error({missing_callbacks, Module, Behaviour, Missing})
            end
        end,
        InstanceBehaviours
    ),
    validate_gen_fun_capabilities(Module, Attributes, DeclaredBehaviours).

validate_instance_metadata(
    Module, Attributes, Typeclasses, DeclaredBehaviours, Beamfiles
) ->
    case instance_metadata(Module, Attributes) of
        legacy ->
            ok;
        {metadata, Instances} ->
            DeclaredTypes = lists:usort(metadata_type_names(types(Attributes))),
            lists:foreach(
                fun(Instance) ->
                    Type = maps:get(type, Instance),
                    Typeclass = maps:get(typeclass, Instance),
                    case lists:member(Type, DeclaredTypes) of
                        true ->
                            ok;
                        false ->
                            validation_error(
                                {undeclared_metadata_type, Module, Type}
                            )
                    end,
                    case
                        ordsets:is_element(Typeclass, Typeclasses) andalso
                            lists:member(Typeclass, DeclaredBehaviours)
                    of
                        true ->
                            ok;
                        false ->
                            validation_error(
                                {undeclared_metadata_capability, Module, Typeclass}
                            )
                    end,
                    validate_dispatch_instance(Module, Instance, Beamfiles)
                end,
                Instances
            )
    end.

validate_dispatch_instance(
    Module,
    #{
        implementation := dispatch,
        type := Type,
        typeclass := Typeclass
    } = Instance,
    Beamfiles
) ->
    Dispatch = maps:get(dispatch, Instance, #{}),
    Functions = module_functions(Module, Beamfiles),
    lists:foreach(
        fun({Callback, Arity}) ->
            case maps:find(Callback, Dispatch) of
                error ->
                    validation_error(
                        {missing_dispatch_callback, Module, Type, Typeclass, {Callback, Arity}}
                    );
                {ok, {Function, Arity}} ->
                    case lists:member({Function, Arity}, Functions) of
                        true ->
                            ok;
                        false ->
                            validation_error(
                                {missing_dispatch_adapter, Module, Type, Typeclass,
                                    {Callback, Arity}, {Function, Arity}}
                            )
                    end;
                {ok, Adapter} ->
                    validation_error(
                        {invalid_dispatch_adapter, Module, Type, Typeclass, {Callback, Arity},
                            Adapter}
                    )
            end
        end,
        required_callbacks(Typeclass, Beamfiles)
    );
validate_dispatch_instance(_Module, _Instance, _Beamfiles) ->
    ok.

module_functions(Module, Beamfiles) ->
    Beamfile = module_beamfile(Module, Beamfiles),
    case beam_lib:chunks(Beamfile, [abstract_code]) of
        {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            [{Name, Arity} || {function, _, Name, Arity, _} <- Forms];
        {ok, {Module, [{abstract_code, no_abstract_code}]}} ->
            validation_error({cannot_validate_dispatch, Module, no_abstract_code});
        {error, beam_lib, Reason} ->
            validation_error({cannot_validate_dispatch, Module, Reason})
    end.

instance_metadata(Module, Attributes) ->
    Values = attribute_values(erlando_instance_meta, Attributes),
    case Values of
        [] ->
            legacy;
        _ ->
            Metadata = [normalize_instance_metadata(Module, Value) || Value <- Values],
            {metadata, lists:append([maps:get(instances, Item, []) || Item <- Metadata])}
    end.

normalize_instance_metadata(Module, {1, Metadata}) when is_map(Metadata) ->
    case maps:get(module, Metadata, Module) of
        Module ->
            Metadata;
        OtherModule ->
            validation_error(
                {instance_metadata_module_mismatch, Module, OtherModule}
            )
    end;
normalize_instance_metadata(Module, {Version, _Metadata}) ->
    validation_error({unsupported_instance_metadata_version, Module, Version});
normalize_instance_metadata(Module, Metadata) ->
    validation_error({invalid_instance_metadata, Module, Metadata}).

attribute_values(Name, Attributes) ->
    lists:append(
        [
            case Value of
                Values when is_list(Values) -> Values;
                _ -> [Value]
            end
         || Value <- proplists:get_all_values(Name, Attributes)
        ]
    ).

metadata_type_names(TypeAttrs) ->
    [
        case Type of
            {Name, _UsedTypes} -> Name;
            Name when is_atom(Name) -> Name
        end
     || Type <- TypeAttrs
    ].

module_exports(Module, Beamfiles) ->
    Beamfile = module_beamfile(Module, Beamfiles),
    case beam_lib:chunks(Beamfile, [exports]) of
        {ok, {Module, [{exports, Exports}]}} ->
            Exports;
        {error, beam_lib, Reason} ->
            validation_error({cannot_read_exports, Module, Reason})
    end.

required_callbacks(Behaviour, Beamfiles) ->
    Beamfile = module_beamfile(Behaviour, Beamfiles),
    case callbacks_from_abstract_code(Behaviour, Beamfile) of
        {ok, {Callbacks, OptionalCallbacks}} ->
            lists:usort(Callbacks -- OptionalCallbacks);
        {error, no_abstract_code} ->
            callbacks_from_loaded_behaviour(Behaviour)
    end.

callbacks_from_abstract_code(Behaviour, Beamfile) ->
    case beam_lib:chunks(Beamfile, [abstract_code]) of
        {ok, {Behaviour, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Callbacks =
                [Function || {attribute, _, callback, {Function, _}} <- Forms],
            Optional =
                lists:append(
                    [Functions || {attribute, _, optional_callbacks, Functions} <- Forms]
                ),
            {ok, {Callbacks, Optional}};
        {ok, {Behaviour, [{abstract_code, no_abstract_code}]}} ->
            {error, no_abstract_code};
        {error, beam_lib, Reason} ->
            validation_error({cannot_read_callbacks, Behaviour, Reason})
    end.

callbacks_from_loaded_behaviour(Behaviour) ->
    try Behaviour:behaviour_info(callbacks) of
        Callbacks ->
            Optional =
                try Behaviour:behaviour_info(optional_callbacks) of
                    OptionalCallbacks when is_list(OptionalCallbacks) -> OptionalCallbacks;
                    _ -> []
                catch
                    _:_ -> []
                end,
            lists:usort(Callbacks -- Optional)
    catch
        _:_ -> validation_error({cannot_read_callbacks, Behaviour, no_abstract_code})
    end.

module_beamfile(Module, Beamfiles) ->
    case maps:find(Module, Beamfiles) of
        {ok, Beamfile} -> Beamfile;
        error -> validation_error({missing_beamfile, Module})
    end.

validate_gen_fun_capabilities(Module, Attributes, DeclaredBehaviours) ->
    GenFunOptions = gen_fun_options(Module, Attributes),
    Referenced = lists:usort(lists:append([gen_fun_capabilities(Opts) || Opts <- GenFunOptions])),
    Missing = Referenced -- DeclaredBehaviours,
    case Missing of
        [] -> ok;
        _ -> validation_error({undeclared_gen_fun_capabilities, Module, Missing})
    end.

gen_fun_options(Module, Attributes) ->
    Legacy = lists:flatten(proplists:get_all_values(gen_fun, Attributes)),
    Retained =
        [
            normalize_gen_fun_metadata(Module, Metadata)
         || Metadata <- attribute_values(gen_fun_meta, Attributes)
        ],
    Legacy ++ Retained.

normalize_gen_fun_metadata(_Module, {1, Options}) when is_map(Options) ->
    Options;
normalize_gen_fun_metadata(Module, {Version, _Options}) ->
    validation_error({unsupported_gen_fun_metadata_version, Module, Version});
normalize_gen_fun_metadata(Module, Metadata) ->
    validation_error({invalid_gen_fun_metadata, Module, Metadata}).

gen_fun_capabilities(Opts) when is_map(Opts) ->
    maps:get(behaviours, Opts, []) ++ maps:get(tbehaviours, Opts, []);
gen_fun_capabilities(_Opts) ->
    [].

merge_behaviour_modules(BehaviourModules, NewBehaviourModules) ->
    maps:fold(
        fun(Key, Module, Acc) ->
            case maps:find(Key, Acc) of
                {ok, Module} ->
                    Acc;
                {ok, ExistingModule} ->
                    validation_error(
                        {conflicting_instance, Key, ExistingModule, Module}
                    );
                error ->
                    maps:put(Key, Module, Acc)
            end
        end,
        BehaviourModules,
        NewBehaviourModules
    ).

validation_error(Reason) ->
    erlang:error({erlando_validation, Reason}).

compile(#state{types = Types, typeclasses = Typeclasses, behaviour_modules = BehaviourModules}) ->
    TypeclassModule = {attribute, 0, module, typeclass},
    Export = {attribute, 0, export, [{module, 2}, {is_typeclass, 1}, {type, 1}]},
    ResolvedTypes =
        maps:map(
            fun
                (Type, undefined) ->
                    [{tuple, [{atom, Type}, any]}];
                (_Type, Patterns) ->
                    Patterns
            end,
            Types
        ),
    TypesFun = generate_type(ResolvedTypes),
    IsTypeClass = generate_is_typeclass(Typeclasses),
    Module = generate_module(BehaviourModules),
    compile:forms([TypeclassModule, Export, TypesFun, IsTypeClass, Module], [debug_info]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_core_from_beam(BeamFile) ->
    try dialyzer_utils:get_core_from_beam(BeamFile) of
        {ok, Core} ->
            {ok, {r20_up, Core, Core}};
        {error, Reason} ->
            {error, Reason}
    catch
        error:undef ->
            case dialyzer_utils:get_abstract_code_from_beam(BeamFile) of
                {ok, Abs} ->
                    case dialyzer_utils:get_core_from_abstract_code(Abs) of
                        {ok, Core} ->
                            {ok, {r19, Core, Abs}};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

type_with_remote(Module, Type, Args, ExportedTypes, TRecMap, OtpVersion) ->
    RecMap =
        case dict:find(Module, TRecMap) of
            {ok, Val} ->
                Val;
            error ->
                #{}
        end,
    Type0 = {type, Type, Args},
    Type1 = {type, {Module, Type, Args}},
    case maps:find(Type0, RecMap) of
        {ok, {{Module, _FileLine, TypeForm, _ArgNames}, _}} ->
            CType = t_from_form(TypeForm, ExportedTypes, Type1, TRecMap, OtpVersion),
            {ok, CType};
        error ->
            {error, undefined_type}
    end.

t_from_form(TypeForm, ExportedTypes, Type1, TRecMap, r20_up) ->
    OtpRelease = list_to_integer(erlang:system_info(otp_release)),
    VarTable = erl_types:var_table__new(),
    TypeSite =
        case OtpRelease >= 24 of
            true ->
                erlang:append_element(Type1, "");
            false ->
                Type1
        end,
    Cache =
        case OtpRelease >= 26 of
            true ->
                #cache{mod_recs = maps:from_list(dict:to_list(TRecMap))};
            false ->
                #cache{mod_recs = {mrecs, TRecMap}}
        end,
    case OtpRelease >= 25 of
        true ->
            t_from_form_with_exported_types_table(
                TypeForm, ExportedTypes, TypeSite, VarTable, Cache
            );
        false ->
            {CType, _NCache} =
                erl_types:t_from_form(
                    TypeForm,
                    ExportedTypes,
                    TypeSite,
                    undefined,
                    VarTable,
                    Cache
                ),
            CType
    end;
t_from_form(TypeForm, ExportedTypes, Type1, TRecMap, r19) ->
    Cache = erl_types:cache__new(),
    VarTable = erl_types:var_table__new(),
    {CType, _NCache} = erl_types:t_from_form(
        TypeForm, ExportedTypes, Type1, TRecMap, VarTable, Cache
    ),
    CType.

t_from_form_with_exported_types_table(
    TypeForm, ExportedTypes, TypeSite, VarTable, Cache
) ->
    ExportedTypesTable = ets:new(exported_types, [set]),
    true = ets:insert(
        ExportedTypesTable,
        [
            {ExportedType}
         || ExportedType <- sets:to_list(ExportedTypes)
        ]
    ),
    try
        {CType, _NCache} =
            erl_types:t_from_form(
                TypeForm,
                ExportedTypesTable,
                TypeSite,
                undefined,
                VarTable,
                Cache
            ),
        CType
    after
        ets:delete(ExportedTypesTable)
    end.

type_to_patterns({c, tuple, Tuples, _}) ->
    TupleLists =
        lists:foldl(
            fun(TupleValue, Accs) ->
                Patterns = type_to_patterns(TupleValue),
                case Accs of
                    [] ->
                        lists:map(
                            fun(Pattern) ->
                                [Pattern]
                            end,
                            Patterns
                        );
                    Accs ->
                        [
                            [Pattern | AccValue]
                         || AccValue <- Accs,
                            Pattern <- Patterns
                        ]
                end
            end,
            [],
            Tuples
        ),
    lists:map(
        fun(TupleList) ->
            {tuple, lists:reverse(TupleList)}
        end,
        TupleLists
    );
type_to_patterns({c, function, _Function, _}) ->
    [{guard, is_function}];
type_to_patterns({c, atom, Atoms, _}) ->
    lists:map(fun(Atom) -> {atom, Atom} end, Atoms);
type_to_patterns({c, tuple_set, [{_N, Sets}], _}) ->
    lists:foldl(fun(Item, Acc) -> type_to_patterns(Item) ++ Acc end, [], Sets);
type_to_patterns({c, union, Unions, _}) ->
    lists:foldl(fun(Item, Acc) -> type_to_patterns(Item) ++ Acc end, [], Unions);
type_to_patterns({c, list, _, _}) ->
    [{guard, is_list}];
type_to_patterns({c, map, _, _}) ->
    [{guard, is_map}];
type_to_patterns({c, binary, _, _}) ->
    [{guard, is_binary}];
type_to_patterns({c, var, _, _}) ->
    [any];
type_to_patterns(any) ->
    [any];
type_to_patterns(none) ->
    [];
type_to_patterns({c, _Type, _Body, _Qualifier}) ->
    [].

pattern_to_clause(Line, Type, Pattern) ->
    {NPattern, Guards, _} =
        pattern_to_pattern_gurads(Line, Pattern, [], 1),
    GuardTest =
        case Guards of
            [] ->
                [];
            _ ->
                [Guards]
        end,
    {clause, Line, [NPattern], GuardTest, [{atom, Line, Type}]}.

pattern_to_pattern_gurads(Line, {tuple, Tuples}, Guards, Offset) ->
    {TupleList, NGuards, NOffset} =
        lists:foldl(
            fun(Element, {PatternAcc, GuardAcc, OffsetAcc}) ->
                {Pattern, NGuardAcc, NOffsetAcc} =
                    pattern_to_pattern_gurads(Line, Element, GuardAcc, OffsetAcc),
                {[Pattern | PatternAcc], NGuardAcc, NOffsetAcc}
            end,
            {[], Guards, Offset},
            Tuples
        ),
    {{tuple, Line, lists:reverse(TupleList)}, NGuards, NOffset};
pattern_to_pattern_gurads(Line, any, Guards, Offset) ->
    {{var, Line, '_'}, Guards, Offset};
pattern_to_pattern_gurads(Line, {atom, Atom}, Guards, Offset) ->
    {{atom, Line, Atom}, Guards, Offset};
pattern_to_pattern_gurads(Line, {guard, Guard}, Guards, Offset) ->
    ArgName = list_to_atom("Args" ++ integer_to_list(Offset)),
    {
        {var, Line, ArgName},
        [{call, Line, {atom, Line, Guard}, [{var, Line, ArgName}]} | Guards],
        Offset + 1
    }.

module_type_info(Module, Attributes, Typeclasses, ETypes, ModRecs, OtpVersion) ->
    TypeAttrs = types(Attributes),
    Behaviours0 = behaviours(Attributes),
    Behaviours1 = erlando_behaviours(Attributes),
    Behaviours = Behaviours0 ++ Behaviours1,
    TypeInstanceMap =
        lists:foldl(
            fun
                ({Type, UsedTypes}, Acc1) ->
                    Patterns = type_patterns(Module, UsedTypes, ETypes, ModRecs, OtpVersion),
                    maps:put(Type, Patterns, Acc1);
                (Type, Acc1) when is_atom(Type) ->
                    case maps:find(Type, Acc1) of
                        {ok, _Patterns} ->
                            Acc1;
                        error ->
                            maps:put(Type, undefined, Acc1)
                    end
            end,
            maps:new(),
            TypeAttrs
        ),
    Types = maps:keys(TypeInstanceMap),
    TypeBehaviourMap =
        case instance_metadata(Module, Attributes) of
            legacy ->
                legacy_type_behaviour_map(Module, Types, Behaviours, Typeclasses);
            {metadata, Instances} ->
                lists:foldl(
                    fun(Instance, Acc) ->
                        Type = maps:get(type, Instance),
                        Behaviour = maps:get(typeclass, Instance),
                        maps:put({Type, Behaviour}, Module, Acc)
                    end,
                    maps:new(),
                    Instances
                )
        end,
    {TypeInstanceMap, TypeBehaviourMap}.

legacy_type_behaviour_map(Module, Types, Behaviours, Typeclasses) ->
    lists:foldl(
        fun(Type, Acc1) ->
            lists:foldl(
                fun(Behaviour, Acc2) ->
                    case ordsets:is_element(Behaviour, Typeclasses) of
                        true ->
                            maps:put({Type, Behaviour}, Module, Acc2);
                        false ->
                            Acc2
                    end
                end,
                Acc1,
                Behaviours
            )
        end,
        maps:new(),
        Types
    ).

merge_type_instance(TypeInstanceMap, NTypeInstanceMap) ->
    maps:fold(
        fun(Type, Pattern, Acc) ->
            case maps:find(Type, Acc) of
                {ok, undefined} ->
                    maps:put(Type, Pattern, Acc);
                {ok, _} ->
                    Acc;
                error ->
                    maps:put(Type, Pattern, Acc)
            end
        end,
        TypeInstanceMap,
        NTypeInstanceMap
    ).

types(Attributes) ->
    lists:flatten(proplists:get_value(erlando_type, Attributes, [])).

behaviours(Attributes) ->
    proplists:get_value(behaviour, Attributes, []).

erlando_behaviours(Attributes) ->
    proplists:get_value(erlando_future_behaviour, Attributes, []).

generate_type(Types) ->
    Clauses =
        maps:fold(
            fun(Type, Patterns, Acc) ->
                NPatterns =
                    case Patterns of
                        undefined ->
                            [{tuple, [{atom, Type}, any]}];
                        _ ->
                            Patterns
                    end,
                lists:map(
                    fun(Pattern) ->
                        pattern_to_clause(0, Type, Pattern)
                    end,
                    NPatterns
                ) ++ Acc
            end,
            [],
            Types
        ),
    LastClause = {clause, 0, [{var, 0, '_'}], [], [{atom, 0, undefined}]},
    {function, 0, type, 1, Clauses ++ [LastClause]}.

generate_is_typeclass(Typeclasses) ->
    Clauses =
        lists:foldl(
            fun(Typeclass, Acc) ->
                [is_typeclass_clause(0, Typeclass) | Acc]
            end,
            [],
            Typeclasses
        ),
    LastClause = {clause, 0, [{var, 0, '_A'}], [], [{atom, 0, false}]},
    {function, 0, is_typeclass, 1, lists:reverse([LastClause | Clauses])}.

generate_module(BehaviourModules) ->
    Clauses =
        maps:fold(
            fun({Type, Behaviour}, Module, Acc) ->
                [module_clause(0, Type, Behaviour, Module) | Acc]
            end,
            [],
            BehaviourModules
        ),
    LastClause =
        {clause, 0, [{var, 0, 'A'}, {var, 0, 'B'}], [], [
            {call, 0, {atom, 0, exit}, [
                {tuple, 0, [
                    {atom, 0, unregisted_module}, {tuple, 0, [{var, 0, 'A'}, {var, 0, 'B'}]}
                ]}
            ]}
        ]},
    {function, 0, module, 2, lists:reverse([LastClause | Clauses])}.

is_typeclass_clause(Line, Typeclass) ->
    {clause, Line, [{atom, Line, Typeclass}], [], [{atom, Line, true}]}.

module_clause(Line, Type, Behaviour, Module) ->
    {clause, 1, [{atom, Line, Type}, {atom, Line, Behaviour}], [], [{atom, Line, Module}]}.

type_patterns(Module, Types, ETypes, ModRecs, OtpVersion) ->
    lists:foldl(
        fun({Type, Arity}, Acc) ->
            case type_with_remote(Module, Type, Arity, ETypes, ModRecs, OtpVersion) of
                {ok, CType} ->
                    Patterns = type_to_patterns(CType),
                    lists:usort(Patterns ++ Acc);
                {error, _} ->
                    Acc
            end
        end,
        [],
        Types
    ).

update_types_and_rec_map(Module, Core, AbsOrCore, Types, MRecDict) ->
    case rec_map(AbsOrCore) of
        {ok, RecMap} ->
            MTypes = exported_types(Core),
            NETypeAcc = sets:union(MTypes, Types),
            NMRecDict =
                case maps:size(RecMap) of
                    0 ->
                        MRecDict;
                    _ ->
                        dict:store(Module, RecMap, MRecDict)
                end,
            {NETypeAcc, NMRecDict};
        {error, Reason} ->
            rebar_api:debug("reason is ~p", [Reason]),
            {Types, MRecDict}
    end.

exported_types(Core) ->
    Attrs = cerl:module_attrs(Core),
    ExpTypes1 = [
        cerl:concrete(L2)
     || {L1, L2} <- Attrs,
        cerl:is_literal(L1),
        cerl:is_literal(L2),
        cerl:concrete(L1) =:= 'export_type'
    ],
    ExpTypes2 = lists:flatten(ExpTypes1),
    M = cerl:atom_val(cerl:module_name(Core)),
    sets:from_list([{M, F, A} || {F, A} <- ExpTypes2]).

rec_map(AbsOrCore) ->
    dialyzer_utils:get_record_and_type_info(AbsOrCore).
