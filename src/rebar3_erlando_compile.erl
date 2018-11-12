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

-record(state, {behaviour_modules = maps:new(), 
                typeclasses = [],
                type_aliases = [], 
                types = maps:new(),
                exported_types = sets:new(), 
                mod_recs = dict:new(),
                beamfiles = maps:new()
               }).

-record(cache,
        {
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
    case dialyzer_utils:get_core_from_beam(Beamfile) of
        {ok, Core} ->
            {NETypes, NModRecs} = 
                update_types_and_rec_map(Module, Core, ETypes, ModRecs),
            State#state{exported_types = NETypes, mod_recs = NModRecs};
        {error, _Reason} ->
            State
    end.
                  
% add typeclass first 
% then add type
add_instance({Module, Attributes}, #state{behaviour_modules = BehaviourModules,
                                          typeclasses = Typeclasses, types = Types, 
                                          exported_types = ETypes, mod_recs = ModRecs} = State) ->    
    {TypeInstanceMap, TypeBehaviourModuleMap} = 
        module_type_info(Module, Attributes, Typeclasses, ETypes, ModRecs),
    NTypes = merge_type_instance(Types, TypeInstanceMap),
    NBehaviourModules = maps:merge(BehaviourModules, TypeBehaviourModuleMap),
    NNTypes = 
        maps:map(
          fun(Type, undefined) ->
                  [{tuple,[{atom, Type},any]}];
             (_Type, Patterns) ->
                  Patterns
          end, NTypes),
    State#state{behaviour_modules = NBehaviourModules, types = NNTypes}.

compile(#state{types = Types, typeclasses = Typeclasses, behaviour_modules = BehaviourModules}) ->
    TypeclassModule = {attribute,0,module,typeclass},
    Export = {attribute,0,export,[{module,2}, {is_typeclass, 1}, {type, 1}]},
    TypesFun = generate_type(Types),
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
type_with_remote(Module, Type, Args, ExportedTypes, TRecMap) ->
    RecMap = case dict:find(Module, TRecMap) of
                 {ok, Val} ->
                     Val;
                 error ->
                     #{}
             end,
    Type0 = {type, Type, Args},
    Type1 = {type, {Module, Type, Args}},
    case maps:find(Type0, RecMap) of
        {ok, {{Module, _FileLine, TypeForm, _ArgNames}, _}} ->
            Cache = #cache{mod_recs = {mrecs, TRecMap}},
            {CType, _NCache} = erl_types:t_from_form(TypeForm, ExportedTypes, Type1, undefined, #{}, Cache),
            {ok, CType};
        error ->
            {error, undefined_type}
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
                            end, Patterns);
                      Accs ->
                          [[Pattern|AccValue] || 
                              AccValue <- Accs,
                              Pattern <- Patterns
                          ]
                  end
          end, [], Tuples),
    lists:map(
      fun(TupleList) ->
              {tuple, lists:reverse(TupleList)}
      end, TupleLists);
type_to_patterns({c, function, _Function, _}) ->
    [{guard, is_function}];
type_to_patterns({c, atom, Atoms, _}) ->
    lists:map(fun(Atom) -> {atom, Atom} end, Atoms);
type_to_patterns({c, tuple_set, [{_N, Sets}], _}) ->
    lists:foldl(fun(Item, Acc) -> type_to_patterns(Item) ++ Acc end, [],Sets);
type_to_patterns({c, union, Unions, _}) ->
    lists:foldl(fun(Item, Acc) -> type_to_patterns(Item) ++ Acc end, [],Unions);
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
                  {[Pattern|PatternAcc], NGuardAcc, NOffsetAcc}
          end, {[], Guards, Offset}, Tuples),
    {{tuple, Line, lists:reverse(TupleList)}, NGuards, NOffset};
pattern_to_pattern_gurads(Line, any, Guards, Offset) ->
    {{var, Line, '_'}, Guards, Offset};
pattern_to_pattern_gurads(Line, {atom, Atom}, Guards, Offset) ->
    {{atom, Line, Atom}, Guards, Offset};
pattern_to_pattern_gurads(Line, {guard, Guard}, Guards, Offset) ->
    ArgName = list_to_atom("Args" ++ integer_to_list(Offset)),
    {{var, Line, ArgName},
     [{call, Line, {atom, Line, Guard}, [{var, Line, ArgName}]}|Guards], Offset + 1}.

module_type_info(Module, Attributes, Typeclasses, ETypes, ModRecs) ->
    TypeAttrs = types(Attributes),
    Behaviours = behaviours(Attributes),
    TypeInstanceMap = 
        lists:foldl(
          fun({Type, UsedTypes}, Acc1) ->
                  Patterns = type_patterns(Module, UsedTypes, ETypes, ModRecs),
                  maps:put(Type, Patterns, Acc1);
             (Type, Acc1) when is_atom(Type) ->
                  case maps:find(Type, Acc1) of
                      {ok, _Patterns} ->
                          Acc1;
                      error ->
                          maps:put(Type, undefined, Acc1)
                  end
          end, maps:new(), TypeAttrs),
    Types = maps:keys(TypeInstanceMap),
    TypeBehaviourMap = 
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
                    end, Acc1, Behaviours)
          end, maps:new(), Types),
    {TypeInstanceMap, TypeBehaviourMap}.

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
      end, TypeInstanceMap, NTypeInstanceMap).

types(Attributes) ->
    lists:flatten(proplists:get_value(erlando_type, Attributes, [])).

behaviours(Attributes) ->
    proplists:get_value(behaviour, Attributes, []).

generate_type(Types) ->
    Clauses = 
        maps:fold(
          fun(Type, Patterns, Acc) ->
                  NPatterns = 
                      case Patterns of
                          undefined ->
                              [{tuple,[{atom, Type},any]}];
                          _ ->
                              Patterns
                      end,
                  lists:map(
                    fun(Pattern) ->
                            pattern_to_clause(0, Type, Pattern)
                    end, NPatterns) ++ Acc
          end, [], Types),
    LastClause = {clause, 0, [{var, 0, '_'}], [], [{atom, 0, undefined}]},
    {function, 0, type, 1, Clauses ++ [LastClause]}.

generate_is_typeclass(Typeclasses) ->
   Clauses = 
        lists:foldl(
          fun(Typeclass, Acc) ->
                  [is_typeclass_clause(0, Typeclass)|Acc]
          end, [], Typeclasses),
    LastClause = {clause, 0, [{var, 0, '_A'}], [], [{atom, 0, false}]},
    {function, 0, is_typeclass, 1, lists:reverse([LastClause|Clauses])}.

generate_module(BehaviourModules) ->
    Clauses = 
        maps:fold(
          fun({Type, Behaviour}, Module, Acc) ->
                  [module_clause(0, Type, Behaviour, Module)|Acc]
          end, [], BehaviourModules),
    LastClause = {clause, 0, [{var, 0, 'A'}, {var, 0, 'B'}], [], 
                  [{call, 0, {atom, 0, exit}, 
                    [{tuple, 0, [{atom, 0, unregisted_module}, {tuple, 0, [{var, 0, 'A'}, {var, 0, 'B'}]}]}]}]},
    {function, 0, module, 2, lists:reverse([LastClause|Clauses])}.

   
is_typeclass_clause(Line, Typeclass) ->
    {clause, Line, [{atom, Line, Typeclass}], [], [{atom, Line, true}]}.

module_clause(Line, Type, Behaviour, Module) ->
    {clause, 1, [{atom, Line, Type}, {atom, Line, Behaviour}], [],
     [{atom, Line, Module}]}.

type_patterns(Module, Types, ETypes, ModRecs) ->
    lists:foldl(
      fun({Type, Arity}, Acc) ->
              case type_with_remote(Module, Type, Arity, ETypes, ModRecs) of
                  {ok, CType} ->
                      Patterns = type_to_patterns(CType),
                      lists:usort(Patterns ++ Acc);
                  {error, _} ->
                      Acc
              end
      end, [], Types).

update_types_and_rec_map(Module, Core, Types, MRecDict) ->
    case rec_map(Core) of
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
        {error, _Reason} ->
            io:format("reason is ~p", [_Reason]),
            {Types, MRecDict}
    end.

exported_types(Core) ->
    Attrs = cerl:module_attrs(Core),
    ExpTypes1 = [cerl:concrete(L2) || {L1, L2} <- Attrs, cerl:is_literal(L1),
                                      cerl:is_literal(L2),
                                      cerl:concrete(L1) =:= 'export_type'],
    ExpTypes2 = lists:flatten(ExpTypes1),
    M = cerl:atom_val(cerl:module_name(Core)),
    sets:from_list([{M, F, A} || {F, A} <- ExpTypes2]).

rec_map(Core) ->
    dialyzer_utils:get_record_and_type_info(Core).
