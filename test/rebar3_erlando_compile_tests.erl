-module(rebar3_erlando_compile_tests).

-include_lib("eunit/include/eunit.hrl").

t_from_form_site_test() ->
    Module = rebar3_erlando_test_type,
    Forms = [
        {attribute, 1, module, Module},
        {attribute, 2, export_type, [{sample, 0}]},
        {attribute, 3, type, {sample, {atom, 3, sample}, []}},
        {attribute, 4, erlando_type, [{sample, [{sample, 0}]}]}
    ],
    {ok, Module, Beam} = compile:forms(Forms, [debug_info]),
    BeamFile = filename:join("_build", atom_to_list(Module) ++ ".beam"),
    ok = filelib:ensure_dir(BeamFile),
    ok = file:write_file(BeamFile, Beam),
    try
        {ok, {Module, [{attributes, Attributes}]}} =
            beam_lib:chunks(BeamFile, [attributes]),
        State =
            rebar3_erlando_compile:add_modules(
              [], [{Module, Attributes}], #{Module => BeamFile},
              rebar3_erlando_compile:new()),
        ?assertMatch({ok, typeclass, _}, rebar3_erlando_compile:compile(State))
    after
        ok = file:delete(BeamFile)
    end.

legacy_mapping_characterization_test() ->
    Typeclass = test_functor,
    Instance = test_identity,
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{fmap, 1}], [])},
       {Instance, instance_forms(Instance, sample, [Typeclass], [{fmap, 1}])}],
      fun(Beamfiles) ->
              State = add_fixture_modules(Typeclass, [Instance], Beamfiles),
              {ok, typeclass, Beam} = rebar3_erlando_compile:compile(State),
              {module, typeclass} = code:load_binary(typeclass, "typeclass.beam", Beam),
              ?assertEqual(Instance, typeclass:module(sample, Typeclass)),
              ?assertEqual(sample, typeclass:type({sample, value})),
              ?assert(typeclass:is_typeclass(Typeclass))
      end).

identical_mapping_is_deduplicated_test() ->
    Typeclass = test_duplicate_capability,
    Instance = test_duplicate_instance,
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{run, 1}], [])},
       {Instance, instance_forms(Instance, duplicate_type, [Typeclass], [{run, 1}])}],
      fun(Beamfiles) ->
              State0 = add_fixture_modules(Typeclass, [Instance], Beamfiles),
              Attributes = beam_attributes(Instance, Beamfiles),
              State1 = rebar3_erlando_compile:add_modules(
                         [], [{Instance, Attributes}], #{}, State0),
              ?assertMatch({ok, typeclass, _}, rebar3_erlando_compile:compile(State1))
      end).

conflicting_mapping_fails_test() ->
    Typeclass = test_conflict_capability,
    First = test_conflict_first,
    Second = test_conflict_second,
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{run, 1}], [])},
       {First, instance_forms(First, conflict_type, [Typeclass], [{run, 1}])},
       {Second, instance_forms(Second, conflict_type, [Typeclass], [{run, 1}])}],
      fun(Beamfiles) ->
              ?assertError(
                 {erlando_validation,
                  {conflicting_instance, {conflict_type, Typeclass}, First, Second}},
                 add_fixture_modules(Typeclass, [First, Second], Beamfiles))
      end).

missing_required_callback_fails_test() ->
    Typeclass = test_required_capability,
    Instance = test_missing_callback_instance,
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{required, 1}], [])},
       {Instance, instance_forms(Instance, missing_callback_type, [Typeclass], [])}],
      fun(Beamfiles) ->
              ?assertError(
                 {erlando_validation,
                  {missing_callbacks, Instance, Typeclass, [{required, 1}]}},
                 add_fixture_modules(Typeclass, [Instance], Beamfiles))
      end).

optional_callback_is_not_required_test() ->
    Typeclass = test_optional_capability,
    Instance = test_optional_instance,
    with_beams(
      [{Typeclass,
        typeclass_forms(Typeclass, [{required, 1}, {optional, 1}], [{optional, 1}])},
       {Instance,
        instance_forms(Instance, optional_type, [Typeclass], [{required, 1}])}],
      fun(Beamfiles) ->
              State = add_fixture_modules(Typeclass, [Instance], Beamfiles),
              ?assertMatch({ok, typeclass, _}, rebar3_erlando_compile:compile(State))
      end).

undeclared_gen_fun_capability_fails_test() ->
    Typeclass = test_gen_fun_capability,
    Instance = test_gen_fun_instance,
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{run, 1}], [])},
       {Instance, instance_forms(Instance, gen_fun_type, [Typeclass], [{run, 1}])}],
      fun(Beamfiles) ->
              Attributes0 = beam_attributes(Instance, Beamfiles),
              Attributes = [{gen_fun, [#{behaviours => [undeclared_capability]}]} |
                            Attributes0],
              ModuleMap = maps:with([Typeclass, Instance], Beamfiles),
              ?assertError(
                 {erlando_validation,
                  {undeclared_gen_fun_capabilities, Instance, [undeclared_capability]}},
                 rebar3_erlando_compile:add_modules(
                   [Typeclass], [{Instance, Attributes}], ModuleMap,
                   rebar3_erlando_compile:new()))
      end).

retained_gen_fun_metadata_is_validated_test() ->
    Typeclass = test_retained_gen_fun_capability,
    Instance = test_retained_gen_fun_instance,
    Forms =
        [{attribute, 1, module, Instance},
         {attribute, 1, erlando_type, retained_gen_fun_type},
         {attribute, 1, behaviour, Typeclass},
         {attribute, 1, gen_fun_meta,
          {1, #{behaviours => [undeclared_capability]}}},
         {attribute, 1, export, [{run, 1}]},
         function_form({run, 1})],
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{run, 1}], [])},
       {Instance, Forms}],
      fun(Beamfiles) ->
              ?assertError(
                 {erlando_validation,
                  {undeclared_gen_fun_capabilities, Instance, [undeclared_capability]}},
                 add_fixture_modules(Typeclass, [Instance], Beamfiles))
      end).

unknown_gen_fun_metadata_version_fails_test() ->
    Typeclass = test_gen_fun_version_capability,
    Instance = test_gen_fun_version_instance,
    Forms =
        [{attribute, 1, module, Instance},
         {attribute, 1, erlando_type, gen_fun_version_type},
         {attribute, 1, behaviour, Typeclass},
         {attribute, 1, gen_fun_meta, {99, #{behaviours => [Typeclass]}}},
         {attribute, 1, export, [{run, 1}]},
         function_form({run, 1})],
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{run, 1}], [])},
       {Instance, Forms}],
      fun(Beamfiles) ->
              ?assertError(
                 {erlando_validation,
                  {unsupported_gen_fun_metadata_version, Instance, 99}},
                 add_fixture_modules(Typeclass, [Instance], Beamfiles))
      end).

metadata_defines_exact_mappings_test() ->
    FirstClass = test_metadata_first_capability,
    SecondClass = test_metadata_second_capability,
    Instance = test_metadata_instance,
    Metadata =
        {1,
         #{module => Instance,
           types => [first_metadata_type, second_metadata_type],
           instances =>
               [#{type => first_metadata_type, typeclass => FirstClass,
                  implementation => local, adapter => manual},
                #{type => second_metadata_type, typeclass => SecondClass,
                  implementation => local, adapter => manual}]}},
    InstanceForms =
        [{attribute, 1, module, Instance},
         {attribute, 1, erlando_type, [first_metadata_type, second_metadata_type]},
         {attribute, 1, behaviour, FirstClass},
         {attribute, 1, behaviour, SecondClass},
         {attribute, 1, erlando_instance_meta, Metadata},
         {attribute, 1, export, [{first, 1}, {second, 1}]},
         function_form({first, 1}),
         function_form({second, 1})],
    with_beams(
      [{FirstClass, typeclass_forms(FirstClass, [{first, 1}], [])},
       {SecondClass, typeclass_forms(SecondClass, [{second, 1}], [])},
       {Instance, InstanceForms}],
      fun(Beamfiles) ->
              Types = [{Instance, beam_attributes(Instance, Beamfiles)}],
              State = rebar3_erlando_compile:add_modules(
                        [FirstClass, SecondClass], Types, Beamfiles,
                        rebar3_erlando_compile:new()),
              {ok, typeclass, Beam} = rebar3_erlando_compile:compile(State),
              {module, typeclass} = code:load_binary(typeclass, "typeclass.beam", Beam),
              ?assertEqual(Instance, typeclass:module(first_metadata_type, FirstClass)),
              ?assertEqual(Instance, typeclass:module(second_metadata_type, SecondClass)),
              ?assertExit({unregisted_module, {first_metadata_type, SecondClass}},
                          typeclass:module(first_metadata_type, SecondClass)),
              ?assertExit({unregisted_module, {second_metadata_type, FirstClass}},
                          typeclass:module(second_metadata_type, FirstClass))
      end).

unknown_metadata_version_fails_test() ->
    Typeclass = test_metadata_version_capability,
    Instance = test_metadata_version_instance,
    Forms =
        [{attribute, 1, module, Instance},
         {attribute, 1, erlando_type, metadata_version_type},
         {attribute, 1, behaviour, Typeclass},
         {attribute, 1, erlando_instance_meta, {99, #{module => Instance}}},
         {attribute, 1, export, [{run, 1}]},
         function_form({run, 1})],
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{run, 1}], [])},
       {Instance, Forms}],
      fun(Beamfiles) ->
              ?assertError(
                 {erlando_validation,
                  {unsupported_instance_metadata_version, Instance, 99}},
                 add_fixture_modules(Typeclass, [Instance], Beamfiles))
      end).

missing_dispatch_callback_fails_test() ->
    Typeclass = test_dispatch_coverage_capability,
    Instance = test_dispatch_coverage_instance,
    Metadata =
        {1, #{module => Instance,
              types => [dispatch_coverage_type],
              instances =>
                  [#{type => dispatch_coverage_type,
                     typeclass => Typeclass,
                     implementation => dispatch,
                     dispatch => #{}}]}},
    Forms = dispatch_instance_forms(Instance, dispatch_coverage_type,
                                    Typeclass, Metadata, [{run, 1}]),
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{run, 1}], [])},
       {Instance, Forms}],
      fun(Beamfiles) ->
              ?assertError(
                 {erlando_validation,
                  {missing_dispatch_callback, Instance, dispatch_coverage_type,
                   Typeclass, {run, 1}}},
                 add_fixture_modules(Typeclass, [Instance], Beamfiles))
      end).

missing_dispatch_adapter_fails_test() ->
    Typeclass = test_dispatch_adapter_capability,
    Instance = test_dispatch_adapter_instance,
    Metadata =
        {1, #{module => Instance,
              types => [dispatch_adapter_type],
              instances =>
                  [#{type => dispatch_adapter_type,
                     typeclass => Typeclass,
                     implementation => dispatch,
                     dispatch => #{run => {missing_adapter, 1}}}]}},
    Forms = dispatch_instance_forms(Instance, dispatch_adapter_type,
                                    Typeclass, Metadata, [{run, 1}]),
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{run, 1}], [])},
       {Instance, Forms}],
      fun(Beamfiles) ->
              ?assertError(
                 {erlando_validation,
                  {missing_dispatch_adapter, Instance, dispatch_adapter_type,
                   Typeclass, {run, 1}, {missing_adapter, 1}}},
                 add_fixture_modules(Typeclass, [Instance], Beamfiles))
      end).

add_fixture_modules(Typeclass, Instances, Beamfiles) ->
    Types = [{Instance, beam_attributes(Instance, Beamfiles)} || Instance <- Instances],
    ModuleMap = maps:with([Typeclass | Instances], Beamfiles),
    rebar3_erlando_compile:add_modules(
      [Typeclass], Types, ModuleMap, rebar3_erlando_compile:new()).

beam_attributes(Module, Beamfiles) ->
    {ok, {Module, [{attributes, Attributes}]}} =
        beam_lib:chunks(maps:get(Module, Beamfiles), [attributes]),
    Attributes.

typeclass_forms(Module, Callbacks, OptionalCallbacks) ->
    [{attribute, 1, module, Module}] ++
        [callback_form(Function) || Function <- Callbacks] ++
        optional_callback_forms(OptionalCallbacks).

callback_form({Name, Arity}) ->
    Arguments = lists:duplicate(Arity, {type, 1, any, []}),
    FunctionType = {type, 1, 'fun', [{type, 1, product, Arguments}, {type, 1, any, []}]},
    {attribute, 1, callback, {{Name, Arity}, [FunctionType]}}.

optional_callback_forms([]) ->
    [];
optional_callback_forms(OptionalCallbacks) ->
    [{attribute, 1, optional_callbacks, OptionalCallbacks}].

instance_forms(Module, Type, Behaviours, Exports) ->
    [{attribute, 1, module, Module},
     {attribute, 1, erlando_type, Type}] ++
        [{attribute, 1, behaviour, Behaviour} || Behaviour <- Behaviours] ++
        [{attribute, 1, export, Exports}] ++
        [function_form(Function) || Function <- Exports].

function_form({Name, Arity}) ->
    Arguments = [{var, 1, list_to_atom("Arg" ++ integer_to_list(N))}
                 || N <- lists:seq(1, Arity)],
    {function, 1, Name, Arity, [{clause, 1, Arguments, [], [{atom, 1, ok}]}]}.

dispatch_instance_forms(Module, Type, Typeclass, Metadata, Exports) ->
    [{attribute, 1, module, Module},
     {attribute, 1, erlando_type, Type},
     {attribute, 1, behaviour, Typeclass},
     {attribute, 1, erlando_instance_meta, Metadata},
     {attribute, 1, export, Exports}] ++
        [function_form(Function) || Function <- Exports].

with_beams(ModuleForms, Test) ->
    Beamfiles = maps:from_list([compile_fixture(Module, Forms) || {Module, Forms} <- ModuleForms]),
    try
        Test(Beamfiles)
    after
        code:purge(typeclass),
        code:delete(typeclass),
        lists:foreach(fun({_Module, Beamfile}) -> ok = file:delete(Beamfile) end,
                      maps:to_list(Beamfiles))
    end.

compile_fixture(Module, Forms) ->
    {ok, Module, Beam} = compile:forms(Forms, [debug_info]),
    BeamFile = filename:join("_build", atom_to_list(Module) ++ ".beam"),
    ok = filelib:ensure_dir(BeamFile),
    ok = file:write_file(BeamFile, Beam),
    {Module, BeamFile}.
