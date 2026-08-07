-module(rebar3_erlando_compile_SUITE).

%% API
-export([all/0]).

%% Test cases
-export([t_from_form_site/1,
         legacy_mapping_characterization/1,
         identical_mapping_is_deduplicated/1,
         conflicting_mapping_fails/1,
         missing_required_callback_fails/1,
         optional_callback_is_not_required/1,
         undeclared_gen_fun_capability_fails/1,
         retained_gen_fun_metadata_is_validated/1,
         unknown_gen_fun_metadata_version_fails/1,
         metadata_defines_exact_mappings/1,
         unknown_metadata_version_fails/1,
         missing_dispatch_callback_fails/1,
         missing_dispatch_adapter_fails/1,
         function_type_patterns_classify_fun_values/1]).

all() ->
    [t_from_form_site,
     legacy_mapping_characterization,
     identical_mapping_is_deduplicated,
     conflicting_mapping_fails,
     missing_required_callback_fails,
     optional_callback_is_not_required,
     undeclared_gen_fun_capability_fails,
     retained_gen_fun_metadata_is_validated,
     unknown_gen_fun_metadata_version_fails,
     metadata_defines_exact_mappings,
     unknown_metadata_version_fails,
     missing_dispatch_callback_fails,
     missing_dispatch_adapter_fails,
     function_type_patterns_classify_fun_values].

t_from_form_site(_Config) ->
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
        {ok, typeclass, _} = rebar3_erlando_compile:compile(State),
        ok
    after
        ok = file:delete(BeamFile)
    end.

legacy_mapping_characterization(_Config) ->
    Typeclass = test_functor,
    Instance = test_identity,
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{fmap, 1}], [])},
       {Instance, instance_forms(Instance, sample, [Typeclass], [{fmap, 1}])}],
      fun(Beamfiles) ->
              State = add_fixture_modules(Typeclass, [Instance], Beamfiles),
              {ok, typeclass, Beam} = rebar3_erlando_compile:compile(State),
              {module, typeclass} = code:load_binary(typeclass, "typeclass.beam", Beam),
              Instance = typeclass:module(sample, Typeclass),
              sample = typeclass:type({sample, value}),
              true = typeclass:is_typeclass(Typeclass),
              ok
      end).

identical_mapping_is_deduplicated(_Config) ->
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
              {ok, typeclass, _} = rebar3_erlando_compile:compile(State1),
              ok
      end).

conflicting_mapping_fails(_Config) ->
    Typeclass = test_conflict_capability,
    First = test_conflict_first,
    Second = test_conflict_second,
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{run, 1}], [])},
       {First, instance_forms(First, conflict_type, [Typeclass], [{run, 1}])},
       {Second, instance_forms(Second, conflict_type, [Typeclass], [{run, 1}])}],
      fun(Beamfiles) ->
              assert_error(
                {erlando_validation,
                 {conflicting_instance, {conflict_type, Typeclass}, First, Second}},
                fun() -> add_fixture_modules(Typeclass, [First, Second], Beamfiles) end)
      end).

missing_required_callback_fails(_Config) ->
    Typeclass = test_required_capability,
    Instance = test_missing_callback_instance,
    with_beams(
      [{Typeclass, typeclass_forms(Typeclass, [{required, 1}], [])},
       {Instance, instance_forms(Instance, missing_callback_type, [Typeclass], [])}],
      fun(Beamfiles) ->
              assert_error(
                {erlando_validation,
                 {missing_callbacks, Instance, Typeclass, [{required, 1}]}},
                fun() -> add_fixture_modules(Typeclass, [Instance], Beamfiles) end)
      end).

optional_callback_is_not_required(_Config) ->
    Typeclass = test_optional_capability,
    Instance = test_optional_instance,
    with_beams(
      [{Typeclass,
        typeclass_forms(Typeclass, [{required, 1}, {optional, 1}], [{optional, 1}])},
       {Instance,
        instance_forms(Instance, optional_type, [Typeclass], [{required, 1}])}],
      fun(Beamfiles) ->
              State = add_fixture_modules(Typeclass, [Instance], Beamfiles),
              {ok, typeclass, _} = rebar3_erlando_compile:compile(State),
              ok
      end).

undeclared_gen_fun_capability_fails(_Config) ->
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
              assert_error(
                {erlando_validation,
                 {undeclared_gen_fun_capabilities, Instance, [undeclared_capability]}},
                fun() ->
                        rebar3_erlando_compile:add_modules(
                          [Typeclass], [{Instance, Attributes}], ModuleMap,
                          rebar3_erlando_compile:new())
                end)
      end).

retained_gen_fun_metadata_is_validated(_Config) ->
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
              assert_error(
                {erlando_validation,
                 {undeclared_gen_fun_capabilities, Instance, [undeclared_capability]}},
                fun() -> add_fixture_modules(Typeclass, [Instance], Beamfiles) end)
      end).

unknown_gen_fun_metadata_version_fails(_Config) ->
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
              assert_error(
                {erlando_validation,
                 {unsupported_gen_fun_metadata_version, Instance, 99}},
                fun() -> add_fixture_modules(Typeclass, [Instance], Beamfiles) end)
      end).

metadata_defines_exact_mappings(_Config) ->
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
              Instance = typeclass:module(first_metadata_type, FirstClass),
              Instance = typeclass:module(second_metadata_type, SecondClass),
              assert_exit({unregisted_module, {first_metadata_type, SecondClass}},
                          fun() -> typeclass:module(first_metadata_type, SecondClass) end),
              assert_exit({unregisted_module, {second_metadata_type, FirstClass}},
                          fun() -> typeclass:module(second_metadata_type, FirstClass) end),
              ok
      end).

unknown_metadata_version_fails(_Config) ->
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
              assert_error(
                {erlando_validation,
                 {unsupported_instance_metadata_version, Instance, 99}},
                fun() -> add_fixture_modules(Typeclass, [Instance], Beamfiles) end)
      end).

missing_dispatch_callback_fails(_Config) ->
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
              assert_error(
                {erlando_validation,
                 {missing_dispatch_callback, Instance, dispatch_coverage_type,
                  Typeclass, {run, 1}}},
                fun() -> add_fixture_modules(Typeclass, [Instance], Beamfiles) end)
      end).

missing_dispatch_adapter_fails(_Config) ->
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
              assert_error(
                {erlando_validation,
                 {missing_dispatch_adapter, Instance, dispatch_adapter_type,
                  Typeclass, {run, 1}, {missing_adapter, 1}}},
                fun() -> add_fixture_modules(Typeclass, [Instance], Beamfiles) end)
      end).

%% Compatibility regression: the typeclass registry must classify real fun
%% values as the `function` type. Lenses declares a bare `function` type
%% (lenses_function) while erlando declares it with a reference to a fun type
%% definition (function_instance). When the bare declaration is scanned first,
%% the merged type pattern must still match fun values; otherwise runtime
%% dispatch falls back to undetermined records and consumers fail with
%% badfun (e.g. lenses getter:view). Reproduces the rebar3_erlando 0.4.0
%% registry-generation regression introduced by commit 44ce586.
function_type_patterns_classify_fun_values(_Config) ->
    BareClass = test_function_bare_capability,
    RefClass = test_function_ref_capability,
    BareInstance = test_function_bare_instance,
    RefInstance = test_function_ref_instance,
    BareForms =
        [{attribute, 1, module, BareInstance},
         {attribute, 1, erlando_type, function},
         {attribute, 1, behaviour, BareClass},
         {attribute, 1, export, [{run, 1}]},
         function_form({run, 1})],
    FunType = {type, 1, 'fun',
               [{type, 1, product, [{type, 1, any, []}]}, {type, 1, any, []}]},
    RefForms =
        [{attribute, 1, module, RefInstance},
         {attribute, 1, erlando_type, {function, [{function_instance, 0}]}},
         {attribute, 1, type, {function_instance, FunType, []}},
         {attribute, 1, behaviour, RefClass},
         {attribute, 1, export, [{run, 1}]},
         function_form({run, 1})],
    with_beams(
      [{BareClass, typeclass_forms(BareClass, [{run, 1}], [])},
       {RefClass, typeclass_forms(RefClass, [{run, 1}], [])},
       {BareInstance, BareForms},
       {RefInstance, RefForms}],
      fun(Beamfiles) ->
              Types = [{BareInstance, beam_attributes(BareInstance, Beamfiles)},
                       {RefInstance, beam_attributes(RefInstance, Beamfiles)}],
              State = rebar3_erlando_compile:add_modules(
                        [BareClass, RefClass], Types, Beamfiles,
                        rebar3_erlando_compile:new()),
              {ok, typeclass, Beam} = rebar3_erlando_compile:compile(State),
              {module, typeclass} = code:load_binary(typeclass, "typeclass.beam", Beam),
              function = typeclass:type(fun() -> ok end),
              ok
      end).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

assert_error(Expected, Fun) ->
    try Fun() of
        _ -> ct:fail({expected_error, Expected, got_success})
    catch
        error:Expected ->
            ok;
        error:Other ->
            ct:fail({expected_error, Expected, other_error, Other});
        Class:Other ->
            ct:fail({expected_error, Expected, other, Class, Other})
    end.

assert_exit(Expected, Fun) ->
    try Fun() of
        _ -> ct:fail({expected_exit, Expected, got_success})
    catch
        exit:Expected ->
            ok;
        exit:Other ->
            ct:fail({expected_exit, Expected, other_exit, Other});
        Class:Other ->
            ct:fail({expected_exit, Expected, other, Class, Other})
    end.

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
