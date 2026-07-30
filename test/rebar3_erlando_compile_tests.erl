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
