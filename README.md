erlando
=====

see https://github.com/slepher/erlando

rebar3_erlando
-----
    
add command for erlando

    $ rebar3 erlando compile

typeclass.beam is now generated compile time by rebar3_erlando rebar3 plugin

if you want to use the typeclass system through `-superclass`,
`-erlando_instance` or the legacy `-erlando_type/-behaviour` attributes, add

    {provider_hooks, [{post, [{compile, {erlando, compile}}]}]}.
    
to rebar.config in your project

otherwise, rebar.config in project which deps on erlando is no need to change.

erlando_typeclass:register_application/1 is nolonger used.

Since 0.3.0 the plugin prefers versioned `erlando_instance_meta` attributes.
These define exact `{Type, Typeclass}` mappings and are validated for required
callbacks, conflicts, generated capability adapters and dispatch coverage.
Modules without the new metadata continue to use the legacy
`erlando_type x behaviour` registration rule.
