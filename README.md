[![CI](https://github.com/slepher/rebar3_erlando/actions/workflows/ci.yml/badge.svg?branch=master&event=push)](https://github.com/slepher/rebar3_erlando/actions/workflows/ci.yml?query=branch%3Amaster)

[![CI](https://github.com/slepher/rebar3_erlando/actions/workflows/release.yml/badge.svg?branch=0.4.4&event=push)](https://github.com/slepher/rebar3_erlando/actions/workflows/release.yml?query=branch%3A0.4.4)

erlando
=====

see https://github.com/slepher/erlando

rebar3_erlando
-----
    
add command for erlando

    $ rebar3 erlando compile

typeclass.beam is now generated compile time by rebar3_erlando rebar3 plugin

Since 0.4.0 the plugin injects the post-compile hook
{post, [{compile, {erlando, compile}}]} into the project state on init,
so projects do not need to declare the hook themselves; the hook runs
once per build (project-wide) and writes the generated typeclass.beam to
the erlando app's out_dir. Per-app hook executions are no-ops.

The typeclass registry is rebuilt from all deps' beams on every compile
instead of accumulating state between builds, so repeated compiles keep
the registry correct.

erlando_typeclass:register_application/1 is nolonger used.

Since 0.3.0 the plugin prefers versioned `erlando_instance_meta` attributes.
These define exact `{Type, Typeclass}` mappings and are validated for required
callbacks, conflicts, generated capability adapters and dispatch coverage.
Modules without the new metadata continue to use the legacy
`erlando_type x behaviour` registration rule.
