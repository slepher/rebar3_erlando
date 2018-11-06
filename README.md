erlando
=====

see https://github.com/slepher/erlando

rebar3_erlando
-----
    
add command for erlando

    $ rebar3 erlando compile

typeclass.beam is now generated compile time by rebar3_erlando rebar3 plugin

if you want to use typeclass system by attribute -superclass|-erlando_type, you should add

    {provider_hooks, [{post, [{compile, {erlando, compile}}]}]}.
    
to rebar.config in your project

otherwise, rebar.config in project which deps on erlando is no need to change.

erlando_typeclass:register_application/1 is nolonger used.
