erlando
=====

see https://github.com/slepher/erlando

rebar3_erlando
-----
    
add command for erlando

    $ rebar3 erlando compile

add to rebar.config in project main directory to make erlando compile typeclass.beam after all modules compiled

    {provider_hooks, [{post, [{compile, {erlando, compile}}]}]}.
