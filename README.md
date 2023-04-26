Blockade
=====

Blockade is library which provides blocking and non-blocking
event handling for Erlang & Elixir applications.

Build
-----

    $ rebar3 compile


Run tests:
rebar3 compile && ct_run -dir test -logdir test_logs -pa ./_build/default/lib/blockade/ebin -setcookie cookievalue