%%%-------------------------------------------------------------------
%% @doc mnesia_test public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_test_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    lager:set_loglevel(lager_console_backend, debug),
    mnesia_test_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
