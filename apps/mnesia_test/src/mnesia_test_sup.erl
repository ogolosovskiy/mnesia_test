%%%-------------------------------------------------------------------
%% @doc mnesia_test top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_test_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

    Tester = {monitor, {mnesia_test_gen, start_link, []},
        permanent, 2000, worker, [mnesia_test_gen]},
    Childrens = [Tester],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Childrens}}.

%%====================================================================
%% Internal functions
%%====================================================================
