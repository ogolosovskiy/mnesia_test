%%%-------------------------------------------------------------------
%%% @author ogo
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jan 2019 12:17
%%%-------------------------------------------------------------------
-module(mnesia_test_gen).
-author("ogo").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
%%-compile([debug_info, export_all]).

-export([
            write1/1,
            write2/1,
            update1/1,
            update2/1,
            write_test/1,
            update_test/1
        ]).

-include("common.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


-record(test_record1, {key1 = <<"">> :: binary(), key2 = <<"">> :: binary(),  data = #{} :: map() | '_'}).
-record(test_record2, {key1 = <<"">> :: binary(), key2 = <<"">> :: binary(),  data = #{} :: map() | '_'}).

-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    ?TRACE(),
    mnesia:create_schema([nodes()]),
    mnesia:start(),
    timer:sleep(500),


    mnesia:delete_table(test_record1),
    mnesia:delete_table(test_record2),
    {atomic, ok} = mnesia:create_table(test_record1, [  {disc_copies, [node()]}, {attributes, record_info(fields,test_record1)} ]),
    {atomic, ok} = mnesia:create_table(test_record2, [ {disc_copies, [node()]}, {index, [#test_record2.key2]}, {attributes, record_info(fields,test_record2)} ]),
    {ok, #state{}}.


write1(L) ->
    F = fun() ->
            Test1 =  #test_record1{key1 = base64:encode(crypto:strong_rand_bytes(6)), key2 =base64:encode(crypto:strong_rand_bytes(6)), data = #{}},
            mnesia:write(Test1)
        end,
    [{atomic,ok} = mnesia:transaction(F) || _ <- L],
    ok.

write2(L) ->
    F = fun() ->
        Test1 =  #test_record2{key1 = base64:encode(crypto:strong_rand_bytes(6)), key2 =base64:encode(crypto:strong_rand_bytes(6)), data = #{}},
        mnesia:write(Test1)
        end,
    [{atomic,ok} = mnesia:transaction(F) || _ <- L],
    ok.

write_test(Count) ->
    L = lists:seq(1, Count),

    {Occ1, _} =  timer:tc(mnesia_test_gen, write1, [L]),
    io:format("write (one index for ~p records): ~w seconds ~n", [Count, Occ1 / 1000000]),

    {Occ2, _} =  timer:tc(mnesia_test_gen, write2, [L]),
    io:format("write (two indexes for ~p records): ~w seconds ~n", [Count, Occ2 / 1000000]),

    ok.


update1(L) ->
     F = fun() ->
         Key = <<"wfOZTPx7">>,
         NewRecord = case mnesia:select(test_record1, [{#test_record1{key1 = Key, _ = '_', _ = '_'}, [], ['$_']}]) of
                       [Record] -> Record;
                       [] -> #test_record1{key1 = Key, key2 =base64:encode(crypto:strong_rand_bytes(6)), data = #{}}
                   end,
         mnesia:write(NewRecord)
         end,
    [{atomic,ok} = mnesia:transaction(F) || _ <- L].

update2(L) ->
    F = fun() ->
        Key = <<"wfOZTPx8">>,
        NewRecord = case mnesia:select(test_record2, [{#test_record2{key1 =  '_', key2 = Key, _ = '_'}, [], ['$_']}]) of
                        [Record] -> Record;
                        [] -> #test_record2{key1 = base64:encode(crypto:strong_rand_bytes(6)), key2 = Key, data = #{}}
                    end,
        mnesia:write(NewRecord)
        end,
    [{atomic,ok} = mnesia:transaction(F) || _ <- L].

update_test(Count) ->
    L = lists:seq(1, Count),

    {Occ1, _} =  timer:tc(mnesia_test_gen, write1, [L]),
    io:format("update(one index for ~p records): ~w seconds ~n", [Count, Occ1 / 1000000]),

    {Occ2, _} =  timer:tc(mnesia_test_gen, write2, [L]),
    io:format("update(two indexes for ~p records): ~w seconds ~n", [Count, Occ2 / 1000000]),

    ok.




-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.


-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.


-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.


-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
