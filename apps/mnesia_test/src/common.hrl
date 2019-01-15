
-include("otp_types.hrl").

-type(gate_reason() :: nonempty_string()).
-type(gate_code() :: integer()).
-type(gate_reply() :: ok | {error, gate_code(), gate_reason()}).

-type(gs_call_gate_reply() ::
{reply, gate_reply(), gs_state()} |
{reply, gate_reply(), gs_state(), timeout() | hibernate} |
{noreply, gs_state()} |
{noreply, gs_state(), timeout() | hibernate} |
{stop, gs_reason(), gate_reply(), gs_state()} |
{stop, gs_reason(), gs_state()}).

-define(INTERNAL_SERVER_ERROR, 500). %% gen_server exception
-define(ETCD_KEY_UNAVAILABLE_ERROR, 501). %% etcd answers error
-define(ETCD_SERVER_UNAVAILABLE_ERROR, 502). %% cant connect to etcd server
-define(DATADOG_UPDATE_INTERVAL, 1000).

%% 6 sec global time out - it happens nothong
-define(SMS_SENDING_GLOBAL_TIME, 6000).
%% 2 sec - every sms sends before this timeout expired. if connection lost, it waits new connection
%% 1 sec for sending + 1 for reconnect
-define(SMS_SENDING_EXPIRATION_TIME, 2).
%% internal sms  submit time out, works bad becouse it working inside connection process only
-define(SMS_SENDING_SUBMIT_TIME, 2).


-define(CATCH_CALL(),
    ClassType:Reason ->
    [{_Module, _Fun, _Arity, [{file, File}, {line, Line}]} | _ ] = Stack = erlang:get_stacktrace(),
    ExReason = lists:flatten(io_lib:format("~p ~s ~p", [Reason, File, Line])),
    lager:error("~p: ~p~nlocation: ~p~nstack:~n~p~n", [ClassType, Reason, lists:flatten(io_lib:format("~s ~p", [File, Line])), Stack]),
    {reply, {error, ?INTERNAL_SERVER_ERROR, ExReason}, State}
).

-define(CATCH_CAST(),
    ClassType:Reason ->
    [{_Module, _Fun, _Arity, [{file, File}, {line, Line}]} | _ ] = Stack = erlang:get_stacktrace(),
    lager:error("~n~p:     ~p~nlocation:  ~p~nstack:     ~p~n", [ClassType, Reason, lists:flatten(io_lib:format("~s ~p", [File, Line])), Stack]),
    {noreply, State}
).

-define(TRACE(), {current_function, {M, F, A}} = process_info(self(), current_function), lager:debug("~p:~p/~p", [M, F, A])).
-define(TRACE_HC(), {current_function, {M, F, _A}} = process_info(self(), current_function), lager:debug("~p:~p(~p)", [M, F, element(1,Request)])).
-define(TRACE(X), {current_function, {M, F, _A}} = process_info(self(), current_function), lager:debug("~p:~p(~p)", [M, F, X])).

-define(ANY_HANDLE_CALL(),
    handle_call(Arg1, _From, State) when is_tuple(Arg1) ->
    {current_function, {M, F, _A}} = process_info(self(), current_function), lager:error("unknown_handle_call ~p:~p(~p)", [M, F, element(1,Arg1)]),
    {reply, {error, ?INTERNAL_SERVER_ERROR, {unknown_handle_call, M, element(1,Arg1)}}, State};
    handle_call(_Request, _From, State) ->
    {current_function, {M, F, A}} = process_info(self(), current_function), lager:error("unknown_handle_call ~p:~p/~p", [M, F, A]),
    {reply, {error, ?INTERNAL_SERVER_ERROR, {unknown_handle_call,M}}, State}
).

-define(ANY_HANDLE_CAST(),
    handle_cast(Arg1, State) when is_tuple(Arg1) ->
    {current_function, {M, F, _A}} = process_info(self(), current_function), lager:error("unknown_handle_cast ~p:~p(~p)", [M, F, element(1,Arg1)]),
    {noreply, State};
    handle_cast(_Request, State) ->
    {current_function, {M, F, A}} = process_info(self(), current_function), lager:error("unknown_handle_cast ~p:~p/~p", [M, F, A]),
    {noreply, State}
).

-define(ANY_HANDLE_INFO(),
    handle_info(Arg1, State) when is_tuple(Arg1) ->
    {current_function, {M, F, _A}} = process_info(self(), current_function), lager:error("unknown_handle_info ~p:~p(~p)", [M, F, element(1,Arg1)]),
    {noreply, State};
    handle_info(_Request, State) ->
    {current_function, {M, F, A}} = process_info(self(), current_function), lager:error("unknown_handle_info ~p:~p/~p", [M, F, A]),
    {noreply, State}
).

