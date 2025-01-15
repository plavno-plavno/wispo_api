-module(wispo_api_db_pg_worker).

-behavior(gen_server).

-export([
%%    start/0,
%%    start/1,
    start_link/0,
    start_link/1,
    stop/0
]).

-export([
    connect/0
]).

-export([
    select/2,
    insert/2,
    update/2,
    delete/2
]).

-export([
    squery/1,
    equery/2,
    transaction/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2,
    terminate/2,
    code_change/3
]).

%% Internal
-export([
    normalize_epgsql_reply/1,
    combine_column_names_and_values/2
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("wispo_api_common_utils.hrl").

%-define(DEBUG, true).

-ifdef(DEBUG).
-define(GEN_SERVER_OPTS, [{debug, [trace, {log_to_file, "log/debug.log"}]}]).
-else.
-define(GEN_SERVER_OPTS, []).
-endif.

-define(GEN_SERVER_TIMEOUT, 30).

-define(IS_INT_TYPE(X), (X =:= int2) or (X =:= int4) or (X =:= int8)).
-define(IS_JSON_TYPE(X), (X =:= json) or (X =:= jsonb)).

-type query() :: string() | binary() | iodata().
-type ok_reply(RowType) ::  {ok, ColumnsDescription :: [epgsql:column()], RowsValues :: [RowType]} |                            % SELECT
                            {ok, Count :: non_neg_integer()} |                                                                  % INSERT/UPDATE/DELETE
                            {ok, Count :: non_neg_integer(), ColumnsDescription :: [epgsql:column()], RowsValues :: [RowType]}. % INSERT/UPDATE/DELETE + RETURNING
-type error_reply() :: {error, #error{}}.
-type epgsql_reply(RowType) :: ok_reply(RowType) | error_reply().

-record(state, {
    args :: #{
        host        := inet:ip_address() | inet:hostname(),
        port        := inet:port_number(),
        timeout     := 5000,
        database    := iodata(),
        username    := iodata(),
        password    := iodata()
    },
    conn                        = undefined :: undefined | epgsql:connection(),
    conn_max_attempts_counter   = 0,
    conn_max_attempts           = infinity :: infinity | pos_integer(),
    conn_retry_after_n_sec      = 3
}).

%% TODO: https://www.erlang.org/doc/apps/stdlib/gen_server.html#start_link/4 (see WARNING section)
-spec start_link() ->
    {ok, Pid :: pid()}
    | ignore
    | {error, Reason :: term()}.
start_link() ->
    Config = wispo_api_config:get(?APP_NAME, ?MODULE),
    start_link(Config).

-spec start_link(Args :: term()) ->
    {ok, Pid :: pid()}
    | ignore
    | {error, Reason :: term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, ?GEN_SERVER_OPTS).

-spec stop() -> ok.
stop() ->
    gen_server:stop({local, ?MODULE}).

-spec connect() -> ok | {error, term()}.
connect() ->
    gen_server:call(?MODULE, connect).

-spec select(binary(), list(term())) ->
    {ok, term()}
    | {error, term()}.
select(Query, Args) ->
    equery(Query, Args).

-spec insert(binary(), list(term())) ->
    {ok, term()}
    | {error, term()}.
insert(Query, Args) ->
    case equery(Query, Args) of
        {ok, 1, [Profile]} ->
            {ok, Profile};
        {ok, 1} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec update(binary(), list(term())) ->
    {ok, term()}
    | {error, term()}.
update(Query, Args) ->
    equery(Query, Args).

-spec delete(binary(), list(term())) ->
    {ok, term()}
    | {error, term()}.
delete(Query, Args) ->
    case equery(Query, Args) of
        {ok, 1, [DeletedProfile]} ->
            {ok, maps:from_list(DeletedProfile)};
        {ok, 0, []} ->
            {error, not_found};
        {ok, 1} ->
            ok;
        {ok, 0} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec squery(query()) -> {ok, proplists:proplist()} | {error, term()}.
squery(Q) ->
    gen_server:call(?MODULE, {squery, Q}).

-spec equery(query(), list()) -> {ok, proplists:proplist()} | {error, term()}.
equery(Q, P) ->
    gen_server:call(?MODULE, {equery, Q, P}).

-spec transaction(query(), list()) -> {ok, proplists:proplist()} | {error, term()}.
transaction(Q, P) ->
    gen_server:call(?MODULE, {transaction, Q, P}).

-spec init(Args :: term()) ->
    {ok, State :: term()} |
    {ok, State :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term()} |
    ignore |
    {error, Reason :: term()}.
init(Args) ->
    ?LOG_INFO("Args: ~p", [Args]),
    {ok, #state{args = Args}, {continue, do_connect}}.

-spec handle_call(Request :: term(), From :: gen_server:from(), State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
handle_call({squery, Q} = Req, From, State) ->
    ?LOG_DEBUG("Req: ~p, From: ~p, State: ~p", [Req, From, State]),
    Reply = epgsql:squery(State#state.conn, Q),
    {reply, normalize_epgsql_reply(Reply), State};
handle_call({equery, Q, P} = Req, From, State) ->
    ?LOG_DEBUG("Req: ~p, From: ~p, State: ~p", [Req, From, State]),
    Reply = epgsql:equery(State#state.conn, Q, P),
    {reply, normalize_epgsql_reply(Reply), State};
handle_call(connect = Req, From, State) ->
    ?LOG_DEBUG("Req: ~p, From: ~p, State: ~p", [Req, From, State]),
    case State#state.conn of
        Pid when erlang:is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    Reply = {ok, connected},
                    {reply, Reply, State};
                false ->
                    Reply = {ok, in_progress},
                    {reply, Reply, State#state{conn = undefined}, {continue, do_connect}}
            end;
        undefined ->
            Reply = {ok, in_progress},
            {reply, Reply, State#state{conn = undefined}, {continue, do_connect}}
    end;
handle_call(disconnect = Req, From, State) ->
    ?LOG_DEBUG("Req: ~p, From: ~p, State: ~p", [Req, From, State]),
    {Reply, State2} = disconnect(State),
    {reply, Reply, State2};
handle_call(get_state = Req, From, State) ->
    ?LOG_DEBUG("Req: ~p, From: ~p, State: ~p", [Req, From, State]),
    Reply = ok,
    {reply, Reply, State};
handle_call(Req, From, State) ->
    ?LOG_INFO("Req: ~p, From: ~p, State: ~p", [Req, From, State]),
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
handle_cast(Req, State) ->
    ?LOG_INFO("Req: ~p, State: ~p", [Req, State]),
    {noreply, State}.

-spec handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
handle_info(Info, State) ->
    ?LOG_INFO("Info: ~p, State: ~p", [Info, State]),
    {noreply, State}.

-spec handle_continue(Info :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
handle_continue(do_connect = Info, State) ->
    ?LOG_DEBUG("Info: ~p, State: ~p", [Info, State]),
    case connect(State) of
        {ok, State2} ->
            ?LOG_DEBUG("DB_CONNECTION: ESTABLISHED"),
            {noreply, State2};
        {error, Reason} ->
            ?LOG_ERROR("DB_CONNECTION: FAILED. Reason: ~p", [Reason]),
            {noreply, State}
    end;
handle_continue(Info, State) ->
    ?LOG_INFO("Info: ~p, State: ~p", [Info, State]),
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: term()) ->
    term().
terminate(Reason, State) ->
    ?LOG_INFO("Reason: ~p, State: ~p", [Reason, State]),
    {ok, #state{conn = undefined}} = disconnect(State),
    ok.

-spec code_change(OldVsn :: (term() | {down, term()}), State :: term(), Extra :: term()) ->
    {ok, NewState :: term()} |
    {error, Reason :: term()}.
code_change(OldVsn, State, Extra) ->
    ?LOG_INFO("OldVsn: ~p, State: ~p, Extra: ~p", [OldVsn, State, Extra]),
    {ok, State}.

%% @private
-spec connect(#state{}) -> {ok, #state{}} | {error, epgsql:connect_error()}.
connect(#state{args = Args} = State) ->
    case epgsql:connect(Args) of
        {ok, C} ->
            {ok, State#state{conn = C}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
-spec disconnect(#state{}) -> {ok, #state{}}.
disconnect(State) ->
    case State#state.conn of
        Pid when erlang:is_pid(Pid) ->
            ok = epgsql:close(Pid),
            {ok, State#state{conn = undefined}};
        undefined ->
            {ok, State#state{conn = undefined}}
    end.

%% @private
-spec normalize_epgsql_reply(epgsql_reply(term())) ->
    {ok, [#column{}]} |
    {error, #error{}}.
normalize_epgsql_reply({ok, Columns, Rows}) ->          % SELECT ...
    {ok, combine_column_names_and_values(Columns, Rows)};
normalize_epgsql_reply({ok, Count}) ->                  % UPDATE ...
    {ok, Count};
normalize_epgsql_reply({ok, Count, Columns, Rows}) ->   % INSERT ... RETURNING ...
    {ok, Count, combine_column_names_and_values(Columns, Rows)};
normalize_epgsql_reply({error, Error}) ->
    {error, error_info_to_message(Error)}.

-spec combine_column_names_and_values([#column{}], [tuple()]) -> proplists:proplist().
combine_column_names_and_values(Cols, Rows) ->
    lists:map(fun(Row) -> combine_column_names_and_values(Cols, Row, 1, []) end, Rows).
    %lists:foldl(fun(Row, AccIn) -> [combine_column_names_and_values(Cols, Row, 1, []) | AccIn] end, [], Rows).

%% @private
-spec combine_column_names_and_values([#column{}], tuple(), pos_integer(), proplists:proplist()) -> proplists:proplist().
combine_column_names_and_values([H|T], Row, N, Acc) ->
    Val = erlang:element(N, Row),
    combine_column_names_and_values(T, Row, N + 1, [{H#column.name, binary_to_type(H, Val)} | Acc]);
combine_column_names_and_values([], _, _, Acc) ->
    Acc.

%% @private
%% @reference https://www.postgresql.org/docs/current/datatype.html
-spec binary_to_type(#column{}, term()) -> term().
binary_to_type(#column{}, null) ->
    null;
binary_to_type(#column{type = Type}, Value) when ?IS_INT_TYPE(Type), is_integer(Value) ->
    Value;
binary_to_type(#column{type = Type}, Value) when ?IS_JSON_TYPE(Type) ->
    ?JSON_DECODE(Value);
binary_to_type(#column{type = bool}, Value) ->
    Value;
binary_to_type(#column{type = xml}, Value) ->
    Value;
binary_to_type(#column{type = _Type}, Value) ->
    % NOTE: Type = varchar | text | timestamp | uuid | double | real | ...
    Value.

-spec error_info_to_message(#error{}) -> proplists:proplist().
error_info_to_message(#error{codename = unique_violation}) ->
    already_exists;
error_info_to_message(#error{code = Code, codename = CodeName, message = Message}) ->
    [{code, Code}, {codename, CodeName}, {message, Message}].
