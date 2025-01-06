%%% ==================================================================
%%% @author ...
%%% @doc
%%% JSON-RPC 2.0 API engine.
%%%
%%% JSON-RPC 2.0 Specification: https://www.jsonrpc.org/specification
%%% @end
%%% ==================================================================

-module(wispo_api_jsonrpc_v1).

%% API
-export([
  handle_call/2
]).

-export([
  to_response/1
]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include_lib("kernel/include/logger.hrl").
-include("wispo_api_common_utils.hrl").
-include("wispo_api_jsonrpc.hrl").

%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec handle_call(Request, State1) -> {reply, Reply, State2} | {noreply, State2} when
  Request :: binary() | map() | [map(), ...],
  State1 :: map(),
  State2 :: map(),
  Reply :: map().

handle_call(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method, <<"id">> := Id} = Req, State) ->
  Params = maps:get(<<"params">>, Req, undefined),
  {reply, Reply, State2} = handle_call(Method, Params, State),
  {reply, maps:put(<<"id">>, Id, Reply), State2};

handle_call(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Req, State) ->
  Params = maps:get(<<"params">>, Req, undefined),
  {reply, _Reply, State2} = handle_call(Method, Params, State), % TODO: Return ok | {ok, Data} | {error, Reason}
  {noreply, State2};

handle_call(BatchRequest, State) when erlang:is_list(BatchRequest) ->
  F =
    fun(Request, {AccIn, StateIn}) ->
      case handle_call(Request, StateIn) of
        {reply, ReplyOut, StateOut} ->
          {[ReplyOut | AccIn], StateOut};
        {noreply, StateOut} ->
          {AccIn, StateOut}
      end
    end,
  case lists:foldl(F, {[], State}, BatchRequest) of
    {[], State2} ->
      {noreply, State2};
    {[_|_] = Reply, State2} ->
      {reply, Reply, State2}
  end;

handle_call(Request, State) when erlang:is_binary(Request) ->
  try jsx:decode(Request, [return_maps]) of
    [_|_] = BatchRequest ->
      handle_call(BatchRequest, State);
    SingleRequest when is_map(SingleRequest) ->
      handle_call(SingleRequest, State);
    _ ->
      {reply, to_response(invalid_request), State}
  catch
    _:_ ->
      {reply, to_response(parse_error), State}
  end;

handle_call(_Request, State) ->
  {reply, to_response(invalid_request), State}.

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec handle_call(Method, Params, State1) -> {reply, Reply, State2} | {noreply, State2} when
  Method :: binary(),
  Params :: map(),
  State1 :: map(),
  State2 :: map(),
  Reply :: map().

handle_call(<<"rpc.discover">>, _Params, State) ->
  Path = code:priv_dir(?APP_NAME) ++ "/www/openrpc_v1.json",
  {ok, SpecJson} = file:read_file(Path),
  Spec = ?JSON_DECODE(SpecJson),
  Reply = to_response({ok, Spec}),
  {reply, Reply, State};

handle_call(<<"phones.register">>, #{<<"phone">> := Phone}, State) ->
  Reply = to_response(wispo_api:reg_phone(Phone)),
  {reply, Reply, State};

handle_call(<<"phones.confirm">>, #{<<"phone">> := Phone, <<"code">> := Code}, State) ->
  Reply = to_response(wispo_api:confirm_phone(Phone, Code)),
  {reply, Reply, State};

handle_call(<<"phones.is_known">>, #{<<"phone">> := Phone}, State) ->
  Reply = to_response(wispo_api:is_known_phone(Phone)),
  {reply, Reply, State};

handle_call(<<"phones.is_confirmed">>, #{<<"phone">> := Phone}, State) ->
  Reply = to_response(wispo_api:is_confirmed_phone(Phone)),
  {reply, Reply, State};

handle_call(<<"contacts.sync">>, #{<<"phone">> := _Phone, <<"contacts">> := _Contacts}, State) ->
  Reply = to_response(not_implemented),
  {reply, Reply, State};

handle_call(<<"contacts.unsync">>, #{<<"phone">> := _Phone}, State) ->
  Reply = to_response(not_implemented),
  {reply, Reply, State};

handle_call(<<"contacts.list">>, #{<<"phone">> := _Phone}, State) ->
  Reply = to_response(not_implemented),
  {reply, Reply, State};

handle_call(<<"health.check">>, _Params, State) ->
  Reply = to_response(ok),
  {reply, Reply, State};

handle_call(Method, Params, State) ->
  ?LOG_DEBUG("Method: ~p, Params: ~p, State: ~p)", [Method, Params, State]),
  ?LOG_INFO("UNEXPECTED_JSONRPC_METHOD. Method: ~p, Params: ~p, State: ~p)", [Method, Params, State]),
  {reply, to_response(not_implemented), State}.

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec to_response(term()) -> map().

to_response(ok) ->
  ?JSONRPC_RESULT(ok);
to_response({ok, Data}) ->
  ?JSONRPC_RESULT(Data);
to_response({ok, 1, [L]}) ->
  M = maps:from_list(L),
  ?JSONRPC_RESULT(M);
to_response(Bool) when is_boolean(Bool) ->
  ?JSONRPC_RESULT(Bool);
to_response(L) when erlang:is_list(L) ->
  ?JSONRPC_RESULT(L);
to_response(M) when is_map(M) ->
  ?JSONRPC_RESULT(M);
to_response({error, Reason}) when erlang:is_atom(Reason) ->
  to_response(Reason);
to_response({error, Reason}) ->
  ?JSONRPC_ERROR(?ERR_INTERNAL_ERROR, Reason);
to_response(invalid_params) ->
  ?JSONRPC_ERROR(?ERR_INVALID_PARAMS, <<"INVALID_PARAMS">>);
to_response(badarg) ->
  ?JSONRPC_ERROR(?ERR_INVALID_PARAMS, <<"INVALID_PARAMS">>);
to_response(not_implemented) ->
  ?JSONRPC_ERROR(?ERR_METHOD_NOT_FOUND, <<"METHOD_NOT_FOUND">>);
to_response(invalid_request) ->
  ?JSONRPC_ERROR(?ERR_INVALID_REQUEST, <<"INVALID_REQUEST">>);
to_response(parse_error) ->
  ?JSONRPC_ERROR(?ERR_INVALID_REQUEST, <<"PARSE_ERROR">>);
to_response(access_denied) ->
  ?JSONRPC_ERROR(?ERR_ACCESS_DENIED, <<"ACCESS_DENIED">>);
to_response(mismatch) ->
  ?JSONRPC_ERROR(?ERR_MISMATCH, <<"MISMATCH">>).