%%% This module depend on the `cowboy` application and uses its API functions.
%%% @reference https://github.com/ninenines/cowboy

-module(wispo_api_net_http).

-export([
  start/0,
  start/1,
  stop/0
]).

%% Cowboy callbacks
-export([
  init/2,
  terminate/3,
  allowed_methods/2,
  allow_missing_post/2,
  content_types_accepted/2,
  is_authorized/2,
  forbidden/2,
  service_available/2
]).

%% API Callbacks
-export([
  from_json/2
]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include_lib("kernel/include/logger.hrl").
-include("wispo_api_net_http.hrl").
-include("wispo_api_common_utils.hrl").

%%% ==================================================================
%%% Cowboy callbacks
%%% ==================================================================

-define(HTTP_API_REF, ?MODULE).
-define(HTTP_API_IP, {127,0,0,1}).
-define(HTTP_API_HOST, '_').
-define(HTTP_API_PORT, 8989).
-define(HTTP_API_URLS, [
  {"/jsonrpc", wispo_api_net_http, #{api => jsonrpc}},
  {"/rest/:vsn/:ns/:op", wispo_api_net_http, #{api => http_rest}},
  {"/static/[...]", cowboy_static_handler, #{}}
]).

-spec start() -> {ok, pid()} | {error, term()}.
start() ->
  start([]).

-spec start(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start(Opts) ->
  Ip = proplists:get_value(ip, Opts, ?HTTP_API_IP),
  Port = proplists:get_value(port, Opts, ?HTTP_API_PORT),
  Host = proplists:get_value(host, Opts, ?HTTP_API_HOST),
  Urls = proplists:get_value(urls, Opts, ?HTTP_API_URLS),
  TransOpts = [
    {ip, Ip},
    {port, Port}
  ],
  Dispatch  = cowboy_router:compile([{Host, Urls}]),
  ProtoOpts = #{env => #{dispatch => Dispatch}},
  cowboy:start_clear(?HTTP_API_REF, TransOpts, ProtoOpts).

-spec stop() -> ok.
stop() ->
  cowboy:stop_listener(?HTTP_API_REF).

%%% ==================================================================
%%% Cowboy callbacks
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec init(Req, any()) -> {ok | module(), Req, any()} | {module(), Req, any(), any()} when
  Req :: cowboy_req:req().

init(Req, Opts) ->
  State = init_state(Req, Opts),
  {cowboy_rest, Req, State}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec terminate(Reason, Req, State) -> ok when
  Reason :: normal | {crash, error | exit | throw, any()},
  Req :: cowboy_req:req(),
  State :: term().

terminate(_Reason, _Req, _State) ->
  ok.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec allowed_methods(Req, State) -> {Result, Req, State} when
  Req :: cowboy_req:req(),
  State :: term(),
  Result :: [Method],
  Method :: binary(),
  Req :: cowboy_req:req(),
  State :: term().

allowed_methods(Req, State) ->
  Methods = [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
  {Methods, Req, State}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec allow_missing_post(Req, State)
      -> {boolean(), Req, State}
    | {stop, Req, State}
    | {cowboy_rest:switch_handler(), Req, State}
  when Req::cowboy_req:req(), State::any().

allow_missing_post(Req, State) ->
  {false, Req, State}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec is_authorized(Req, State) ->
  {true | {false, iodata()}, Req, State}
  | {stop, Req, State}
  | {cowboy_rest:switch_handler(), Req, State}
  when Req::cowboy_req:req(), State::any().

is_authorized(Req, State) ->
  {true, Req, State}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec forbidden(Req, State)
      -> {boolean(), Req, State}
    | {stop, Req, State}
    | {cowboy_rest:switch_handler(), Req, State}
  when Req::cowboy_req:req(), State::any().

forbidden(Req, State) ->
  {false, Req, State}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec service_available(Req, State)
      -> {boolean(), Req, State}
    | {stop, Req, State}
    | {cowboy_rest:switch_handler(), Req, State}
  when Req::cowboy_req:req(), State::any().

service_available(Req, State) ->
  {true, Req, State}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec content_types_accepted(Req, State) -> {Result, Req, State} when
  Req :: cowboy_req:req(),
  State :: term(),
  Result :: [{binary() | ParsedMime, AcceptCallback :: atom()}],
  ParsedMime :: {Type :: binary(), SubType :: binary(), '*' | Params},
  Params :: [{Key :: binary(), Value :: binary()}],
  Req :: cowboy_req:req(),
  State :: term().

content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, from_json}
  ], Req, State}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec from_json(Req, State) -> {Result, Req, State} when
  Req :: cowboy_req:req(),
  State :: term(),
  Result :: cowboy_req:resp_body().

% cowboy_req:set_resp_cookie(<<"t">>, Jwt, Req1, #{path => <<"/">>});
from_json(Req, State) ->
  State2 = init_state(Req, State),
  {ok, ReqBody} = get_req_body(Req),
  case wispo_api_jsonrpc_v1:handle_call(ReqBody, State2) of
    {reply, Reply, State3} ->
      Json = jsx:encode(Reply),
      Headers = #{<<"content-type">> => <<"application/json">>},
      Req2 = cowboy_req:reply(?HTTP_200_OK, Headers, Json, Req),
      {stop, Req2, State3};
    {noreply, State3} ->
      {stop, Req, State3}
  end.

%% TODO: Fix -H 'Accept: application/json' (to_json/2 -> from_json/2)
%% curl -X POST http://localhost:8989/jsonrpc2 -H 'Content-Type: application/json' -H 'Accept: application/json' -d '{"id": 123, "method": "foo.bar"}'

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec init_state(Req, State) -> {ApiCall, Ctx} when
  Req :: cowboy_req:req(),
  State :: maps:map(),
  ApiCall :: maps:map(),
  Ctx :: maps:map().

init_state(Req, State) ->
  {Ip, _Port} = cowboy_req:peer(Req),
  State2 = #{
    request_time  => erlang:system_time(seconds),
    client_ip     => erlang:list_to_binary(inet:ntoa(Ip)),
    protocol_vsn  => cowboy_req:binding(vsn, Req, 1) % TODO: Hardcoded default protocol version. Use value from wispo.config
  },
  maps:merge(State, State2).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec get_req_body(cowboy_req:req()) -> {ok, binary()} | {error, nobody}.

get_req_body(Req) ->
  case cowboy_req:has_body(Req) of
    true ->
      {ok, ReqBodyRaw, _} = cowboy_req:read_body(Req),
      {ok, ReqBodyRaw};
    false ->
      {error, nobody}
  end.

%% @private
-spec http_method_to_op_name(cowboy_req:req()) -> binary() | undefined.
http_method_to_op_name(Req) ->
  case cowboy_req:method(Req) of
    <<"GET">> -> <<"read">>;
    <<"POST">> -> <<"create">>;
    <<"PUT">> -> <<"update">>;
    <<"DELETE">> -> <<"delete">>;
    _ -> undefined
  end.