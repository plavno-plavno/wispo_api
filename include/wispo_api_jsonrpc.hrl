%%% ==================================================================
%%% @author ...
%%% @doc
%%% JSON-RPC 2.0 Specification: https://www.jsonrpc.org/specification
%%% @end
%%% ==================================================================

-ifndef(WISPO_API_JSONRPC_HRL).
-define(WISPO_API_JSONRPC_HRL, true).

%%% ==================================================================
%%% Error Codes.
%%%
%%% The error codes from and including -32768 to -32000 are reserved
%%% for pre-defined errors. Any code within this range, but not
%%% defined explicitly below is reserved for future use.
%%%
%%% The error codes from -32000 to -32099 are reserved for
%%% implementation-defined server-errors.
%%% ==================================================================

%% Pre-defined errors
-define(ERR_INVALID_REQUEST,          -32600).
-define(ERR_METHOD_NOT_FOUND,         -32601).
-define(ERR_INVALID_PARAMS,           -32602).
-define(ERR_INTERNAL_ERROR,           -32603).
-define(ERR_PARSE_ERROR,              -32700).

%% Implementation-defined errors
-define(ERR_ACCESS_DENIED,            -32001).
-define(ERR_MISMATCH,                 -32002).

-define(JSONRPC_NOTIFICATION(Method),         #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => Method}).
-define(JSONRPC_NOTIFICATION(Method, Params), #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => Method, <<"params">> => Params}).

-define(JSONRPC_REQ(Method, Id),          #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => Method, <<"id">> => Id}).
-define(JSONRPC_REQ(Method, Params, Id),  #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => Method, <<"params">> => Params, <<"id">> => Id}).

-define(JSONRPC_RESULT(Result),       #{<<"jsonrpc">> => <<"2.0">>, <<"result">> => Result}).
-define(JSONRPC_ERROR(Code, Message), #{<<"jsonrpc">> => <<"2.0">>, <<"error">> => #{<<"code">> => Code, <<"message">> => Message}}).

-endif.