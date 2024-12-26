-module(wispo_api_time_utils).

-export([
    is_valid_unixtime/1
]).

-include("wispo_api_common_utils.hrl").

-spec is_valid_unixtime(term()) -> boolean().
is_valid_unixtime(X) when is_integer(X) ->
    X > ?EPOCH_ZERO;
is_valid_unixtime(_X) ->
    false.


