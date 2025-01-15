-module(wispo_api_time_utils).

-export([
    is_valid_unixtime/1,
    unixtime_to_localtime/1,
    unixtime_to_rfc1123/1
]).

-include("wispo_api_common_utils.hrl").

-spec is_valid_unixtime(term()) -> boolean().
is_valid_unixtime(X) when is_integer(X) ->
    X > ?EPOCH_ZERO;
is_valid_unixtime(_X) ->
    false.

-spec unixtime_to_localtime(pos_integer()) -> calendar:datetime().
unixtime_to_localtime(UnixTime) ->
    calendar:system_time_to_local_time(UnixTime, seconds).

-spec unixtime_to_rfc1123(pos_integer()) -> binary().
unixtime_to_rfc1123(UnixTime) ->
    LocalTime = unixtime_to_localtime(UnixTime),
    list_to_binary(httpd_util:rfc1123_date(LocalTime)).