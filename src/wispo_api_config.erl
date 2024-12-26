-module(wispo_api_config). % TODO: Move into wispo_api_config_app_env.erl

-export([
    get/0,
    get/2,
    get/3,
    set/3,
    unset/2
]).

-include("wispo_api_common_utils.hrl").

-spec get() -> list().
get() ->
    application:get_all_env(?APP_NAME).

%% Example:
%% wispo_api_config:get(wispo_api, http_api).
-spec get(atom(), atom()) -> term() | undefined.
get(App, Key) ->
    get(App, Key, undefined).

-spec get(atom(), atom(), term()) -> term().
get(App, Key, Default) ->
    application:get_env(App, Key, Default).

-spec set(atom(), atom(), term()) -> ok.
set(App, Key, Val) ->
    application:set_env(App, Key, Val).

-spec unset(atom(), atom()) -> ok.
unset(App, Key) ->
    application:unset_env(App, Key).