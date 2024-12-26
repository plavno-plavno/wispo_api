-module(wispo_api_os_env).

-export([
    env/0
]).

-spec env() -> list().
env() ->
    L = lists:map(fun({K, V}) -> {list_to_binary(K), V} end, os:env()),
    F = fun({<<"WISPO_", _/binary>>, _V}) -> true; (_) -> false end,
    lists:filter(F, L).
