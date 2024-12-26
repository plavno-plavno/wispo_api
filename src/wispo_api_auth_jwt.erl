-module(wispo_api_auth_jwt).

-export([
  generate/1,
  generate/2,
  validate/1,
  verify/1
]).

-define(DEFAULT_JWT_TTL, 60 * 2).

-spec generate(map()) -> map().
generate(Payload) ->
  Config = wispo_api_config:get(wispo_api, jwt),
  generate(Payload, Config).

%% wispo_api_auth_jwt:generate(#{}, []).
-spec generate(map(), proplists:proplist()) -> map().
generate(Payload, Opts) ->
  Key = proplists:get_value(key, Opts),
  Ttl = proplists:get_value(ttl, Opts, ?DEFAULT_JWT_TTL),
  Jwk = #{<<"kty">> => <<"oct">>, <<"k">> => Key},
  Jws = #{<<"alg">> => <<"HS256">>},
  Jwt = #{<<"iss">> => <<"wispo">>,
          <<"exp">> => erlang:system_time(seconds) + Ttl},
  Signed = jose_jwt:sign(Jwk, Jws, maps:merge(Jwt, Payload)),
  jose_jws:compact(Signed).

-spec validate(binary()) -> ok.
validate(_) ->
  {error, not_implemented}.

-spec verify(binary()) -> ok.
verify(_) ->
  {error, not_implemented}.

%% TODO: pubkey_rotate

