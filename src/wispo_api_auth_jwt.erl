-module(wispo_api_auth_jwt).

-export([
  generate/1,
  generate/2,
  verify/1,
  is_jwt_refresh/1
]).

-include("wispo_api_common_utils.hrl").

-define(DEFAULT_JWT_TTL, 60 * 5).
-define(DEFAULT_JWT_REFRESH_TTL, ?SECONDS_PER_DAY * 365).

-spec generate(map()) -> map().
generate(Payload) ->
  Config = wispo_api_config:get(wispo_api, auth_jwt),
  generate(Payload, Config).

-spec generate(map(), proplists:proplist()) -> map().
generate(Payload, Opts) ->
  Alg = proplists:get_value(alg, Opts),
  Key = proplists:get_value(key, Opts),
  Iss = proplists:get_value(iss, Opts),
  TtlA = proplists:get_value(jwt_access_ttl, Opts, ?DEFAULT_JWT_TTL),
  TtlR = proplists:get_value(jwt_refresh_ttl, Opts, ?DEFAULT_JWT_REFRESH_TTL),
  ExpA = erlang:system_time(seconds) + TtlA,
  ExpR = erlang:system_time(seconds) + TtlR,
  Jwk = #{<<"kty">> => <<"oct">>, <<"k">> => Key},
  Jws = #{<<"alg">> => Alg},
  Jwt = #{
    <<"iss">> => Iss,
    <<"exp">> => ExpA
  },
  Signed = jose_jwt:sign(Jwk, Jws, maps:merge(Jwt, Payload)),
  {_, AccessJwt} = jose_jws:compact(Signed),
  RefreshJwt = maps:put(<<"exp">>, ExpR, Jwt),
  RefreshJwt2 = maps:put(<<"is_refresh">>, true, RefreshJwt),
  SignedRefresh = jose_jwt:sign(Jwk, Jws, maps:merge(RefreshJwt2, Payload)),
  {_, RefreshJwt3} = jose_jws:compact(SignedRefresh),
  #{
    access_jwt => AccessJwt,
    access_jwt_expires_in => ExpA,
    refresh_jwt => RefreshJwt3,
    refresh_jwt_expires_in => ExpR
  }.

-spec verify(binary()) -> ok.
verify(Jwt) ->
  Config = wispo_api_config:get(wispo_api, auth_jwt),
  verify(Jwt, Config).

-spec verify(binary(), proplists:proplist()) -> ok.
verify(Jwt, Opts) ->
  Key = proplists:get_value(key, Opts),
  Jwk = #{<<"kty">> => <<"oct">>, <<"k">> => Key},
  {Bool, _, _} = jose_jwt:verify(Jwk, Jwt),
  Bool.

-spec is_jwt_refresh(binary()) -> boolean().
is_jwt_refresh(Jwt) ->
  case jose_jwt:peek(Jwt) of
    {jose_jwt, #{<<"is_refresh">> := true}} ->
      true;
    _ ->
      false
  end.