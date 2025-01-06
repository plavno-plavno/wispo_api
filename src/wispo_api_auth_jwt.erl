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
  Config = wispo_api_config:get(wispo_api, auth_jwt),
  generate(Payload, Config).

-spec generate(map(), proplists:proplist()) -> map().
generate(Payload, Opts) ->
  Alg = proplists:get_value(alg, Opts),
  Key = proplists:get_value(key, Opts),
  Iss = proplists:get_value(iss, Opts),
  Ttl = proplists:get_value(jwt_access_ttl, Opts, ?DEFAULT_JWT_TTL),
  Exp = erlang:system_time(seconds) + Ttl,
  Jwk = #{<<"kty">> => <<"oct">>, <<"k">> => Key},
  Jws = #{<<"alg">> => Alg},
  Jwt = #{
    <<"iss">> => Iss,
    <<"exp">> => Exp
  },
  Signed = jose_jwt:sign(Jwk, Jws, maps:merge(Jwt, Payload)),
  {_, AccessJwt} = jose_jws:compact(Signed),
  #{
    access_jwt => AccessJwt,
    refresh_jwt => null,
    expires_in => Exp
  }.

-spec validate(binary()) -> ok.
validate(_) ->
  {error, not_implemented}.

-spec verify(binary()) -> ok.
verify(_) ->
  {error, not_implemented}.

%% TODO: pubkey_rotate

