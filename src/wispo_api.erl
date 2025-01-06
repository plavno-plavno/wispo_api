-module(wispo_api).

-export([
    reg_phone/1,
    reg_phone/2,
    confirm_phone/2
]).

-export([
    is_known_phone/1,
    is_confirmed_phone/1
]).

-include("wispo_api_ets.hrl").

-type phone() :: binary().
-export_type([phone/0]).

-type code() :: list(non_neg_integer()).
-export_type([code/0]).

%% wispo_api:reg_phone(<<"+1234567890">>).
-spec reg_phone(binary()) ->
    {ok, map()}
    | {error, term()}.
reg_phone(Phone) ->
    Config = wispo_api_config:get(wispo_api, auth_code),
    reg_phone(Phone, Config).

%% wispo_api:reg_phone(<<"+1234567890">>, [{code_ttl, 60}]).
-spec reg_phone(phone(), map()) ->
    {ok, map()}
    | {error, term()}.
reg_phone(Phone, Opts) ->
    CodeTtl = proplists:get_value(code_ttl, Opts, 60 * 2),
    CodeExp = erlang:system_time(seconds) + CodeTtl,
    Code = wispo_api_common_utils:rand_code(),
    Rec = #wispo_api_phone{
        phone = Phone,
        code = Code,
        code_exp = CodeExp
    },
    true = ets:insert(?ETS_NAME, Rec),
    {ok, #{
        code => Code,
        code_ttl => CodeTtl
    }}.

%% wispo_api:confirm_phone(<<"+1234567890">>, [6,5,4,6])
-spec confirm_phone(phone(), code()) -> ok | {error, expired}.
confirm_phone(Phone, Code) ->
    case ets:lookup(?ETS_NAME, Phone) of
        [#wispo_api_phone{phone = Phone, code = Code, is_confirmed = false}] ->
            Rec = #wispo_api_phone{
                phone = Phone,
                is_confirmed = true
            },
            true = ets:insert(?ETS_NAME, Rec),
            {ok, wispo_api_auth_jwt:generate(#{<<"user_id">> => wispo_api_common_utils:uuid7()})};
        _ ->
            {error, mismatch}
    end.

%% wispo_api:is_known_phone(<<"+1234567890">>).
-spec is_known_phone(phone()) -> boolean().
is_known_phone(Phone) ->
    case ets:lookup(?ETS_NAME, Phone) of
        [#wispo_api_phone{phone = Phone}] ->
            true;
        [] ->
            false
    end.

%% wispo_api:is_confirmed_phone(<<"+1234567890">>).
-spec is_confirmed_phone(phone()) -> boolean().
is_confirmed_phone(Phone) ->
    case ets:lookup(?ETS_NAME, Phone) of
        [#wispo_api_phone{phone = Phone, is_confirmed = true}] ->
            true;
        _ ->
            false
    end.