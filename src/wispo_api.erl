-module(wispo_api).

-export([
    handle_call/3
]).

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

-type path() :: binary().
-export_type([path/0]).

-type phone() :: binary().
-export_type([phone/0]).

-type code() :: list(non_neg_integer()).
-export_type([code/0]).

-spec handle_call(path(), map(), map()) ->
    {ok, map()}
    | {error, map()}.
handle_call(<<"/phones/register">>, #{<<"phone">> := Phone}, _State) ->
    % TODO: #{<<"debug">> => true, <<"debug_key">> => <<"secret-id">>} (for CT)
    reply(reg_phone(Phone));
handle_call(<<"/phones/confirm">>, #{<<"phone">> := Phone, <<"code">> := Code}, _State) ->
    reply(confirm_phone(Phone, Code));
handle_call(<<"/contacts/sync">>, Data, _State) ->
    ContactsOwner = undefined, % TODO: Use JID from JWT
    reply(wispo_api_contacts:sync(ContactsOwner, Data));
handle_call(<<"/contacts/remove-synced">>, _Data, _State) ->
    ContactsOwner = undefined, % TODO: Use JID from JWT
    reply(wispo_api_contacts:remove_synced(ContactsOwner));
handle_call(<<"/jwt/refresh">>, #{<<"refresh_jwt">> := RefreshJwt}, _State) ->
    case wispo_api_auth_jwt:is_jwt_refresh(RefreshJwt) of
        true ->
            case wispo_api_auth_jwt:verify(RefreshJwt) of
                true ->
                    {jose_jwt, #{<<"jid">> := Jid}} = jose_jwt:peek(RefreshJwt),
                    Jwt = wispo_api_auth_jwt:generate(#{<<"jid">> => Jid}),
                    Jwt2 = maps:put(jid, Jid, Jwt),
                    reply({ok, Jwt2});
                false ->
                    reply({error, invalid_token})
            end;
        false ->
            reply({error, invalid_token})
    end;
handle_call(<<"/health/check">>, _Data, _State) ->
    reply(ok);
handle_call(_Path, _Data, _State) ->
    reply({error, unexpected_call}).

%%% Internal functions

%% @private
-spec reply(atom() | tuple()) -> map().
reply(ok) ->
    #{status => ok};
reply({ok, Data}) ->
    #{status => ok, data => Data};
reply({error, Reason}) ->
    #{status => error, data => #{message => Reason}};
reply(_Any) ->
    #{status => error, data => #{message => internal_error}}.

%% @private
-spec reg_phone(binary()) ->
    {ok, map()}
    | {error, term()}.
reg_phone(Phone) ->
    Config = wispo_api_config:get(wispo_api, auth_code),
    reg_phone(Phone, Config).

%% @private
-spec reg_phone(phone(), map()) ->
    {ok, map()}
    | {error, term()}.
reg_phone(Phone, Opts) ->
    CodeLen = proplists:get_value(code_len, Opts, 6),
    CodeTtl = proplists:get_value(code_ttl, Opts, 60 * 2),
    CodeExp = erlang:system_time(seconds) + CodeTtl,
    %Code = wispo_api_common_utils:rand_code(CodeLen),
    {ok, Code} = wispo_api_common_utils:cut_n_last_numbers(CodeLen, Phone),
    Rec = #wispo_api_phone{
        phone = Phone,
        code = Code,
        code_exp = CodeExp
    },
    true = ets:insert(?ETS_NAME, Rec),
    {ok, #{code_ttl => CodeTtl}}.

%% @private
-spec confirm_phone(phone(), code()) -> ok | {error, mismatch}.
confirm_phone(Phone, Code) ->
    case ets:lookup(?ETS_NAME, Phone) of
        [#wispo_api_phone{phone = Phone, code = Code, is_confirmed = false}] ->
            Rec = #wispo_api_phone{
                phone = Phone,
                is_confirmed = true
            },
            true = ets:insert(?ETS_NAME, Rec),
            Cfg = wispo_api_config:get(wispo_api, auth_jwt),
            XmppHost = proplists:get_value(xmpp_host, Cfg),
            Jid = <<(uuid:uuid_to_string(uuid:get_v3(Phone), binary_standard))/binary, "@", XmppHost/binary>>,
            Jwt = wispo_api_auth_jwt:generate(#{<<"jid">> => Jid}),
            {ok, maps:put(jid, Jid, Jwt)};
        _ ->
            {error, mismatch}
    end.

%% @private
-spec is_known_phone(phone()) -> boolean().
is_known_phone(Phone) ->
    case ets:lookup(?ETS_NAME, Phone) of
        [#wispo_api_phone{phone = Phone}] ->
            true;
        [] ->
            false
    end.

%% @private
-spec is_confirmed_phone(phone()) -> boolean().
is_confirmed_phone(Phone) ->
    case ets:lookup(?ETS_NAME, Phone) of
        [#wispo_api_phone{phone = Phone, is_confirmed = true}] ->
            true;
        _ ->
            false
    end.