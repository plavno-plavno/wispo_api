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

-include("wispo_api_net_http.hrl").
-include("wispo_api_ets.hrl").

-type path() :: binary().
-export_type([path/0]).

-type phone() :: binary().
-export_type([phone/0]).

-type code() :: list(non_neg_integer()).
-export_type([code/0]).

-spec handle_call(path(), map(), map()) ->
    pos_integer()
    | {pos_integer(), map()}.
handle_call(<<"/phones/register">>, Params, _State) ->
    case maps:get(<<"phone">>, Params, null) of
        null ->
            to_reply({error, invalid_params});
        Phone when is_binary(Phone) ->
            to_reply(reg_phone(Phone))
    end;
handle_call(<<"/phones/confirm">>, Params, _State) ->
    case {maps:get(<<"phone">>, Params, null), maps:get(<<"code">>, Params, null)} of
        {Phone, Code} when is_binary(Phone), is_binary(Code) ->
            to_reply(confirm_phone(Phone, Code));
        _ ->
            to_reply({error, invalid_params})
    end;
handle_call(<<"/contacts/sync">>, Params, State) ->
    case maps:get(<<"contacts">>, Params, null) of
        Contacts = [_|_] ->
            ContactsOwnerJid = maps:get(user_jid, State),
            to_reply(wispo_api_contacts:sync(ContactsOwnerJid, Contacts));
        _ ->
            to_reply({error, invalid_params})
    end;
handle_call(<<"/contacts/remove-synced">>, _Params, State) ->
    case maps:get(user_jid, State, null) of
        ContactsOwnerJid when is_binary(ContactsOwnerJid) ->
            to_reply(wispo_api_contacts:remove_synced(ContactsOwnerJid));
        _ ->
            to_reply({error, unauthorized})
    end;
handle_call(<<"/jwt/refresh">>, #{<<"refresh_jwt">> := RefreshJwt}, _State) ->
    case wispo_api_auth_jwt:is_jwt_refresh(RefreshJwt) of
        true ->
            case wispo_api_auth_jwt:verify(RefreshJwt) of
                true ->
                    {jose_jwt, #{<<"jid">> := Jid}} = jose_jwt:peek(RefreshJwt),
                    Jwt = wispo_api_auth_jwt:generate(#{<<"jid">> => Jid}),
                    Jwt2 = maps:put(jid, Jid, Jwt),
                    to_reply({ok, Jwt2});
                false ->
                    to_reply({error, invalid_token})
            end;
        false ->
            to_reply({error, invalid_token})
    end;
handle_call(<<"/health/check">>, _Data, _State) ->
    to_reply({ok, #{status => ok}});
handle_call(_Path, _Data, _State) ->
    to_reply({error, unexpected_call}).

%%% Internal functions

-spec to_reply(atom() | tuple()) ->
    pos_integer()
    | {pos_integer(), map()}.
to_reply(ok) ->
    ?HTTP_204_NO_CONTENT;
to_reply({ok, Data}) ->
    {?HTTP_200_OK, Data};
to_reply({error, invalid_token}) ->
    {?HTTP_401_UNAUTHORIZED, #{error_message => invalid_token}};
to_reply({error, unexpected_call}) ->
    {?HTTP_404_NOT_FOUND, #{error_message => unexpected_call}};
to_reply({error, invalid_params}) ->
    {?HTTP_400_BAD_REQUEST, #{error_message => invalid_params}};
to_reply({error, auth_code_mismatch}) ->
    {?HTTP_401_UNAUTHORIZED, #{error_message => auth_code_mismatch}};
to_reply({error, unauthorized}) ->
    {?HTTP_401_UNAUTHORIZED, #{error_message => unauthorized}};
to_reply({error, not_implemented}) ->
    {?HTTP_501_NOT_IMPLEMENTED, #{error_message => not_implemented}};
to_reply({error, Reason}) ->
    {?HTTP_400_BAD_REQUEST, #{error_message => Reason}};
to_reply(_Any) ->
    {?HTTP_500_INTERNAL_SERVER_ERROR, #{error_message => internal_error}}.

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
    {ok, #{code_ttl => CodeTtl, code_ttl_unit => seconds}}.

%% @private
-spec confirm_phone(phone(), code()) -> ok | {error, auth_code_mismatch}.
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
            wispo_api_contacts:insert_user(Jid, Phone),
            {ok, maps:put(jid, Jid, Jwt)};
        _ ->
            {error, auth_code_mismatch}
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