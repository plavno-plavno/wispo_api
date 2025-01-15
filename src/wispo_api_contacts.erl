-module(wispo_api_contacts).

-export([
  insert_user/2,
  select_contacts/1
]).

-export([
  sync/2,
  get_synced/1,
  remove_synced/1
]).

-define(INSERT_USER, <<"INSERT INTO wispo_api_users (jid, phone) VALUES ($1, $2)">>).

-define(SELECT_PHONE_BY_JID, <<"SELECT phone FROM wispo_api_users WHERE jid = $1">>).
-define(SELECT_JID_BY_PHONE, <<"SELECT jid FROM wispo_api_users WHERE phone = $1">>).

-define(SELECT_CONTACTS, <<"SELECT jid, phone FROM wispo_api_users WHERE phone = ANY($1::varchar[])">>).

-spec insert_user(binary(), binary()) -> term().
insert_user(Jid, Phone) ->
  wispo_api_db_pg_worker:insert(?INSERT_USER, [Jid, Phone]).

-spec select_contacts(list()) -> list().
select_contacts(L) ->
  wispo_api_db_pg_worker:select(?SELECT_CONTACTS, [L]).

-spec sync(binary(), list(binary())) ->
  {ok, list()}
  | {error, term()}.
sync(_OwnerJid, Contacts) ->
  select_contacts(Contacts).

-spec get_synced(binary()) ->
  {ok, list()}
  | {error, term()}.
get_synced(_OwnerJid) ->
  {error, not_implemented}.

-spec remove_synced(binary()) ->
  ok
  | {error, term()}.
remove_synced(_OwnerJid) ->
  {error, not_implemented}.