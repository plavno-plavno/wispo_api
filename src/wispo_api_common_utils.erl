-module(wispo_api_common_utils).

-export([
  rand_code/0,
  rand_code/1
]).

-export([
  uuid7/0
]).

-export([
  parse_ip/1
]).

-export([
  is_valid_phone/1
]).

-export([
  reverse_bin/1,
  cut_n_last_numbers/2
]).

-define(DEFAULT_CODE_LENGTH, 4).

-type uuid_string() :: <<_:256>> | <<_:288>>.
-export_type([uuid_string/0]).

-type uuid() :: <<_:128>>.
-export_type([uuid/0]).

-define(UUIDv7Ver, 2#0111).
-define(UUIDv7Var, 2#10).

-spec rand_code() -> list(non_neg_integer()).
rand_code() ->
  Config = wispo_api_config:get(wispo_api, auth_code),
  Len = proplists:get_value(code_len, Config, ?DEFAULT_CODE_LENGTH),
  rand_code(Len).

-spec rand_code(pos_integer()) -> [non_neg_integer()].
rand_code(Len) ->
  rand_code(Len, []).

%% @private
-spec rand_code(non_neg_integer(), list()) -> list().
rand_code(0, Acc) ->
  Acc;
rand_code(N, Acc) ->
  rand_code(N - 1, [case rand:uniform(10) of 10 -> 0; X -> X end | Acc]).

-spec uuid7() -> uuid_string().
uuid7() ->
  <<RandA:12, RandB:62, _:6>> = crypto:strong_rand_bytes(10),
  uuid_to_string(<<(os:system_time(millisecond)):48, ?UUIDv7Ver:4, RandA:12, ?UUIDv7Var:2, RandB:62>>).

-doc(#{equiv => uuid_to_string(Bin, binary_standard)}).
-spec uuid_to_string(uuid()) -> uuid_string().
uuid_to_string(Bin) ->
  uuid_to_string(Bin, binary_standard).

-spec uuid_to_string(uuid(), binary_standard | binary_nodash) -> uuid_string().
uuid_to_string(Bin, binary_standard) ->
  uuid:uuid_to_string(Bin, binary_standard);
uuid_to_string(Bin, binary_nodash) ->
  binary:encode_hex(Bin, lowercase).

-spec parse_ip(binary() | string()) -> {ok, inet:ip_address()} | {error, term()}.
parse_ip(Ip) when is_list(Ip) ->
  inet_parse:strict_address(Ip);
parse_ip(Ip) when is_binary(Ip) ->
  parse_ip(binary_to_list(Ip));
parse_ip(_) ->
  {error, badarg}.

-spec is_valid_phone(binary()) -> boolean().
is_valid_phone(_Phone) ->
  {error, not_implemented}.

-spec reverse_bin(binary()) -> binary().
reverse_bin(Binary) ->
  Size = erlang:size(Binary) * 8,
  <<X:Size/integer-little>> = Binary,
  <<X:Size/integer-big>>.

%% Phone = wispo_api_common_utils:reverse_bin(<<"+1234567890">>).
%% Num = wispo_api_common_utils:cut_n_last_numbers(4, Phone).
-spec cut_n_last_numbers(pos_integer(), binary()) -> binary().
cut_n_last_numbers(N, Subject) ->
  case re:run(Subject, <<"[0-9]">>, [global, report_errors, {capture, all, binary}]) of
    {match, L} ->
      L2 = [X || [X] <- L],
      cut_n_numbers(N, L2, []);
    nomatch ->
      nomatch
  end.

-spec cut_n_numbers(non_neg_integer(), list(), list()) -> binary().
cut_n_numbers(0, _L, Acc) ->
  list_to_binary(Acc);
cut_n_numbers(N, [H|T], Acc) ->
  cut_n_numbers(N-1, T, [H|Acc]).