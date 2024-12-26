-ifndef(WISPO_API_UTILS_HRL).
-define(WISPO_API_UTILS_HRL, true).

-define(APP_NAME, wispo_api).
-define(WWW_ROOT, "www").

-ifdef(OTP_RELEASE).
  -if(?OTP_RELEASE >= 27).
    -define(HEX_ENCODE(Dec), binary:encode_hex(Dec)).
    -define(HEX_DECODE(Enc), binary:decode_hex(Enc)).
  -else.
    -define(HEX_ENCODE(Dec), collector_crypto_utils:hex_encode(Dec)).
    -define(HEX_DECODE(Enc), collector_crypto_utils:hex_decode(Enc)).
  -endif.
-else.
  -error("Unsupported OTP release").
-endif.

-define(UUIDv4BinStr, uuid:uuid_to_string(uuid:get_v4(strong), binary_standard)).

-define(LIB_JSON, jsx).
-define(LIB_TOML, tomerl).
-define(LIB_XML, xmerl).
-define(LIB_BASE64URL, jose_base64url). % cow_base64url | jose_base64url

-ifdef(LIB_JSON).
  -if(?LIB_JSON =:= jsx).
    -define(JSON_ENCODE(Term), jsx:encode(Term)).
    -define(JSON_DECODE(Json), jsx:decode(Json, [return_maps])).
  -elif(?LIB_JSON =:= jiffy).
    -define(JSON_ENCODE(Term), jiffy:encode(Term)).
    -define(JSON_DECODE(Json), jiffy:decode(Json, [return_maps])).
  -elif(?LIB_JSON =:= jsone).
    -define(JSON_ENCODE(Term), jsone:encode(Term)).
    -define(JSON_DECODE(Json), jsone:decode(Json, [{object_format, map}])).
  -endif.
-endif.

-ifdef(LIB_XML).
  -if(?LIB_XML =:= xmerl).
    -define(XML_ENCODE(X), erlang:iolist_to_binary(xmerl:export_simple([X], xmerl_xml, [{encoding, 'utf-8'}]))).
    -define(XML_DECODE(X), begin {Rec, []} = xmerl_scan:string(X, [{document, true}, {comments, false}, {space, normalize}]), Rec end).
  -elif(?LIB_XML =:= exml).
    -define(XML_ENCODE(X), exml:to_binary(X)).
    -define(XML_DECODE(X), exml:parse(X)).
  -endif.
-endif.

-ifdef(LIB_TOML).
  -if(?LIB_TOML =:= tomerl).
    -define(TOML_READ(Path), tomerl:read_file(Path)).
  -endif.
-endif.

-ifdef(LIB_BASE64URL).
  -if(?LIB_BASE64URL =:= cow_base64url).
    -define(BASE64URL_ENCODE(Dec), cow_base64url:encode(Dec)).
    -define(BASE64URL_DECODE(Enc), cow_base64url:decode(Enc)).
  -elif(?LIB_BASE64URL =:= jose_base64url).
    -define(BASE64URL_ENCODE(Dec), jose_base64url:encode(Dec)).
    -define(BASE64URL_DECODE(Enc), jose_base64url:decode(Enc)).
  -endif.
-endif.

%%% Time Utils
%%% 1 Day: 86,400 seconds.
%%% 1 Week: 604,800 seconds.
%%% 1 Month: 2,629,743 seconds (30.44 days on average)
%%% 1 Year: 31,556,926 seconds (365.24 days)
-define(SECONDS_PER_MINUTE, 60).
-define(SECONDS_PER_HOUR, 3600).
-define(SECONDS_PER_DAY, 86400).
-define(SECONDS_PER_WEEK, 604800).
-define(SECONDS_PER_YEAR, 31536000).
-define(SECONDS_PER_LEAP_YEAR, 31622400).
-define(DAYS_PER_YEAR, 365).
-define(DAYS_PER_LEAP_YEAR, 366).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(SECONDS_FROM_0_TO_1970, (?DAYS_FROM_0_TO_1970 * ?SECONDS_PER_DAY)).
-define(EPOCH_ZERO, ?SECONDS_FROM_0_TO_1970).

%% Think: (is_integer(Year), Year >= 0, Year rem 4 =:= 0, Year rem 100 > 0) orelse (is_integer(Year), Year >= 0, Year rem 400 =:= 0)
-define(IS_LEAP_YEAR(Year),
  (case Year of
     Year when erlang:is_integer(Year), Year >= 0, Year rem 4 =:= 0, Year rem 100 > 0 ->
       true;
     Year when erlang:is_integer(Year), Year >= 0, Year rem 400 =:= 0 ->
       true;
     _ ->
       false
   end)).

%% -------------------------------------------------------------------
%% @doc
%% Helper macro for supervisors.
%%
%% Example:
%%
%% ```
%% ?WORKER(foo, [Config]),
%% ```
%% @end
%% -------------------------------------------------------------------
-define(WORKER(Mod, Args), {Mod, {Mod, start_link, Args}, transient, brutal_kill, worker, [Mod]}).
-define(WORKER(Mod), ?WORKER(Mod, [])).

-define(SUP(Mod, Args), {Mod, {Mod, start_link, Args}, permanent, infinity, supervisor, [Mod]}).
-define(SUP(Mod), ?SUP(Mod, [])).

%%-define(WORKER(Mod), ?WORKER(Mod, start_link)).
%%-define(WORKER(Mod, Fun), ?WORKER(Mod, Fun, [])).
%%-define(WORKER(Mod, Fun, Args),
%%  #{
%%    id => Mod,
%%    start => {Mod, Fun, Args},
%%    restart => transient,
%%    shutdown => 5000,
%%    type => worker,
%%    modules => [Mod]
%%  }).
%%
%%-define(SUPERVISOR(Mod), ?SUPERVISOR(Mod, start_link)).
%%-define(SUPERVISOR(Mod, Fun), ?SUPERVISOR(Mod, Fun, [])).
%%-define(SUPERVISOR(Mod, Fun, Args),
%%  #{
%%    id => Mod,
%%    start => {Mod, Fun, Args},
%%    restart => permanent,
%%    shutdown => infinity,
%%    type => supervisor,
%%    modules => [Mod]
%%  }).

%% -------------------------------------------------------------------
%% @doc
%% Usage:
%% ...
%% User = #user{...},
%% ToList = ?rec_to_proplist(user),
%% L = ToList(User),
%% ...
%% @todo Cache function with using persistent_term module
%% @end
%% -------------------------------------------------------------------
-define(rec_to_proplist(RecName),
  fun(Rec) ->
    Fields = record_info(fields, RecName),
    [_ | Values] = erlang:tuple_to_list(Rec),
    lists:zip(Fields, Values)
  end).

%% TODO: rec_from_proplist(RecTag, Proplist)

%% -------------------------------------------------------------------
%% @doc
%% Usage:
%% ...
%% User = #user{...},
%% ToMap = ?rec_to_map(user),
%% M = ToMap(User),
%% ...
%% @end
%% -------------------------------------------------------------------
-define(rec_to_map(RecName),
  fun(Rec) ->
    Fields = record_info(fields, RecName),
    [_ | Values] = erlang:tuple_to_list(Rec),
    maps:from_list(lists:zip(Fields, Values))
  end).

%% NOTE: proplists:proplist() -> #partner{}
%%
%% L1 = [{swarm_site_id, 123}, ...].
%% L2 = [V || {K, V} <- L1].
%% Partner = erlang:list_to_tuple([partner | lists:sublist(L2, size(#partner{})-1)]).

%-type iso3166_1_alpha_2() :: binary().
%-type iso3166_1_alpha_3() :: binary().
%-type iso3166_1_numeric() :: non_neg_integer().
%-type iso3166_1() :: iso3166_1_alpha_2() | iso3166_1_alpha_3() | iso3166_1_numeric().
%-type nationality() :: iso3166_1().
%-type citizenship() :: iso3166_1().
%-type person() :: maps:map(). % first name, middle name, last name | name, surname, nickname; birth date; photos; height; weight

%-type country() :: iso3166_1().

%-type tag() :: binary().

%% Attitude, Point of view, viewpoint, position, stance, treatment, opinion
%% Personal attitude to the event
%% Person's attitude to the event
%% Official attitude to the event
%% rather positive
%-type attitude() :: negative | neutral | positive.

-endif.
