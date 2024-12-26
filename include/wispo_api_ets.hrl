-ifndef(WISPO_API_ETS).
-define(WISPO_API_ETS, true).

-define(ETS_NAME, wispo_api_phones).
-define(ETS_OPTS, [public, named_table, {read_concurrency, true}, {write_concurrency, true}, {keypos, #wispo_api_phone.phone}]).

-record(wispo_api_phone, {
    phone 					:: binary(),
    code					:: term(),
    code_exp				:: term(),
    is_confirmed = false	:: boolean()
}).

-endif.