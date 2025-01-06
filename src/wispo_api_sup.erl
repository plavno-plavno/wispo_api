-module(wispo_api_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include_lib("kernel/include/logger.hrl").
-include("wispo_api_common_utils.hrl").
-include("wispo_api_ets.hrl").

start_link() ->
	Ret = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	?ETS_NAME = ets:new(?ETS_NAME, ?ETS_OPTS),
	?LOG_INFO("CONFIG: ~p", [wispo_api_config:get()]),
	case wispo_api_net_http:start() of
		{ok, _Pid} ->
			Ret;
		{error, Reason} ->
			{error, Reason}
	end.

init(Args) ->
	?LOG_INFO("Args: ~p", [Args]),
	SupFlags = #{
		strategy => one_for_one,
		intensity => 1,
		period => 5
	},
	ChildSpecs = [
		%?WORKER(wispo_api_db_pg_worker, [Args])
	],
	{ok, {SupFlags, ChildSpecs}}.

