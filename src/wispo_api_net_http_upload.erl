-module(wispo_api_net_http_upload).

-export([init/2]).

init(Req, Opts) ->
  {ok, Headers, Req2} = cowboy_req:read_part(Req),
  {ok, Data, Req3} = cowboy_req:read_part_body(Req2),
  {file, <<"data">>, Filename, ContentType} = cow_multipart:form_data(Headers),
  io:format("Received file ~p of content-type ~p as follow:~n~p~n~n", [Filename, ContentType, Data]),
  Path = path(Filename),
  file:write_file(Path, Data),
  {ok, Req3, Opts}.

path(Filename) ->
  code:priv_dir(wispo_api) ++ "/files/" ++ binary_to_list(Filename).