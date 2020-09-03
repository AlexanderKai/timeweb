-module(api).

-compile([export_all]).

-include("timeweb.hrl").

call(Pid, Token, {M,F,A}) ->
	user_worker:call(Pid, Token, {M, F, A}).

handle_request(Module, Req, State) ->
	Method = cowboy_req:method(Req),
	{ok, RawBody, _} = cowboy_req:read_body(Req),
	Body = case RawBody of
		<<>> -> #{};
		RawBody -> timeweb_utils:json_decode(RawBody)
	end,
	Response = Module:handle_request(Method, State, Body),
	{Response2, Extra} = handle_response(Response),
	Cookie = maps:get(cookie, Extra, undefined),
	Req2 = case Cookie of
		undefined ->
			Req;
		_ ->
			cowboy_req:set_resp_cookie(<<"session_id">>, Cookie, Req, #{max_age => ?USER_SESSION_TIMEOUT, path => "/", http_only => true})
	end,
			
	case Method of
		<<"GET">> ->
			{jsone:encode(Response2), Req2, State};
		_ ->
			Req3 = cowboy_req:set_resp_body(jsone:encode(Response2), Req2),
			{true, Req3, State}
	end.

handle_response(Response) ->
	case Response of
		ok ->
			{#{result => <<"success">>}, #{}};
		{ok, Data} ->
			{#{result => <<"success">>, data => Data}, #{}};
		{ok, Data, Extra} ->
			{#{result => <<"success">>, data => Data}, Extra};
		error ->
			{#{result => <<"fail">>}, #{}};
		{error, Error} ->
			{#{result => <<"fail">>, error => ?ERROR(Error)}, #{}}
	end.

content_types(Req, State) ->
	{[{<<"application/json">>, handle_request} ], Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

