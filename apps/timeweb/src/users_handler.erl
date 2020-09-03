-module(users_handler).

-include("timeweb.hrl").

-compile([export_all]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	api:allowed_methods(Req, State).

content_types_provided(Req, State) ->
	api:content_types(Req, State).	

content_types_accepted(Req, State) ->
	api:content_types(Req, State).	


is_authorized(Req, #{request_name := register} = State) ->
	{true, Req, State};

is_authorized(Req, #{request_name := sign_in} = State) ->
	{true, Req, State};

is_authorized(Req, State) ->
	Cookies = cowboy_req:parse_cookies(Req),
	Token = proplists:get_value(<<"session_id">>, Cookies, <<>>),
	{IsAuth, PidSession} = auth_srv:is_authorized(Token),
	Req2 = case IsAuth of
		true ->
			Req;
		false ->
			cowboy_req:set_resp_cookie(<<"session_id">>, <<>>, Req, #{max_age => 0})
	end,	
	{IsAuth, Req2, State#{pid => PidSession, token => Token}}.


handle_request(Req, State) ->
	api:handle_request(?MODULE, Req, State).

handle_request(<<"GET">>, 
				#{request_name := list, pid := Pid, token := Token} = State, 
				Options) ->
	api:call(Pid, Token, {users_handler, list, []});

handle_request(<<"POST">>, 
				#{request_name := register}, 
				#{<<"login">> := Login, <<"password">> := Password}) ->
	register_user(Login, Password);

handle_request(<<"POST">>, 
				#{request_name := sign_in}, 
				#{<<"login">> := Login, <<"password">> := Password}) ->
	sign_in(Login, Password);

handle_request(<<"POST">>, 
				#{request_name := change_password}, 
				#{<<"new">> := OldPassword, <<"old">> := OldPassword}) ->
	{error, ?E_CANNOT_CHANGE_SAME_PASSWORD};

handle_request(<<"POST">>, 
				#{request_name := change_password, token := Token, pid := Pid}, 
				#{<<"new">> := NewPassword, <<"old">> := OldPassword}) ->
	api:call(Pid, Token, {users_handler, change_password, [Token, NewPassword, OldPassword]});

handle_request(_, _, _) ->
	?ERROR(?E_INVALID_REQUEST).

% API implementation

change_password(Token, New, Old) ->
	Login = user_worker:get_session_data(Token, login),
	Hash = timeweb_utils:md5_hex(Old),
	Res = psql:equery(main, "
	UPDATE
	users
	SET
	password = $2
	WHERE
	login = $1 AND password = $3",
	[Login, timeweb_utils:md5_hex(New), Hash]),
	case Res of
		{ok, 1} ->
			ok;
		_ ->
			{error, ?E_INTERNAL_DB_ERROR}
	end.

register_user(Login, Password) ->
	Hash = timeweb_utils:md5_hex(Password),
	Res = psql:equery(main, "
	INSERT INTO 
	users 
	(login, password) 
	VALUES ($1, $2)
	",
	[Login, Hash]),
	case Res of
		{ok, 1} ->
			ok;
		_ ->
			{error, ?E_CANNOT_REGISTER}
	end.

sign_in(Login, Password) ->
	Hash = timeweb_utils:md5_hex(Password),
	Res = psql:equery(main, "
	SELECT
	'ok'
	FROM	
	users
	WHERE
	login = $1 AND password = $2",
	[Login, Hash]),
	case Res of
		{ok, _, [{<<"ok">>}]} ->
			Token = timeweb_utils:get_token(),
			auth_srv:sign_in(Login, Token),
			{ok, [], #{cookie => Token}};
		_ ->
			{error, ?E_WRONG_LOGIN_OR_PASSWORD}
	end.

list() ->
	Res = psql:equery(main, "
	SELECT
	login
	FROM	
	users
	",
	[]),
	case Res of
		{ok, _, List} ->
			Data = [ Login || {Login} <- List],
			{ok, #{users =>Data}};
		_ ->
			{error, ?E_INTERNAL_DB_ERROR}
	end.
