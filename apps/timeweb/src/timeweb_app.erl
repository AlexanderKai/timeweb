-module(timeweb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	application:start(sasl),
	application:start(crypto),
	application:start(cowlib),
	application:start(ranch),
	application:start(cowboy),
	psql:start(),


	{ok, Settings} = application:get_env(timeweb, cowboy),
	Port = proplists:get_value(port, Settings, 80),
	Routes = cowboy_router:compile([
			{'_', [
			{"/api/user/register",			users_handler, #{request_name => register}},
			{"/api/user/sign_in",			users_handler, #{request_name => sign_in}},
			{"/api/user/change_password",	users_handler, #{request_name => change_password}},
			{"/api/users/list",				users_handler, #{request_name => list}}
				]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, Port}], #{
		env => #{dispatch => Routes}
	}),
		timeweb_sup:start_link().

stop(_State) ->
		ok.

%% internal functions
