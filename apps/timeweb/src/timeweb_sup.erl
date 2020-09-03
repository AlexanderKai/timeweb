-module(timeweb_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	AuthSrvSpec = #{
		id		=> auth_srv,
		start	=> {auth_srv, start_link, []},
		restart	=> permanent,
		shutdown => infinity,
		type	=> worker,
		modules	=> []
	},
	UsersSessionsSpec = #{
		id		=> users_sessions_sup,
		start	=> {timeweb_users_sup, start_link, []},
		restart	=> permanent,
		shutdown => 5000,
		type	=> supervisor
	},
	{ok, {{one_for_one, 10, 10}, [AuthSrvSpec, UsersSessionsSpec]}}.


