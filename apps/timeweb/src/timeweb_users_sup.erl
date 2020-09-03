-module(timeweb_users_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(Args) ->
	Child = #{id => user_worker,
		start => {user_worker, start_link, Args},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [user_worker]},

	{ok, {#{strategy => simple_one_for_one,
		intensity => 2,
		period => 10},
		[Child]}
	}.
