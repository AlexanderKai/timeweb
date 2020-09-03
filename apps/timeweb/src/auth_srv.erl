-module(auth_srv).

-behaviour(gen_server).

-include("timeweb.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3, is_authorized/1, sign_in/2]).

-define(SERVER, ?MODULE).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	self() ! init,
	{ok, #{}}.

handle_call({sign_in, User, Token}, From, State) ->
	sign_in_do(User, Token, From),
	{noreply, State};

handle_call({is_authorized, Token}, From, State) ->
	is_authorized_do(Token, From),
	{noreply, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_info(init, State) ->

	ets:new(timeweb_users_sessions, [named_table, set, public, {write_concurrency, true}, {read_concurrency, true}]),
	ets:new(timeweb_users_sessions_data, [named_table, set, public, {write_concurrency, true}, {read_concurrency, true}]),
	{noreply, State};
	
handle_info(_Info, State) ->
	{noreply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%


is_authorized(Token) ->
	gen_server:call(?MODULE, {is_authorized, Token}).

is_authorized_do(Token, From) ->
	spawn(fun() ->
		User = ets:lookup(timeweb_users_sessions, Token),
		case User of
			[{Token, #{pid := Pid}}] ->
				gen_server:reply(From, {true, Pid});
			_ ->
				gen_server:reply(From, {false, undefined})
		end
	end).



sign_in(User, Token) ->
	gen_server:call(?MODULE, {sign_in, User, Token}).

sign_in_do(User, Token, From) ->
spawn(fun() ->
	Child = supervisor:start_child(timeweb_users_sup, [{User, Token}]),
	case Child of
		{ok, Pid} ->
			gen_server:reply(From, {ok, Token});
		E ->
			gen_server:reply(From, {error, ?E_CANNOT_CREATE_USER_SESSION})
	end
end).
