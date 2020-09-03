-module(user_worker).

-behaviour(gen_server).

-include("timeweb.hrl").

-compile([export_all]).

-define(SERVER, ?MODULE).

-record(state, {
	user = #{} :: map(),
	token = <<>> :: binary()
}).

-define(USER_SESSION_TIMEOUT_MS, ?USER_SESSION_TIMEOUT * 1000).

%%%===================================================================
%%% API
%%%===================================================================

start_link({User, Token} = Args) ->
	gen_server:start_link({global, Token}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({User, Token}) ->
	process_flag(trap_exit, true),
	self() ! {init, User, Token},
	{ok, #{login => User}, ?USER_SESSION_TIMEOUT_MS}.


handle_call({call, T, M, F, A}, _From, #{token:=T} = State) ->
	Res = erlang:apply(M, F, A),
	{reply, Res, State, ?USER_SESSION_TIMEOUT_MS};

handle_call(_Request, _From, State) ->
	{reply, ok, State, ?USER_SESSION_TIMEOUT_MS}.

handle_info({init, User, Token}, State) ->
	Pid = self(),
	ets:insert(timeweb_users_sessions, [{Token, #{pid => Pid}}]),
	SessionData = retrieve_session_data(User),
	ets:insert(timeweb_users_sessions_data, [{Token, SessionData}]),
	NewState = State#{token => Token, pid => Pid},
	{noreply, NewState, ?USER_SESSION_TIMEOUT_MS};

handle_info(timeout, #{token:=Token} = State) ->
	ets:delete(timeweb_users_sessions, Token),
	ets:delete(timeweb_users_sessions_data, Token),
	ok = gen_server:stop(self()), 
	DeleteChild = supervisor:delete_child(timeweb_sup, Token),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State, ?USER_SESSION_TIMEOUT_MS}.

handle_cast(_Request, State) ->
	{noreply, State, ?USER_SESSION_TIMEOUT_MS}.

zerminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

call(P,T,{M,F,A}) ->
	gen_server:call(P, {call, T, M, F, A}).

get_session_data(Token, Key) ->
	Res = ets:lookup(timeweb_users_sessions_data, Token),
	case Res of
		[] -> undefined;
		[{Token, #{Key := Value}}] -> Value
	end.
	

retrieve_session_data(Login) ->
	{ok, _, [{_, Password}]} = psql:equery(main, "
	SELECT
	login,
	password
	FROM
	users
	WHERE
	login = $1", [Login]),
	#{login => Login, password => Password}.


