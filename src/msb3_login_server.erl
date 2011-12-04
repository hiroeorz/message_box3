%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(msb3_login_server).

-behaviour(gen_server).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("user.hrl").

%% API
-export([start_link/1, stop/0, stop/1]).
-export([authenticate/2, login/2]).

%% Local API
-export([authenticate/3, login/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Name_Or_Args::list()|atom()) -> 
             {ok, Pid::pid()} | ignore | {error, Error::atom()}).

start_link(_Args) when is_list(_Args) ->
    gen_server:start_link(?MODULE, [], []);

start_link(Name) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

-spec(stop(Name_OR_Pid::atom()|pid()) -> ok ).

stop(Name_OR_Pid) ->
    gen_server:cast(Name_OR_Pid, {stop}).

-spec(stop() -> ok ).

stop() -> 
    gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc プロセスプールに対してauthenticate/3を要求する。
%% @end
%%--------------------------------------------------------------------
-spec(authenticate(Name::string(), Password::string()) -> 
             {ok, SessionKey::string()} | {error, password_incollect}).

authenticate(Name, Password) when is_list(Name) and is_list(Password) ->
    Worker = poolboy:checkout(msb3_login_server_pool),
    Reply = msb3_login_server:authenticate(Worker, Name, Password),
    poolboy:checkin(msb3_login_server_pool, Worker),
    Reply.

%%--------------------------------------------------------------------
%% @doc 認証を行い、認証にパスしたらセッションキーを新たに生成して返す。
%% @end
%%--------------------------------------------------------------------
-spec(authenticate(Pid::pid(), Name::string(), Password::string()) -> 
             {ok, SessionKey::string()} | {error, password_incollect}).

authenticate(Name_Or_Pid, Name, Password) when is_list(Name) and 
                                               is_list(Password) ->
    gen_server:call(Name_Or_Pid, {authenticate, Name, Password}).

%%--------------------------------------------------------------------
%% @doc 
%% ユーザIdとセッションの組み合わせが既に認証済みか確認する
%% 確認がとれた場合はセッションの有効期限を延ばす
%% @end
%%--------------------------------------------------------------------
login(UserId, SessionKey)  when is_integer(UserId) and is_list(SessionKey) ->
    Worker = poolboy:checkout(msb3_login_server_pool),
    Reply = msb3_login_server:login(Worker, UserId, SessionKey),
    poolboy:checkin(msb3_login_server_pool, Worker),
    Reply.

%%--------------------------------------------------------------------
%% @doc 
%% ユーザIdとセッションの組み合わせが既に認証済みか確認する
%% 確認がとれた場合はセッションの有効期限を延ばす
%% @end
%%--------------------------------------------------------------------
login(Name_Or_Pid, UserId, SessionKey)  when is_integer(UserId) and 
                                             is_list(SessionKey) ->
    gen_server:call(Name_Or_Pid, {login, UserId, SessionKey}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({authenticate, Name, Password}, _From, State) ->
    Reply = case msb3_user:get_user(Name) of
                {ok, User} ->
                    CryptedPass = 
                        msb3_util:create_crypted_password(User, Password),

                    if User#user.password == CryptedPass ->
                            Id = User#user.id,
                            SessionKey = msb3_util:create_session_key(Id,
                                                                   CryptedPass),
                            ok = msb3_session:add_new_session(Id, SessionKey),
                            ok = msb3_session:update_expire(Id, SessionKey),
                            {ok, SessionKey, Id};
                       true ->
                            {error, password_incollect}
                    end;
                _ ->
                    {error, password_incollect}
            end,

    {reply, Reply, State};

handle_call({login, UserId, SessionKey}, _From, State) ->
    Reply = case msb3_session:check_session_expire(UserId, SessionKey) of
                ok -> 
                    msb3_session:update_expire(UserId, SessionKey),
                    ok;
                expired ->
                    expired
            end,

    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({stop}, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
