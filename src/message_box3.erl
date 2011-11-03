%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 23 Oct 2011 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(message_box3).

-behaviour(gen_server).


%% Include
-include("user.hrl").

%% API
-export([start/0, stop/0, connect_dbsrv/1]).
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create_user/3, get_user/1, authenticate/2,
         get_home_timeline/3, get_mentions_timeline/3, get_sent_timeline/3,
         send_message/4, 
         follow/3, unfollow/3]).

-define(SERVER, ?MODULE). 

-record(state, {connection_count ::integer()}).

%%--------------------------------------------------------------------
%% @doc
%% Start Server。
%%
%% @end
%%--------------------------------------------------------------------
-spec(start() -> ok).

start() ->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% Stop Server。
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop() -> ok).

stop() ->
    application:stop(?MODULE).    

%%--------------------------------------------------------------------
%% @doc
%% Connect to eredis_pool database server.
%%
%% @end
%%--------------------------------------------------------------------
-spec(connect_dbsrv(Node::node()) -> true | false).

connect_dbsrv(Node) ->
    net_kernel:connect(Node).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() -> {ok, Pid::pid()} | ignore | {error, Error::atom()}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(start_link(_::list()) -> {ok, Pid::pid()} | ignore | 
                               {error, Error::atom()}).

start_link(_) ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% ユーザを新規に作成する。
%%
%% @end
%%--------------------------------------------------------------------
-spec(create_user(Name::string(), Mail::string(), Password::string()) -> 
             {ok, UserId::integer()} | {error, Reason::binary()}).

create_user(Name, Mail, Password) when is_list(Name) and is_list(Mail) and 
                                       is_list(Password) ->
    call(fun(Worker) ->
                 gen_server:call(Worker, {create_user, Name, Mail, Password})
         end).

%%--------------------------------------------------------------------
%% @doc
%% ユーザ情報を取得する
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_user(UserId::integer()) -> {ok, User::#user{}} | {error, not_found}).

get_user(UserId) when is_integer(UserId) ->
    call(fun(Worker) ->
                 gen_server:call(Worker, {get_user, UserId})
         end).

%%--------------------------------------------------------------------
%% @doc
%% 認証を実行し、以後要求を実行する為のセッションキーを得る。
%%
%% @end
%%--------------------------------------------------------------------
-spec(authenticate(Name::string(), Password::string()) -> 
             {ok, SessionKey::string()} | {error, password_incollect}).

authenticate(Name, Password) when is_list(Name) and is_list(Password) ->
    call(fun(Worker) ->
                 gen_server:call(Worker, {authenticate, Name, Password})
         end).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%% @todo 実装
%%--------------------------------------------------------------------

get_home_timeline(UserId, SessionKey, Count) ->
    call(fun(Worker) ->
                 gen_server:call(Worker, 
                                 {get_home_timeline, UserId, SessionKey, Count})
         end).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%% @todo 実装
%%--------------------------------------------------------------------

get_mentions_timeline(UserId, SessionKey, Count) ->
    call(fun(Worker) ->
                 gen_server:call(Worker, 
                                 {get_mentions_timeline, UserId, SessionKey, Count})
         end).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%% @todo 実装
%%--------------------------------------------------------------------

get_sent_timeline(UserId, SessionKey, Count) ->
    call(fun(Worker) ->
                 gen_server:call(Worker, 
                                 {get_sent_timeline, UserId, SessionKey, Count})
         end).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%% @todo 実装
%%--------------------------------------------------------------------
-spec(send_message(UserId::integer(), SessionKey::string(), Text::string(), 
                   InReplyTo::integer()|undefined) -> 
             {ok, MessageId::integer()} | {error, session_expired}).

send_message(UserId, SessionKey, Text, InReplyTo) when is_integer(UserId) and
                                                       is_list(SessionKey) and
                                                       is_list(Text) ->
    call(fun(Worker) ->
                 gen_server:call(Worker, 
                                 {send_message, UserId, SessionKey, Text, InReplyTo})
         end).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%% @todo 実装
%%--------------------------------------------------------------------
-spec(follow(UserId::integer(), SessionKey::string(), FollowUserId::integer()) -> 
             {ok, 
              {follow, FollowCount::integer()}, 
              {follower, FollowerCount::integer()}} | 
             {error, session_expired}).

follow(UserId, SessionKey, FollowUserId) when is_integer(UserId) and
                                              is_list(SessionKey) and
                                              is_integer(FollowUserId) ->
    call(fun(Worker) ->
                 gen_server:call(Worker, 
                                 {follow, UserId, SessionKey, FollowUserId})
         end).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%% @todo 実装
%%--------------------------------------------------------------------
-spec(unfollow(UserId::integer(), SessionKey::string(), FollowUserId::integer()) -> 
             {ok, 
              {follow, FollowCount::integer()}, 
              {follower, FollowerCount::integer()}} | 
             {error, session_expired}).

unfollow(UserId, SessionKey, UnFollowUserId) when is_integer(UserId) and
                                              is_list(SessionKey) and
                                              is_integer(UnFollowUserId) ->
    call(fun(Worker) ->
                 gen_server:call(Worker, 
                                 {unfollow, UserId, SessionKey, UnFollowUserId})
         end).


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
    {ok, #state{connection_count = 0}}.

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
handle_call({parallel, Fun}, From, State) ->
    spawn_link(fun() -> 
                       Reply = Fun(),
                       gen_server:reply(From, Reply)
               end),
    {noreply, State};

handle_call({create_user, Name, Mail, Password}, _From, State) ->
    Reply = msb3_user:add_user(Name, Mail, Password),
    {reply, Reply, State};

handle_call({get_user, UserId}, _From, State) ->
    Reply = case msb3_user:get_user(UserId) of
                {ok, User}         -> {ok, User#user{password = undefined}};
                {error, not_found} -> {error, not_found}
            end,
    {reply, Reply, State};

handle_call({authenticate, Name, Password}, _From, State) ->
    Reply = msb3_login_server:authenticate(Name, Password),
    {reply, Reply, State};

handle_call({get_home_timeline, UserId, SessionKey, Count}, _From, State) ->
    Reply = case msb3_login_server:login(UserId, SessionKey) of
                ok      -> home_timeline:get_timeline(UserId, Count);
                expired -> {error, session_expired}
            end,
    {reply, Reply, State};

handle_call({get_mentions_timeline, UserId, SessionKey, Count}, _From, State) ->
    Reply = case msb3_login_server:login(UserId, SessionKey) of
                ok      -> mentions_timeline:get_timeline(UserId, Count);
                expired -> {error, session_expired}
            end,
    {reply, Reply, State};

handle_call({get_sent_timeline, UserId, SessionKey, Count}, _From, State) ->
    Reply = case msb3_login_server:login(UserId, SessionKey) of
                ok      -> sent_timeline:get_timeline(UserId, Count);
                expired -> {error, session_expired}
            end,
    {reply, Reply, State};

handle_call({send_message, UserId, SessionKey, Text, InReplyTo}, _From, State) ->
    Reply = case msb3_login_server:login(UserId, SessionKey) of
                ok      -> 
                    {ok, MsgId, MsgKey} = 
                        message_send_server:add_message(UserId, Text, InReplyTo),
                    TextBin = list_to_binary(Text),
                    ok = mention_send_server:add_mention(MsgKey, TextBin),
                    ok = home_send_server:add_home_to_followers(UserId, MsgKey),
                    {ok, MsgId};
                expired -> 
                    {error, session_expired}
            end,
    {reply, Reply, State};

handle_call({follow, UserId, SessionKey, FollowUserId}, _From, State) ->
    Reply = case msb3_login_server:login(UserId, SessionKey) of
                ok -> 
                    {ok, FollowCount} = 
                        user_relation:add_follow(UserId, FollowUserId),
                    {ok, _} = user_relation:add_follower(FollowUserId, UserId),
                    FollowerCount = user_relation:follower_count(UserId),
                    {ok, {follow, FollowCount}, {follower, FollowerCount}};
                expired -> 
                    {error, session_expired}
            end,
    {reply, Reply, State};

handle_call({unfollow, UserId, SessionKey, FollowUserId}, _From, State) ->
    Reply = case msb3_login_server:login(UserId, SessionKey) of
                ok -> 
                    {ok, _} = user_relation:delete_follow(UserId, FollowUserId),
                    {ok, _} = user_relation:delete_follower(FollowUserId, UserId),
                    FollowCount = user_relation:follow_count(UserId),
                    FollowerCount = user_relation:follower_count(UserId),
                    {ok, {follow, FollowCount}, {follower, FollowerCount}};
                expired -> 
                    {error, session_expired}
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
handle_cast(_Request, State) ->
    {noreply, State}.

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

call(Fun) ->
    Worker = poolboy:checkout({global, msb3_worker_pool}),
    Reply = Fun(Worker),
    poolboy:checkin({global, msb3_worker_pool}, Worker),
    Reply.

