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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create_user/3, get_user/1, authenticate/2,
         get_home_timeline/3, get_mentions_timeline/3, get_sent_timeline/3,
         send_message/4, 
         follow/3, unfollow/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%--------------------------------------------------------------------
%% @doc
%% Start Server。
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.

start() ->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% Stop Server。
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.

stop() ->
    application:stop(?MODULE).    

%%--------------------------------------------------------------------
%% @doc
%% Connect to eredis_pool database server.
%%
%% @end
%%--------------------------------------------------------------------
-spec connect_dbsrv(Node) -> true | false when
      Node::node().

connect_dbsrv(Node) ->
    net_kernel:connect(Node).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, Pid} | ignore | {error, Error} when
      Pid :: pid(),
      Error :: atom().
      
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% ユーザを新規に作成する。
%%
%% @end
%%--------------------------------------------------------------------
-spec create_user(Name, Mail, Password) -> {ok, UserId} | {error, Reason} when
      Name :: string(), 
      Mail :: string(), 
      Password :: string(),
      UserId :: integer(),
      Reason :: binary().

create_user(Name, Mail, Password) ->
    gen_server:call(?SERVER, {create_user, Name, Mail, Password}).

%%--------------------------------------------------------------------
%% @doc
%% ユーザ情報を取得する
%%
%% @end
%%--------------------------------------------------------------------
-spec get_user(UserId) -> {ok, User} | {error, not_found} when
      UserId :: integer(),
      User :: #user{}.

get_user(UserId) ->
    gen_server:call(?SERVER, {get_user, UserId}).

%%--------------------------------------------------------------------
%% @doc
%% 認証を実行し、以後要求を実行する為のセッションキーを得る。
%%
%% @end
%%--------------------------------------------------------------------
-spec authenticate(Name, Password) -> {ok, SessionKey} | {error, password_incollect} when
      Name :: string(), 
      Password :: string(),
      SessionKey :: string().

authenticate(Name, Password) ->
    gen_server:call(?SERVER, {authenticate, Name, Password}).

%%--------------------------------------------------------------------
%% @doc
%% ユーザーのホームタイムラインを取得する。
%%
%% @end
%%--------------------------------------------------------------------
-spec get_home_timeline(UserId, SessionKey, Count)
                       -> {ok, [tuple()]} | {error, Reason} when
      UserId :: integer(), 
      SessionKey :: string(), 
      Count :: integer(),
      Reason :: binary().

get_home_timeline(UserId, SessionKey, Count) ->
    gen_server:call(?SERVER, {get_home_timeline, UserId, SessionKey, Count}).

%%--------------------------------------------------------------------
%% @doc
%% ユーザーのリプライタイムラインを取得する。
%%
%% @end
%%--------------------------------------------------------------------
-spec get_mentions_timeline(UserId, SessionKey, Count)
                           -> {ok, [tuple()]} | {error, Reason::binary()} when
      UserId :: integer(), 
      SessionKey :: string(), 
      Count :: integer().

get_mentions_timeline(UserId, SessionKey, Count) ->
    gen_server:call(?SERVER, {get_mentions_timeline, UserId, SessionKey, Count}).

%%--------------------------------------------------------------------
%% @doc
%% ユーザーの送信タイムラインを取得する。
%%
%% @end
%%--------------------------------------------------------------------
-spec get_sent_timeline(UserId, SessionKey, Count) 
                       -> {ok, [tuple()]} | {error, Reason} when
      UserId :: integer(), 
      SessionKey :: string(), 
      Count :: integer(),
      Reason :: binary().

get_sent_timeline(UserId, SessionKey, Count) ->
    gen_server:call(?SERVER, {get_sent_timeline, UserId, SessionKey, Count}).

%%--------------------------------------------------------------------
%% @doc
%% メッセージを送信する。
%%
%% @end
%%--------------------------------------------------------------------
-spec send_message(UserId, SessionKey, Text, 
                   InReplyTo|undefined) -> 
             {ok, MessageId} | {error, session_expired} when
      UserId :: integer(), 
      SessionKey :: string(), 
      Text :: string(), 
      InReplyTo :: integer(),
      MessageId :: integer().

send_message(UserId, SessionKey, Text, InReplyTo) when is_integer(UserId) and
                                                       is_list(SessionKey) and
                                                       is_list(Text) ->
    gen_server:call(?SERVER, {send_message, UserId, SessionKey, Text, InReplyTo}).

%%--------------------------------------------------------------------
%% @doc
%% 他のユーザをフォローする。
%%
%% @end
%% @todo 実装
%%--------------------------------------------------------------------
-spec follow(UserId, SessionKey, FollowUserId) -> 
                    {ok, 
                     {follow, FollowCount}, 
                     {follower, FollowerCount}} | 
                    {error, session_expired} when
      UserId :: integer(), 
      SessionKey :: string(), 
      FollowUserId :: integer(),
      FollowCount :: integer(),
      FollowerCount :: integer().


follow(UserId, SessionKey, FollowUserId) when is_integer(UserId) and
                                              is_list(SessionKey) and
                                              is_integer(FollowUserId) ->
    gen_server:call(?SERVER, {follow, UserId, SessionKey, FollowUserId}).

%%--------------------------------------------------------------------
%% @doc
%% フォロー中のユーザをフォローリストから外す。
%%
%% @end
%%--------------------------------------------------------------------
-spec unfollow(UserId, SessionKey, FollowUserId) -> 
                      {ok, 
                       {follow, FollowCount}, 
                       {follower, FollowerCount}} | 
                      {error, session_expired} when
      UserId :: integer(), 
      SessionKey :: string(), 
      FollowUserId :: integer(),
      FollowCount :: integer(),
      FollowerCount :: integer().

unfollow(UserId, SessionKey, UnFollowUserId) when is_integer(UserId) and
                                              is_list(SessionKey) and
                                              is_integer(UnFollowUserId) ->
    gen_server:call(?SERVER, {unfollow, UserId, SessionKey, UnFollowUserId}).

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
handle_call({create_user, Name, Mail, Password}, From, State) ->
    spawn(fun() ->
                  
                  Reply = msb3_user:add_user(Name, Mail, Password),
                  gen_server:reply(From, Reply)
          end),
    {noreply, State};

handle_call({get_user, UserId}, From, State) ->
    spawn(fun() ->
                  Reply = case msb3_user:get_user(UserId) of
                              {ok, User}         -> {ok, User#user{password = undefined}};
                              {error, not_found} -> {error, not_found}
                          end,
                  gen_server:reply(From, Reply)
          end),
    {noreply, State};

handle_call({authenticate, Name, Password}, From, State) ->
    spawn(fun() ->
                  Reply = msb3_login_server:authenticate(Name, Password),
                  gen_server:reply(From, Reply)
          end),

    {noreply, State};

handle_call({get_home_timeline, UserId, SessionKey, Count}, From, State) ->
    spawn(fun() ->
                  Reply = case msb3_login_server:login(UserId, SessionKey) of
                              ok      -> home_timeline:get_timeline(UserId, Count);
                              expired -> {error, session_expired}
                          end,
                  gen_server:reply(From, Reply)
          end),
    {noreply, State};

handle_call({get_mentions_timeline, UserId, SessionKey, Count}, From, State) ->
    spawn(fun() ->
                  Reply = case msb3_login_server:login(UserId, SessionKey) of
                              ok      -> mentions_timeline:get_timeline(UserId, Count);
                              expired -> {error, session_expired}
                          end,
                  gen_server:reply(From, Reply)
          end),
    {noreply, State};

handle_call({get_sent_timeline, UserId, SessionKey, Count}, From, State) ->
    spawn(fun() ->
                  Reply = case msb3_login_server:login(UserId, SessionKey) of
                              ok      -> sent_timeline:get_timeline(UserId, Count);
                              expired -> {error, session_expired}
                          end,
                  gen_server:reply(From, Reply)
          end),
    {noreply, State};

handle_call({send_message, UserId, SessionKey, Text, InReplyTo}, From, State) ->
    spawn(fun() ->
                  Reply = case msb3_login_server:login(UserId, SessionKey) of
                              ok -> 
                                  {ok, MsgId, MsgKey} =
                                      message:save_message(UserId, Text, InReplyTo),
                                  
                                  TextBin = list_to_binary(Text),
                                  spawn(fun() ->
                                                mentions_timeline:add_mention(MsgKey, TextBin)
                                        end),
                                  spawn(fun() -> 
                                                home_timeline:add_home_to_followers(UserId, 
                                                                                    MsgKey)
                                        end),
                                  {ok, MsgId};
                              expired -> 
                                  {error, session_expired}
                          end,
                  gen_server:reply(From, Reply)
          end),
    {noreply, State};

handle_call({follow, UserId, SessionKey, FollowUserId}, From, State) ->
    spawn(fun() ->
                  Reply = 
                      case msb3_login_server:login(UserId, SessionKey) of
                          ok -> 
                              {ok, FollowCount} = 
                                  user_relation:add_follow(UserId, FollowUserId),
                              {ok, _} = user_relation:add_follower(FollowUserId, UserId),
                              FollowerCount = user_relation:follower_count(UserId),
                              {ok, {follow, FollowCount}, {follower, FollowerCount}};
                          expired -> 
                              {error, session_expired}
                      end,
                  gen_server:reply(From, Reply)
          end),                  
    {noreply, State};

handle_call({unfollow, UserId, SessionKey, FollowUserId}, From, State) ->
    spawn(fun() ->
                  Reply = 
                      case msb3_login_server:login(UserId, SessionKey) of
                          ok -> 
                              {ok, _} = user_relation:delete_follow(UserId, FollowUserId),
                              {ok, _} = user_relation:delete_follower(FollowUserId, UserId),
                              FollowCount = user_relation:follow_count(UserId),
                              FollowerCount = user_relation:follower_count(UserId),
                              {ok, {follow, FollowCount}, {follower, FollowerCount}};
                          expired -> 
                              {error, session_expired}
                      end,
                  gen_server:reply(From, Reply)
          end),
    {noreply, State}.

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
