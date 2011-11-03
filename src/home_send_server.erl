%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2011 by Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(home_send_server).

-behaviour(gen_server).

%% Include
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1, stop/0, stop/1]).
-export([add_home_to_followers/2, add_home_to_followers/3]).

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
%% @doc 
%% プロセスプールからワーカーを一つ取り出して以下の処理を行います。
%% 受け取ったIdのユーザの各フォロワーのhomeにメッセージIdを保存する。
%% 処理は非同期に行います。
%% @end
%%--------------------------------------------------------------------
-spec(add_home_to_followers(UserId::integer(), MsgKey::binary()) -> ok).

add_home_to_followers(UserId, MsgKey) when is_integer(UserId) and 
                                           is_binary(MsgKey) ->
    Worker = poolboy:checkout(home_send_server_pool),
    Reply = home_send_server:add_home_to_followers(Worker, UserId, MsgKey),
    poolboy:checkin(home_send_server_pool, Worker),
    Reply.

%%--------------------------------------------------------------------
%% @doc 
%% 受け取ったIdのユーザの全フォロワーのhomeに受け取ったメッセージへのキーを保存する。
%% 処理は非同期に行います。
%% @end
%%--------------------------------------------------------------------
-spec(add_home_to_followers(Name_OR_Pid::pid()|atom(), 
                            UserId::integer(), MsgKey::binary()) -> ok).

add_home_to_followers(Name_OR_Pid, UserId, MsgKey) when is_integer(UserId) and 
                                                        is_binary(MsgKey) ->
    gen_server:cast(Name_OR_Pid, {add_home_to_followers, UserId, MsgKey}).

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
handle_call(_Request, _From, State) ->
    Reply = ok,
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
    {stop, normal, State};

handle_cast({add_home_to_followers, UserId, MsgKey}, State) ->
    Followers = user_relation:get_followers(UserId),
    lists:map(fun(Id) ->
                      home_timeline:add_message_key(Id, MsgKey)
              end, [UserId | Followers]),

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
