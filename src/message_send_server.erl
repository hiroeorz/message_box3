%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(message_send_server).

-behaviour(gen_server).

%% Include
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1, stop/0, stop/1]).
-export([add_message/3, add_message/4]).

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
%% プロセスプールからワーカーを一つ取り出してadd_message/4の処理を行います。
%% @end
%%--------------------------------------------------------------------
-spec(add_message(UserId::integer(), Text::list(), 
                  InReplyTo::integer()) -> 
    {ok, MessageId::integer(), MessgeKey::binary()} | 
    {error, Reason::binary()}).

add_message(UserId, Text, InReplyTo) when is_integer(UserId) and 
                                          is_list(Text) and
                                          is_integer(InReplyTo) ->
    Worker = poolboy:checkout(mentions_send_server_pool),
    Reply = message_send_server:add_message(Worker, UserId, Text, InReplyTo),
    poolboy:checkin(mentions_send_server_pool, Worker),
    Reply.

%%--------------------------------------------------------------------
%% @doc 受け取ったメッセージを保存します。
%% @end
%%--------------------------------------------------------------------
-spec(add_message(Name_OR_Pid::pid()|atom(), UserId::integer(), 
                  Text::list(), InReplyTo::integer()) -> 
    {ok, MessageId::integer(), MessgeKey::binary()} | 
    {error, Reason::binary()}).

add_message(Name_OR_Pid, UserId, Text, InReplyTo) when is_integer(UserId) and
                                                       is_list(Text) and
                                                       is_integer(InReplyTo) ->
    gen_server:call(Name_OR_Pid, {add_message, UserId, Text, InReplyTo}).

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
handle_call({add_message, UserId, Text, InReplyTo}, _From, State) ->
    Reply = message:save_message(UserId, Text, InReplyTo),
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
handle_cast(_Msg, State) ->
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
