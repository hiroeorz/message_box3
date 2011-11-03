%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(mention_send_server).

-behaviour(gen_server).

%% Include
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1, stop/0, stop/1]).
-export([add_mention/2, add_mention/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SEPARATOR, "\s\n").
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
%% 受け取ったテキストからリプライ送信先ユーザを特定して各ユーザのmentionsリストに
%% メッセージのキーを保存します。
%% 処理は非同期に行います。
%% @end
%%--------------------------------------------------------------------
-spec(add_mention(MsgKey::binary(), TextBin::binary()) -> ok).

add_mention(MsgKey, TextBin) when is_binary(MsgKey) and is_binary(TextBin) ->
    Worker = poolboy:checkout(mention_send_server_pool),
    Reply = mention_send_server:add_mention(Worker, MsgKey, TextBin),
    poolboy:checkin(mention_send_server_pool, Worker),
    Reply.

%%--------------------------------------------------------------------
%% @doc 
%% 受け取ったテキストからリプライ送信先ユーザを特定して各ユーザのmentionsリストに
%% メッセージのキーを保存します。
%% 処理は非同期に行います。
%% @end
%%--------------------------------------------------------------------
-spec(add_mention(Name_OR_Pid::pid()|atom(), 
                               MsgKey::binary(), 
                               TextBin::binary()) -> ok).

add_mention(Name_OR_Pid, MsgKey, TextBin) when is_binary(MsgKey) and
                                               is_binary(TextBin) ->
    gen_server:cast(Name_OR_Pid, {add_mention, MsgKey, TextBin}).


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

handle_cast({add_mention, MsgKey, TextBin}, State) ->
    NameList = get_reply_list(TextBin),
    IdList = msb3_user:get_id_list(NameList),

    lists:map(fun(Id) ->
                      mentions_timeline:add_message_key(Id, MsgKey)
              end, IdList),
    
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

%%
%% @doc create reply name list from tweet text.
%%
-spec(get_reply_list(string()) -> list(atom()) ).

get_reply_list(Text) when is_binary(Text) ->
    get_reply_list(binary_to_list(Text));

get_reply_list(Text) when is_list(Text) ->
    Tokens = string:tokens(Text, ?SEPARATOR),
    get_reply_list(Tokens, []).

get_reply_list([], List) -> lists:usort(List);

get_reply_list(Tokens, List) when is_list(Tokens) ->
    [Token | Tail] = Tokens,
    case string:sub_string(Token, 1, 1) of
	"@" ->
	    UserNameStr = string:sub_string(Token, 2, length(Token)),
	    get_reply_list(Tail, [UserNameStr | List]);
	_Other ->
	    get_reply_list(Tail, List)
    end.
