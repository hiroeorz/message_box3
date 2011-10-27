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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([create_user/3, get_user/1, authenticate/2]).

-define(SERVER, ?MODULE). 

-record(state, {connection_count ::integer()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() -> {ok, Pid::pid()} | ignore | {error, Error::atom()}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    Fun = fun() -> msb3_user:add_user(Name, Mail, Password) end,
    gen_server:call(?SERVER, {parallel, Fun}).

%%--------------------------------------------------------------------
%% @doc
%% ユーザ情報を取得する
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_user(UserId::integer()) -> {ok, User::#user{}} | {error, not_found}).

get_user(UserId) when is_integer(UserId) ->
    Fun = fun() ->
                  case msb3_user:get_user(UserId) of
                      {ok, User} -> 
                          {ok, User#user{password = undefined}};
                      {error, not_found} ->
                          {error, not_found}
                  end
          end,

    gen_server:call(?SERVER, {parallel, Fun}).

%%--------------------------------------------------------------------
%% @doc
%% 認証を実行し、以後要求を実行する為のセッションキーを得る。
%%
%% @end
%%--------------------------------------------------------------------
-spec(authenticate(Name::string(), Password::string()) -> 
             {ok, SessionKey::string()} | {error, password_incollect}).

authenticate(Name, Password) when is_list(Name) and is_list(Password) ->
    Fun = fun() -> msb3_login_server:authenticate(Name, Password) end,
    gen_server:call(?SERVER, {parallel, Fun}).


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
