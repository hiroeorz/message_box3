%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2011 by Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(message_box3_sup).
-include_lib("eunit/include/eunit.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    HomeSendServerSup = {home_send_server_sup, 
                         {home_send_server_sup, start_link, []},
                         Restart, Shutdown, Type, 
                         [home_send_server_sup, home_send_server]},

    MentionSendServerSup = {mention_send_server_sup, 
                            {mention_send_server_sup, start_link, []},
                            Restart, Shutdown, Type, 
                            [mention_send_server_sup, mention_send_server]},

    MessageSendServerSup = {message_send_server_sup, 
                            {message_send_server_sup, start_link, []},
                            Restart, Shutdown, Type, 
                            [message_send_server_sup, message_send_server]},

    LoginServerSup = {msb3_login_server_sup, 
                      {msb3_login_server_sup, start_link, []},
                      Restart, Shutdown, Type, 
                      [msb3_login_server_sup, msb3_login_server]},

    MainController = {message_box3, 
                      {message_box3, start_link, []},
                      Restart, Shutdown, Type, 
                      [message_box3]},
    
    {ok, {SupFlags, [HomeSendServerSup, MentionSendServerSup,
                     MessageSendServerSup, LoginServerSup, MainController]}}.

