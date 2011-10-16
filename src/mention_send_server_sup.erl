%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2011 by Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(mention_send_server_sup).

-behaviour(supervisor).

%% Include
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 5000,
    Type = worker,

    {ok, Pools} = application:get_env(message_box3, mention_send_server_pools),

    PoolSpecs = lists:map(fun({PoolName, PoolConfig}) ->
                                  Args = [{name, {local, PoolName}},
                                          {worker_module, mention_send_server}]
                                      ++ PoolConfig,
                                  
                                  {PoolName, {poolboy, start_link, [Args]},
                                   Restart, Shutdown, Type, 
                                   [poolboy, mention_send_server]}
                          end, Pools),

    {ok, {SupFlags, PoolSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
