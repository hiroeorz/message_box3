-module(message_box3_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    eredis_pool:start(),
    create_eredis_pools(),
    message_box3_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

create_eredis_pools() ->
    {ok, Pools} = application:get_env(message_box3, eredis_pools),

    Restart = permanent,
    Shutdown = 5000,
    Type = worker,

    PoolSpecs = lists:map(fun({PoolName, PoolConfig}) ->
                                  Args = [{name, {local, PoolName}},
                                          {worker_module, eredis}]
                                      ++ PoolConfig,
                                  
                                  {PoolName, {poolboy, start_link, [Args]},
                                   Restart, Shutdown, Type, [poolboy, eredis]}
                          end, Pools),

    ok = eredis_pool:create_pools(PoolSpecs).
