%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(home_send_server_tests).
-include_lib("eunit/include/eunit.hrl").

add_home_to_followers_test() ->
    test_before(),
    UserId = 1,
    Ids = lists:seq(2, 10001),
    LimitInterval = 500,
    MsgKey = <<"msg_101">>,
    lists:map(fun(Id) -> user_relation:add_follower(UserId, Id) end, Ids),
    home_send_server:add_home_to_followers(1, MsgKey),
    msb3_util:sleep(LimitInterval),

    lists:map(fun(Id) ->
                      HomeKey = "h_" ++ integer_to_list(Id),
                      {ok, [Key]} = eredis_pool:q(default, ["LRANGE", 
                                                            HomeKey, 0, -1]),
                      ?assertEqual(Key, MsgKey)
              end, Ids),
    test_after().
    

%%%===================================================================
%%% Internal functions
%%%===================================================================

test_before() ->
    eredis_pool:start(),
    eredis_pool:create_pool(default, 10),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("h_" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 10001)),

    lists:map(fun(Id) -> 
                      Key = list_to_binary("follower_" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 100)),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("follow_" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 100)),
    home_send_server:start_link().

test_after() ->
    lists:map(fun(Id) -> 
                      Key = list_to_binary("h_" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 10001)),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("follower_" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 100)),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("follow_" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 100)),
    home_send_server:stop().
