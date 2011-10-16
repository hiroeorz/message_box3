%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(mention_send_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(NAME, mention_send_server).

add_mention_to_followers_test() ->
    test_before(),
    MsgKey = <<"msg_101">>,
    TextBin = <<"@taro dororicchi now! :-) @nobody @neko">>,
    
    {ok, _} = msb3_user:add_user("shin", "shin@mail", "pass"),
    {ok, _} = msb3_user:add_user("taro", "shin@mail", "pass"),
    {ok, _} = msb3_user:add_user("neko", "shin@mail", "pass"),
    {ok, _} = msb3_user:add_user("goro", "shin@mail", "pass"),

    ?assertEqual(ok, mention_send_server:add_mention(?NAME, MsgKey, TextBin)),
    msb3_util:sleep(100),

    ?assertEqual({ok, []}, 
                 eredis_pool:q(default, ["LRANGE", "m_1", 0, -1])),

    ?assertEqual({ok, [MsgKey]}, 
                 eredis_pool:q(default, ["LRANGE", "m_2", 0, -1])),
    
    ?assertEqual({ok, [MsgKey]}, 
                 eredis_pool:q(default, ["LRANGE", "m_3", 0, -1])),

    ?assertEqual({ok, []}, 
                 eredis_pool:q(default, ["LRANGE", "m_4", 0, -1])),

    test_after().
    

%%%===================================================================
%%% Internal functions
%%%===================================================================

test_before() ->
    eredis_pool:start(),
    eredis_pool:create_pool(default, 10),
    eredis_pool:q(default, ["DEL", <<"max_usr_id">>]),
    
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
    lists:map(fun(Id) -> 
                      Key = list_to_binary("m_" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 100)),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("usr_" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 100)),
    mention_send_server:start_link(?NAME).

test_after() ->
    eredis_pool:q(default, ["DEL", <<"max_usr_id">>]),
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
    lists:map(fun(Id) -> 
                      Key = list_to_binary("m_" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 100)),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("usr_" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 100)),

    mention_send_server:stop(?NAME).
