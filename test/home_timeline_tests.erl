%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(home_timeline_tests).
-include_lib("eunit/include/eunit.hrl").
-include("message.hrl").

add_message_key_test() ->
    test_before(),
    UserId = 1,
    ?assertEqual(ok, home_timeline:add_message_key(UserId, <<"msg_1">>)),
    ?assertEqual(ok, home_timeline:add_message_key(UserId, <<"msg_2">>)),
    ?assertEqual(ok, home_timeline:add_message_key(UserId, <<"msg_3">>)),
    ?assertEqual(ok, home_timeline:add_message_key(UserId, <<"msg_4">>)),
    ?assertEqual(ok, home_timeline:add_message_key(UserId, <<"msg_5">>)),
    test_after().

get_timeline_test() ->
    test_before(),
    UserId = 1,
    {ok, Id1, Key_1} = message:save_message(101, "text 1"),
    home_timeline:add_message_key(UserId, Key_1),    
    {ok, Id2, Key_2} = message:save_message(102, "text 2"),
    home_timeline:add_message_key(UserId, Key_2),
    {ok, Id3, Key_3} = message:save_message(103, "text 3"),
    home_timeline:add_message_key(UserId, Key_3),

    ?assertMatch({ok, _}, home_timeline:get_timeline(UserId, 3)),

    {ok, HomeTimeLine} = home_timeline:get_timeline(UserId, 3),

    [Msg1 | Tail1] = HomeTimeLine, 
    ?assertEqual(Id1, Msg1#message.id),
    ?assertEqual("text 1", Msg1#message.text),
    ?assertEqual(101, Msg1#message.user_id),

    [Msg2 | Tail2] = Tail1, 
    ?assertEqual(Id2, Msg2#message.id),
    ?assertEqual("text 2", Msg2#message.text),
    ?assertEqual(102, Msg2#message.user_id),

    [Msg3 | _Tail3] = Tail2, 
    ?assertEqual(Id3, Msg3#message.id),
    ?assertEqual("text 3", Msg3#message.text),
    ?assertEqual(103, Msg3#message.user_id),
    test_after().

add_home_to_followers_test() ->
    test_before(),
    UserId = 1,
    Ids = lists:seq(2, 1001),
    LimitInterval = 100,
    MsgKey = <<"msg_101">>,
    lists:map(fun(Id) -> user_relation:add_follower(UserId, Id) end, Ids),
    home_timeline:add_home_to_followers(1, MsgKey),
    msb3_util:sleep(LimitInterval),

    lists:map(fun(Id) ->
                      HomeKey = "h_" ++ integer_to_list(Id),
                      {ok, [Key]} = eredis_pool:q({global, dbsrv}, 
                                                  ["LRANGE", HomeKey, 0, -1]),
                      ?assertEqual(Key, MsgKey)
              end, Ids),
    test_after().

%%%===================================================================
%%% Internal functions
%%%===================================================================

test_before() ->
    eredis_pool:start(),
    eredis_pool:create_pool(dbsrv, 10),
    eredis_pool:q({global, dbsrv}, ["DEL", <<"h_1">>]),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("msg" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 100)),
    
    lists:map(fun(Id) -> 
                      Key = list_to_binary("h_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 10001)),

    lists:map(fun(Id) -> 
                      Key = list_to_binary("follower_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 100)),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("follow_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 100)).


test_after() ->
    eredis_pool:q({global, dbsrv}, ["DEL", <<"h_1">>]),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("msg" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 100)),
    
    lists:map(fun(Id) -> 
                      Key = list_to_binary("h_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 10001)),

    lists:map(fun(Id) -> 
                      Key = list_to_binary("follower_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 100)),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("follow_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 100)).


