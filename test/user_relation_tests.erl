%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(user_relation_tests).
-include_lib("eunit/include/eunit.hrl").

add_follower_test() ->
    test_before(),
    ?assertEqual({ok, 1}, user_relation:add_follower(1, 101)),
    ?assertEqual({ok, 2}, user_relation:add_follower(1, 112)),
    ?assertEqual({ok, 3}, user_relation:add_follower(1, 123)),
    test_after().

delete_follower_test() ->
    test_before(),
    ?assertEqual({ok, 1}, user_relation:add_follower(1, 101)),
    ?assertEqual({ok, 0}, user_relation:delete_follower(1, 101)),
    test_after().
    
get_followers_test() ->
    UserId = 1,
    Ids = [101, 102, 1100, 123, 132, 3, 222, 345],
    lists:map(fun(Id) -> user_relation:add_follower(UserId, Id) end, Ids),
    ?assertEqual(Ids, user_relation:get_followers(UserId)).

add_follow_test() ->
    test_before(),
    ?assertEqual({ok, 1}, user_relation:add_follow(1, 101)),
    ?assertEqual({ok, 2}, user_relation:add_follow(1, 112)),
    ?assertEqual({ok, 3}, user_relation:add_follow(1, 123)),
    test_after().

delete_follow_test() ->
    test_before(),
    ?assertEqual({ok, 1}, user_relation:add_follow(1, 101)),
    ?assertEqual({ok, 0}, user_relation:delete_follow(1, 101)),
    test_after().
    
get_follows_test() ->
    UserId = 1,
    Ids = [101, 102, 1100, 123, 132, 3, 222, 345],
    lists:map(fun(Id) -> user_relation:add_follow(UserId, Id) end, Ids),
    ?assertEqual(Ids, user_relation:get_follows(UserId)).
    

%%%===================================================================
%%% Internal functions
%%%===================================================================

test_before() ->
    eredis_pool:start(),
    eredis_pool:create_pool(dbsrv, 10),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("follower_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 100)),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("follow_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 100)).


test_after() ->
    lists:map(fun(Id) -> 
                      Key = list_to_binary("follower_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 100)),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("follow_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 100)).



