%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(msb3_user_tests).
-include_lib("eunit/include/eunit.hrl").

add_user_test() ->
    test_before(),
    ?assertMatch({ok, _}, msb3_user:add_user("shin", "shin@mail", "pass")),
    test_after().

get_id_test() ->
    test_before(),
    {ok, UserId} = msb3_user:add_user("shin", "shin@mail", "pass"),
    ?assertEqual({ok, UserId}, msb3_user:get_id("shin")),
    ?assertMatch({error, not_found}, msb3_user:get_id("nobody")),
    test_after().

get_id_list_test() ->    
    test_before(),
    NameList = ["shin", "goro", "taro", "tama", "dora"],

    lists:map(fun(Name) -> 
                      Mail = Name ++ "@mail",
                      {ok, _} = msb3_user:add_user(Name, Mail, "pass")
              end, NameList),

    ?assertMatch([2, 3], msb3_user:get_id_list(["goro", "taro"])),
    ?assertMatch([5, 1], msb3_user:get_id_list(["dora", "shin"])),
    ?assertMatch([1, 3, 5], msb3_user:get_id_list(["shin", "taro", "dora"])),
    test_after().
    

%%%===================================================================
%%% Internal functions
%%%===================================================================
test_before() ->
    eredis_pool:start(),
    eredis_pool:create_pool(dbsrv, 10),
    eredis_pool:q({global, dbsrv}, ["DEL", <<"max_usr_id">>]),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("usr_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 10001)),
    lists:map(fun(Name) -> 
                      Key = list_to_binary("usr_name_index_" ++ Name),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, ["shin"]).

test_after() ->
    eredis_pool:q({global, dbsrv}, ["DEL", <<"max_usr_id">>]),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("usr_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 10001)),
    lists:map(fun(Name) -> 
                      Key = list_to_binary("usr_name_index_" ++ Name),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, ["shin"]).

