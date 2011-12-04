%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(msb3_login_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(NAME, msb3_login_server).

authenticate_test() ->
    test_before(),

    {ok, _} = msb3_user:add_user("usr1", "usr1@mail", "pass1"),

    ?assertMatch({ok, _, _}, 
                 msb3_login_server:authenticate(?NAME, "usr1", "pass1")),

    ?assertEqual({error, password_incollect}, 
                 msb3_login_server:authenticate(?NAME, "usr1", "incollect")),

    ?assertEqual({error, password_incollect}, 
                 msb3_login_server:authenticate(?NAME, "usr1", "")),

    ?assertEqual({error, password_incollect}, 
                 msb3_login_server:authenticate(?NAME, "", "incollect")),

    ?assertEqual({error, password_incollect}, 
                 msb3_login_server:authenticate(?NAME, "", "")),

    test_after().    

login_test() ->
    test_before(),
    {ok, _} = msb3_user:add_user("usr1", "usr1@mail", "pass1"),
    {ok, SessionKey, _Id} = msb3_login_server:authenticate(?NAME, 
                                                              "usr1", "pass1"),
    ?assertEqual(ok, msb3_login_server:login(?NAME, 1, SessionKey)),
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
              end, lists:seq(1, 10)),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("usr_name_index_usr" ++ 
                                               integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 10)),
    application:start(message_box3),
    msb3_login_server:start_link(?NAME).

test_after() ->
    lists:map(fun(Id) -> 
                      Key = list_to_binary("usr_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 10)),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("usr_name_index_usr" ++ 
                                               integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 10)),

    application:stop(message_box3),
    msb3_login_server:stop(?NAME).
