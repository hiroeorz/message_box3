%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(message_send_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(NAME, message_send_server).

add_message_test() ->
    test_before(),
    Result = message_send_server:add_message(?NAME, 1, "this is test.", 2),
    ?assertMatch({ok, _, _}, Result),

    {ok, Id, Key} = Result,
    ?assertEqual(true, is_integer(Id)),
    ?assertEqual(true, is_binary(Key)),
    test_after().    

%%%===================================================================
%%% Internal functions
%%%===================================================================

test_before() ->
    eredis_pool:start(),
    eredis_pool:create_pool(dbsrv, 10),
    eredis_pool:q({global, dbsrv}, ["DEL", <<"max_message_id">>]),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("msg_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 1000)),

    message_send_server:start_link(?NAME).

test_after() ->
    eredis_pool:q({global, dbsrv}, ["DEL", <<"max_message_id">>]),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("msg_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 1000)),

    message_send_server:stop(?NAME).
