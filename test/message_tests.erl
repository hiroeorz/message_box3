%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(message_tests).
-include_lib("eunit/include/eunit.hrl").
-include("message.hrl").

save_message_test() ->
    test_before(),
    UserId = 1,
    Text = "hello this is test post.",
    ?assertMatch({ok, _, _}, message:save_message(UserId, Text)),
    ?assertMatch({ok, _, _}, message:save_message(UserId, Text, 1)),
    test_after().

get_message_test() ->
    test_before(),
    UserId = 1,
    Text = "hello this is test post.",
    {ok, MsgId, _Key} = message:save_message(UserId, Text),
    Message = message:get_message(MsgId),
    ?assertEqual(UserId, Message#message.user_id),
    ?assertEqual(MsgId, Message#message.id),
    ?assertEqual(Text, Message#message.text),
    ?assertEqual(undefined, Message#message.in_reply_to),
    ?assertMatch({{_, _, _}, {_, _, _}}, Message#message.created_at),    
    test_after().

get_message_list_test() ->
    test_before(),
    UserId = 1,
    Text = "hello this is test post.",
    {ok, MsgId_1, Key_1} = message:save_message(UserId, Text),
    {ok, MsgId_2, Key_2} = message:save_message(UserId, Text),
    {ok, MsgId_3, Key_3} = message:save_message(UserId, Text),
    {ok, MsgId_4, Key_4} = message:save_message(UserId, Text),
    {ok, MsgId_5, Key_5} = message:save_message(UserId, Text),

    Keys = [Key_1, Key_2, Key_3, Key_4, Key_5],

    {ok, MessageList} = message:get_message_list(Keys),
    [Msg1 | Tail1] = MessageList, ?assertEqual(MsgId_1, Msg1#message.id),
    [Msg2 | Tail2] = Tail1, ?assertEqual(MsgId_2, Msg2#message.id),
    [Msg3 | Tail3] = Tail2, ?assertEqual(MsgId_3, Msg3#message.id),
    [Msg4 | Tail4] = Tail3, ?assertEqual(MsgId_4, Msg4#message.id),
    [Msg5 |     _] = Tail4, ?assertEqual(MsgId_5, Msg5#message.id),
    test_after().    

test_before() ->
    eredis_pool:start(),
    eredis_pool:create_pool(default, 10),
    eredis_pool:q(default, ["DEL", <<"max_message_id">>]),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("msg" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 100)).

test_after() ->
    eredis_pool:q(default, ["DEL", <<"max_message_id">>]),
    lists:map(fun(Id) -> 
                      Key = list_to_binary("msg" ++ integer_to_list(Id)),
                      eredis_pool:q(default, ["DEL", Key])
              end, lists:seq(1, 100)).

