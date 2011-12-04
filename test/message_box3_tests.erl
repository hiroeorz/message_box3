-module(message_box3_tests).
-include_lib("eunit/include/eunit.hrl").

%% Include
-include("message.hrl").
-include("user.hrl").

-export([clean_up/0]).

-define(Setup, fun() ->
                       message_box3_tests:clean_up(),
                       application:start(message_box3)
               end).

-define(Clearnup, fun(_) ->
                       message_box3_tests:clean_up(),
                       application:stop(message_box3)
		  end).


basic_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [
       { "新規ユーザを登録する",
         fun() ->
                 ?assertMatch({ok, _UserId},
                              message_box3:create_user("taro", "taro@mail", 
                                                       "password1")),

                 ?assertMatch({ok, _UserId},
                              message_box3:create_user("jiro", "jiro@mail", 
                                                       "password2")),

                 ?assertMatch({ok, _UserId},
                              message_box3:create_user("saburo", "saburo@mail", 
                                                       "password3"))

         end
       },

       { "ユーザ情報を取得する",
         fun() ->
                 ?assertMatch({ok, _User}, message_box3:get_user(1)),
                 ?assertMatch({error, not_found}, message_box3:get_user(-1))
         end
       },

       { "認証を実行してセッションキーを取得する",
         fun() ->
                 ?assertMatch({ok, _, _}, 
                              message_box3:authenticate("taro", "password1")),

                 ?assertMatch({error, password_incollect}, 
                              message_box3:authenticate("taro", "")),

                 ?assertMatch({error, password_incollect}, 
                              message_box3:authenticate("taro", "incollect"))
         end
       },

       { "1がメッセージを送信する",
         fun() ->
                 {ok, SessionKey, _Id} = 
                     message_box3:authenticate("taro", "password1"),

                 Result = message_box3:send_message(1, SessionKey, 
                                                    "hello :-)", undefined),
                 ?assertMatch({ok, _}, Result)
         end
       },

       { "メッセージを送信の為のログインに失敗する",
         fun() ->
                 Result = message_box3:send_message(1, "invalid", 
                                                    "hello :-)", undefined),
                 ?assertMatch({error, session_expired}, Result)
         end
       },

       { "1が2をフォローする",
         fun() ->
                 {ok, SessionKey, _Id} = 
                     message_box3:authenticate("taro", "password1"),

                 Result = message_box3:follow(1, SessionKey, 2),
                 ?assertEqual({ok, {follow, 1}, {follower, 0}}, Result)
         end
       },

       { "1が2をフォロー為のログインに失敗する",
         fun() ->
                 Result = message_box3:follow(1, "invalid", 2),
                 ?assertMatch({error, session_expired}, Result) 
         end
       },

       { "1が3のフォローを外す",
         fun() ->
                 {ok, SessionKey, _Id} = 
                     message_box3:authenticate("taro", "password1"),

                 {ok, _, _} = message_box3:follow(1, SessionKey, 3),
                 Result = message_box3:unfollow(1, SessionKey, 3),
                 ?assertEqual({ok, {follow, 1}, {follower, 0}}, Result)
         end

       }

      ]
     }
    }.


%%%===================================================================
%%% Internal functions
%%%===================================================================

clean_up() ->
    eredis_pool:start(),
    eredis_pool:create_pool(dbsrv, 10),
    eredis_pool:q({global, dbsrv}, 
                  ["DEL", <<"max_usr_id">>]),

    lists:map(fun(Id) -> 
                      Key = list_to_binary("usr_" ++ integer_to_list(Id)),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, lists:seq(1, 10001)),

    lists:map(fun(Name) -> 
                      Key = list_to_binary("usr_name_index_" ++ Name),
                      eredis_pool:q({global, dbsrv}, ["DEL", Key])
              end, ["shin"]).

