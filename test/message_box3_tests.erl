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
                 ?assertMatch({ok, _}, 
                              message_box3:authenticate("taro", "password1")),

                 ?assertMatch({error, password_incollect}, 
                              message_box3:authenticate("taro", "")),

                 ?assertMatch({error, password_incollect}, 
                              message_box3:authenticate("taro", "incollect"))
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

