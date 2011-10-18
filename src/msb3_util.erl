%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(msb3_util).

%% API
-export([sleep/1, create_session_key/2, create_crypted_password/2]).

%% Include
-include("user.hrl").

-define(KEY_PHRASE_1, "message_box3").
-define(KEY_PHRASE_2, "SHIMANE").
-define(KEY_PHRASE_3, "MATSUE").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 一定時間処理を停止します。
%% @end
%%--------------------------------------------------------------------
-spec(sleep(MSec::integer()) -> ok ).

sleep(MSec) when is_integer(MSec) ->
    receive
    after MSec -> ok
    end.

%%--------------------------------------------------------------------
%% @doc 与えられたIdとパスワードからセッションキーを生成します。
%% @end
%%--------------------------------------------------------------------
-spec(create_session_key(Id::integer(), Password::string()) -> 
             SessionKey::string()).

create_session_key(Id, Password) when is_integer(Id) and is_list(Password) ->
    {Megaseconds, Seconds, Microseconds} = erlang:now(),
    TimeStr = integer_to_list(Megaseconds) ++
        integer_to_list(Seconds) ++
        integer_to_list(Microseconds),

    SessionKey = crypto:sha([TimeStr, 
                             atom_to_list(node()), 
                             integer_to_list(Id),
                             Password]),

    lists:flatten(lists:map(fun(X) -> 
                                    io_lib:format("~.16X", [X, ""]) 
                            end, 
                            binary_to_list(SessionKey))).
    

-spec(create_crypted_password(User::#user{}, Password::string()) ->
             CryptedPassword::string()).

create_crypted_password(User, Password) when is_list(Password)->

    {{Year, Month, Day}, {Hour, Min, Sec}} = User#user.created_at,

    TimeStr = integer_to_list(Year) ++ integer_to_list(Month) ++
        integer_to_list(Day) ++ integer_to_list(Hour) ++
        integer_to_list(Min) ++ integer_to_list(Sec),

    CryptedPassword = crypto:sha([TimeStr, 
                                  User#user.name,
                                  User#user.email,
                                  ?KEY_PHRASE_1,
                                  ?KEY_PHRASE_2,
                                  ?KEY_PHRASE_3,
                                  Password]),

    lists:flatten(lists:map(fun(X) -> 
                                    io_lib:format("~.16X", [X, ""]) 
                            end, 
                            binary_to_list(CryptedPassword))).

%%%===================================================================
%%% Internal functions
%%%===================================================================
