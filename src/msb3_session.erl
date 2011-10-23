%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2011 by Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(msb3_session).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("message_box3.hrl").

%% API
-export([add_new_session/2, update_expire/2, check_session_expire/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 新たなセッションをユーザのセッションリストに追加する
%% @end
%%--------------------------------------------------------------------
-spec(add_new_session(UserId::integer(), SessionKey::string()) -> ok ).

add_new_session(UserId, SessionKey) ->
    Key = get_list_key(UserId),
    eredis_pool:q(?DB_SRV, ["SADD", Key, SessionKey]),
    update_expire(UserId, SessionKey).

%%--------------------------------------------------------------------
%% @doc セッションの有効期限を延長する
%% @end
%%--------------------------------------------------------------------
-spec(update_expire(UserId::integer(), SessionKey::string()) -> ok).

update_expire(UserId, SessionKey) ->
    Key = get_key(SessionKey),
    {ok, Expire} = application:get_env(message_box3, session_expire),
    {ok, <<"OK">>} = eredis_pool:q(?DB_SRV, ["SETEX", Key, Expire, UserId]),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% セッションが有効かどうか確認する
%% 無効であればユーザのセッションリストから削除する
%% @end
%%--------------------------------------------------------------------
-spec(check_session_expire(UserId::integer(), SessionKey::string()) -> 
             ok | expired).

check_session_expire(UserId, SessionKey) ->
    SessListKey = get_list_key(UserId),
    Key = get_key(SessionKey),

    case eredis_pool:q(?DB_SRV, ["SISMEMBER", SessListKey, SessionKey]) of
        {ok, <<"0">>} -> expired;
        {ok, <<"1">>} ->
            case eredis_pool:q(?DB_SRV, ["TTL", Key]) of
                {ok, <<"-1">>} -> 
                    eredis_pool:q(?DB_SRV, 
                                  ["SREM", SessListKey, SessionKey]),
                    expired;
                {ok, _} -> 
                    ok
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(get_key(SessionKey::string()) -> string() ).

get_key(SessionKey) -> "session_" ++ SessionKey.
get_list_key(UserId) -> "session_list_" ++ integer_to_list(UserId).   
     
