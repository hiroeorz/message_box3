%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2011 by Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(msb3_session).

%% API
-export([add_new_session/2, update_session/2, check_session_expire/2]).

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
    eredis_pool:q(default, ["SADD", Key, SessionKey]),
    update_session(UserId, SessionKey).

%%--------------------------------------------------------------------
%% @doc セッションの有効期限を延長する
%% @end
%%--------------------------------------------------------------------
-spec(update_session(UserId::integer(), SessionKey::string()) -> ok).

update_session(UserId, SessionKey) ->
    Key = get_key(SessionKey),
    {ok, Expire} = application:get_env(message_box3, session_expire),
    eredis_pool:q(default, ["SETEX", Key, Expire, UserId]).

%%--------------------------------------------------------------------
%% @doc セッションが有効かどうか確認する
%% @end
%%--------------------------------------------------------------------
-spec(check_session_expire(UserId::integer(), SessionKey::string()) -> 
             ok | expired).

check_session_expire(UserId, SessionKey) ->
    SessListKey = get_list_key(UserId),
    Key = get_key(SessionKey),

    case eredis_pool:q(default, ["SISMEMBER", SessListKey, SessionKey]) of
        {ok, <<"0">>} -> expired;
        {ok, <<"1">>} ->
            Ttl = eredis_pool:q(default, ["TTL", Key]),

            if Ttl > 0 -> ok;
               true ->
                    eredis_pool:q(default, ["SREM", SessListKey, SessionKey]),
                    expired
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(get_key(SessionKey::string()) -> string() ).

get_key(SessionKey) -> "session_" ++ SessionKey.
get_list_key(UserId) -> "session_list_" ++ integer_to_list(UserId).   
     
