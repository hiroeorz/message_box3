%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by Hiroe Shin <shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(msb3_login_server).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("user.hrl").

%% API
-export([authenticate/2, login/2]).

%%--------------------------------------------------------------------
%% @doc 認証を行い、認証にパスしたらセッションキーを新たに生成して返す。
%% @end
%%--------------------------------------------------------------------
-spec authenticate(Name, Password) -> {ok, SessionKey} | {error, password_incollect} when
      Name :: string(),
      Password :: string(),
      SessionKey :: string().

authenticate(Name, Password) ->
    case msb3_user:get_user(Name) of
        {ok, User} ->
            CryptedPass = 
                msb3_util:create_crypted_password(User, Password),
            
            if User#user.password == CryptedPass ->
                    Id = User#user.id,
                    SessionKey = msb3_util:create_session_key(Id,
                                                              CryptedPass),
                    ok = msb3_session:add_new_session(Id, SessionKey),
                    ok = msb3_session:update_expire(Id, SessionKey),
                    {ok, SessionKey, Id};
               true ->
                    {error, password_incollect}
            end;
        _ ->
            {error, password_incollect}
    end.

%%--------------------------------------------------------------------
%% @doc 
%% ユーザIdとセッションの組み合わせが既に認証済みか確認する
%% 確認がとれた場合はセッションの有効期限を延ばす
%% @end
%%--------------------------------------------------------------------
-spec login(UserId, SessionKey) -> ok | expired when
      UserId::integer(),
      SessionKey::string().

login(UserId, SessionKey)  ->
    case msb3_session:check_session_expire(UserId, SessionKey) of
        ok -> 
            msb3_session:update_expire(UserId, SessionKey),
            ok;
        expired ->
            expired
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
