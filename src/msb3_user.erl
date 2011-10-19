%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(msb3_user).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("user.hrl").
-include("message_box3.hrl").

%% API
-export([add_user/3, get_user/1, get_id/1, get_id_list/1]).

-define(MaxIdKey, <<"max_usr_id">>).
-define(USER_NAME_INDEX_KEY, "usr_name_index_").

%%%===================================================================
%%% API
%%%===================================================================
-spec(add_user(Name::string(), Mail::string(), Password::string()) -> 
             {ok, UserId::integer()} | {error, Reason::binary()}).

add_user(Name, Mail, Password) when is_list(Name) and is_list(Mail) and 
                                    is_list(Password) ->
    UserId = get_next_id(),
    Key = "usr_" ++ integer_to_list(UserId),
    User1 = #user{id = UserId, name = Name, password = Password,
                 email = Mail, created_at = {date(), time()}},

    CryptedPassword = msb3_util:create_crypted_password(User1, Password),
    User2 = User1#user{password = CryptedPassword},

    case eredis_pool:q(?DB_SRV, ["SET", Key, term_to_binary(User2)]) of
        {ok, _} ->
            case add_user_name_index(UserId, Name) of
                ok -> {ok, UserId};
                _ -> {error, index_save_error} %% todo 後始末
            end;
        Other -> Other
    end.

-spec(get_user(Id_OR_Name::integer() | string()) -> 
             {ok, User::#user{}} | {error, not_found}).

get_user(Name) when is_list(Name) ->
    case get_id(Name) of
        {error, not_found} -> {error, not_found};
        {ok, UserId} -> get_user(UserId)
    end;

get_user(Id) when is_integer(Id) ->
    Key = "usr_" ++ integer_to_list(Id),
    case eredis_pool:q(?DB_SRV, ["GET", Key]) of
        {ok, undefined} -> {error, not_found};
        {ok, User} -> {ok, User}
    end.

-spec(get_id(Name::string()) -> {ok, integer()} | {error, not_found}).

get_id(Name) ->
    Key = ?USER_NAME_INDEX_KEY ++ Name,

    case eredis_pool:q(?DB_SRV, ["GET", Key]) of
        {ok, undefined} -> {error, not_found};
        {ok, IdBin} -> {ok, list_to_integer(binary_to_list(IdBin))}
    end.

-spec(get_id_list(NameList::[string()]) -> [integer()] ).

get_id_list(NameList) when is_list(NameList) ->
    KeyList = lists:map(fun(Name) -> ?USER_NAME_INDEX_KEY ++ Name end, 
                        NameList),

    case eredis_pool:q(?DB_SRV, ["MGET" | KeyList]) of
        {ok, undefined} -> [];
        {ok, UserIdList} ->
            List = lists:foldl(fun(IdBin, Result) ->
                                       case IdBin of 
                                           undefined -> Result;
                                           Bin ->
                                               Id = list_to_integer(
                                                      binary_to_list(Bin)),
                                               [Id | Result]
                                       end
                               end, [], UserIdList),
            lists:reverse(List)
    end.    

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_next_id() ->
    {ok, NextIdBin} = eredis_pool:q(?DB_SRV, ["INCR", ?MaxIdKey]),
    list_to_integer(binary_to_list(NextIdBin)).

add_user_name_index(UserId, Name) ->
    Key = ?USER_NAME_INDEX_KEY ++ Name,

    case eredis_pool:q(?DB_SRV, ["SET", Key, UserId]) of
        {ok, _} -> ok;
        Error -> Error
    end.
             
