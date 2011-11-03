%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(user_relation).

%% Include
-include("message_box3.hrl").

%% API
-export([add_follower/2, get_followers/1, delete_follower/2, follower_count/1,
         add_follow/2, get_follows/1, delete_follow/2, follow_count/1]).

%%%===================================================================
%%% Follower API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc フォロワーを追加する
%% @end
%%--------------------------------------------------------------------
-spec(add_follower(UserId::integer(), FollowUserId::integer()) -> 
             {ok, Count::integer()} ).

add_follower(UserId, FollowUserId) ->
    Key = get_follower_list_Key(UserId),
    {ok, CountBin} = eredis_pool:q(?DB_SRV, ["RPUSH", Key, FollowUserId]),
    Count = list_to_integer(binary_to_list(CountBin)),
    {ok, Count}.

%%--------------------------------------------------------------------
%% @doc フォロワーを削除する
%% @end
%%--------------------------------------------------------------------
-spec(delete_follower(UserId::integer(), FollowUserId::integer()) -> 
             {ok, Count::integer()} ).

delete_follower(UserId, FollowUserId) ->
    Key = get_follower_list_Key(UserId),
    IdBin = list_to_binary(integer_to_list(FollowUserId)),
    {ok, _} = eredis_pool:q(?DB_SRV, ["LREM", Key, 0, IdBin]),
    {ok, CountBin} = eredis_pool:q(?DB_SRV, ["LLEN", Key]),
    Count = list_to_integer(binary_to_list(CountBin)),
    {ok, Count}.

%%--------------------------------------------------------------------
%% @doc フォロワーIDのリストを取得する
%% @end
%%--------------------------------------------------------------------
-spec(get_followers(UserId::integer()) -> [integer()] ).

get_followers(UserId) ->
    Key = get_follower_list_Key(UserId),
    {ok, List} = eredis_pool:q(?DB_SRV, ["LRANGE", Key, 0, -1]),
    lists:map(fun(IdBin) -> list_to_integer(binary_to_list(IdBin)) end, List).

%%--------------------------------------------------------------------
%% @doc フォロワー数を返す
%% @end
%%--------------------------------------------------------------------
-spec(follower_count(UserId::integer()) -> integer()).

follower_count(UserId) ->
    Key = get_follower_list_Key(UserId),
    {ok, CountBin} = eredis_pool:q(?DB_SRV, ["LLEN", Key]),
    list_to_integer(binary_to_list(CountBin)).

%%%===================================================================
%%% Follow API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc フォローを追加する
%% @end
%%--------------------------------------------------------------------
-spec(add_follow(UserId::integer(), FollowUserId::integer()) -> 
             {ok, Count::integer()} ).

add_follow(UserId, FollowUserId) ->
    Key = get_follow_list_Key(UserId),
    {ok, CountBin} = eredis_pool:q(?DB_SRV, ["RPUSH", Key, FollowUserId]),
    Count = list_to_integer(binary_to_list(CountBin)),
    {ok, Count}.

%%--------------------------------------------------------------------
%% @doc フォローを削除する
%% @end
%%--------------------------------------------------------------------
-spec(delete_follow(UserId::integer(), FollowUserId::integer()) -> 
             {ok, Count::integer()} ).

delete_follow(UserId, FollowUserId) ->
    Key = get_follow_list_Key(UserId),
    IdBin = list_to_binary(integer_to_list(FollowUserId)),
    {ok, _} = eredis_pool:q(?DB_SRV, ["LREM", Key, 0, IdBin]),
    {ok, CountBin} = eredis_pool:q(?DB_SRV, ["LLEN", Key]),
    Count = list_to_integer(binary_to_list(CountBin)),
    {ok, Count}.

%%--------------------------------------------------------------------
%% @doc フォローIDのリストを取得する
%% @end
%%--------------------------------------------------------------------
-spec(get_follows(UserId::integer()) -> [integer()] ).

get_follows(UserId) ->
    Key = get_follow_list_Key(UserId),
    {ok, List} = eredis_pool:q(?DB_SRV, ["LRANGE", Key, 0, -1]),
    lists:map(fun(IdBin) -> list_to_integer(binary_to_list(IdBin)) end, List).

%%--------------------------------------------------------------------
%% @doc フォロー数を返す
%% @end
%%--------------------------------------------------------------------
-spec(follow_count(UserId::integer()) -> integer()).

follow_count(UserId) ->
    Key = get_follow_list_Key(UserId),
    {ok, CountBin} = eredis_pool:q(?DB_SRV, ["LLEN", Key]),
    list_to_integer(binary_to_list(CountBin)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_follower_list_Key(Id) when is_integer(Id) ->
    list_to_binary("follower_" ++ integer_to_list(Id)).

get_follow_list_Key(Id) when is_integer(Id) ->
    list_to_binary("follow_" ++ integer_to_list(Id)).
