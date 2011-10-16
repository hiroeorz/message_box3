%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(user_relation).

%% API
-export([add_follower/2, get_followers/1, add_follow/2, get_follows/1]).

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
    {ok, CountBin} = eredis_pool:q(default, ["RPUSH", Key, FollowUserId]),
    Count = list_to_integer(binary_to_list(CountBin)),
    {ok, Count}.

%%--------------------------------------------------------------------
%% @doc フォロワーIDのリストを取得する
%% @end
%%--------------------------------------------------------------------
-spec(get_followers(UserId::integer()) -> [integer()] ).

get_followers(UserId) ->
    Key = get_follower_list_Key(UserId),
    {ok, List} = eredis_pool:q(default, ["LRANGE", Key, 0, -1]),
    lists:map(fun(IdBin) -> list_to_integer(binary_to_list(IdBin)) end, List).

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
    {ok, CountBin} = eredis_pool:q(default, ["RPUSH", Key, FollowUserId]),
    Count = list_to_integer(binary_to_list(CountBin)),
    {ok, Count}.

%%--------------------------------------------------------------------
%% @doc フォローIDのリストを取得する
%% @end
%%--------------------------------------------------------------------
-spec(get_follows(UserId::integer()) -> [integer()] ).

get_follows(UserId) ->
    Key = get_follow_list_Key(UserId),
    {ok, List} = eredis_pool:q(default, ["LRANGE", Key, 0, -1]),
    lists:map(fun(IdBin) -> list_to_integer(binary_to_list(IdBin)) end, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_follower_list_Key(Id) when is_integer(Id) ->
    list_to_binary("follower_" ++ integer_to_list(Id)).

get_follow_list_Key(Id) when is_integer(Id) ->
    list_to_binary("follow_" ++ integer_to_list(Id)).
