%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 12 Oct 2011 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(sent_timeline).

%% API
-export([add_message_key/2, get_timeline/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% メンションタイムラインのリストの末尾にメッセージのキーを追加する。
%% @end
%%--------------------------------------------------------------------
-spec(add_message_key(UserId::integer(), MsgKey::binary()) -> ok).

add_message_key(UserId, MsgKey) when is_integer(UserId) and is_binary(MsgKey) ->
    SentKey = get_key(UserId),
    {ok, _} = eredis_pool:q(default, ["RPUSH", SentKey, MsgKey]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% メンションタイムラインのメッセージを取得する。
%% @end
%%--------------------------------------------------------------------
-spec(get_timeline(UserId::integer(), Count::integer()) -> 
             {ok, [tuple()]} | {error, Reason::binary()}).

get_timeline(UserId, Count) ->
    SentKey = get_key(UserId),
    {ok, KeyList} = eredis_pool:q(default, 
                                  ["LRANGE", SentKey, (0 - Count), -1]),
    message:get_message_list(KeyList).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_key(UserId) when is_integer(UserId) ->
    list_to_binary("s_" ++ integer_to_list(UserId)).
