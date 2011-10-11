%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u752230.xgsfmg14.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2011 by Hiroe Shin <shin@u752230.xgsfmg14.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(message).

%% Include
-include("message.hrl").

-define(MaxMessageIdKey, <<"max_message_id">>).

%% API
-export([save_message/2, save_message/3, get_message/1, get_message_list/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 受け取ったメッセージを保存します。
%% @end
%%--------------------------------------------------------------------
-spec(save_message(UserId::integer(), Text::string()) -> 
             {ok, Id::integer()} | {error, Reason::binary()}).

save_message(UserId, Text) when is_integer(UserId) and is_list(Text)  ->
    save_message(UserId, Text, undefined).

-spec(save_message(UserId::integer(), Text::string(), InReplyTo::integer()) ->
             {ok, Id::integer()} | {error, Reason::binary()}).

save_message(UserId, Text, InReplyTo) when is_integer(UserId) and
                                           is_list(Text)  ->
    Id = get_next_id(),
    DateTime = {date(), time()},

    Message = #message{id=Id, text=Text, created_at=DateTime,
                       in_reply_to=InReplyTo, user_id=UserId},
    
    Key = get_message_Key(Id),
    case eredis_pool:q(default, ["SET", Key, term_to_binary(Message)]) of
        {ok, <<"OK">>}  -> {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 指定されたIdのメッセージを返します。
%% @end
%%--------------------------------------------------------------------
-spec(get_message(Id::integer()|string()|binary()) -> 
             {ok, #message{}} | {error, Reason::binary()}).

get_message(Id) ->
    Key = get_message_Key(Id),

    case eredis_pool:q(default, ["GET", Key]) of
        {ok, Bin} -> binary_to_term(Bin);
        Error     -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% 指定されたIdリストに対応するメッセージを取得します。
%% @end
%%--------------------------------------------------------------------
-spec(get_message_list(MessageIdList::[integer()]) -> 
             {ok, [tuple()]} | {error, Reason::binary()}).

get_message_list(MessageIdList) when is_list(MessageIdList) ->
    KeyList = lists:map(fun(Id) -> get_message_Key(Id) end, MessageIdList),

    case eredis_pool:q(default, ["MGET" | KeyList]) of
        {ok, BinList} -> 
            {ok, lists:map(fun(Bin) -> binary_to_term(Bin) end, BinList)};
        Error ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_next_id() ->
    {ok, MaxIdBin} = eredis_pool:q(default, ["INCR", ?MaxMessageIdKey]),
    list_to_integer(binary_to_list(MaxIdBin)) + 1.

get_message_Key(Id) when is_integer(Id) ->
    list_to_binary("msg_" ++ integer_to_list(Id));

get_message_Key(Id) when is_binary(Id) ->
    list_to_binary("msg_" ++ binary_to_list(Id)).
