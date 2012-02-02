%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 12 Oct 2011 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(mentions_timeline).

%%Include
-include_lib("eunit/include/eunit.hrl").
-include("message_box3.hrl").

%% API
-export([add_mention/2, add_message_key/2, get_timeline/2]).

-define(SEPARATOR, "\s\n").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%% 受け取ったテキストからリプライ送信先ユーザを特定して各ユーザのmentionsリストに
%% メッセージのキーを保存します。
%% 処理は非同期に行います。
%% @end
%%--------------------------------------------------------------------
-spec(add_mention(MsgKey::binary(), TextBin::binary()) -> ok).

add_mention(MsgKey, TextBin) when is_binary(MsgKey) and
                                  is_binary(TextBin) ->
    NameList = get_reply_list(TextBin),
    IdList = msb3_user:get_id_list(NameList),
    lists:map(fun(Id) -> add_message_key(Id, MsgKey) end, IdList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% メンションタイムラインのリストの末尾にメッセージのキーを追加する。
%% @end
%%--------------------------------------------------------------------
-spec(add_message_key(UserId::integer(), MsgKey::binary()) -> ok).

add_message_key(UserId, MsgKey) when is_integer(UserId) and is_binary(MsgKey) ->
    MentionsKey = get_key(UserId),
    {ok, _} = eredis_pool:q(?DB_SRV, ["RPUSH", MentionsKey, MsgKey]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% メンションタイムラインのメッセージを取得する。
%% @end
%%--------------------------------------------------------------------
-spec(get_timeline(UserId::integer(), Count::integer()) -> 
             {ok, [tuple()]} | {error, Reason::binary()}).

get_timeline(UserId, Count) ->
    MentionsKey = get_key(UserId),
    {ok, KeyList} = eredis_pool:q(?DB_SRV, 
                                  ["LRANGE", MentionsKey, (0 - Count), -1]),
    message:get_message_list(KeyList).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_key(UserId) when is_integer(UserId) ->
    list_to_binary("m_" ++ integer_to_list(UserId)).

%%
%% @doc create reply name list from tweet text.
%%
-spec(get_reply_list(string()) -> list(atom()) ).

get_reply_list(Text) when is_binary(Text) ->
    get_reply_list(binary_to_list(Text));

get_reply_list(Text) when is_list(Text) ->
    Tokens = string:tokens(Text, ?SEPARATOR),
    get_reply_list(Tokens, []).

get_reply_list([], List) -> lists:usort(List);

get_reply_list(Tokens, List) when is_list(Tokens) ->
    [Token | Tail] = Tokens,
    case string:sub_string(Token, 1, 1) of
	"@" ->
	    UserNameStr = string:sub_string(Token, 2, length(Token)),
	    get_reply_list(Tail, [UserNameStr | List]);
	_Other ->
	    get_reply_list(Tail, List)
    end.
