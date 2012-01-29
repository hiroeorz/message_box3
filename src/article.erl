%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(article).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("article.hrl").
-include("message_box3.hrl").

-define(MaxIdKey, <<"max_article_id">>).

%% API
-export([create/3, update/4, delete/2, get_list/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 受け取った記事を保存します。
%% @end
%%--------------------------------------------------------------------
-spec(create(UserId::integer(), Title::binary(), Text::binary()) ->
             {ok, Id::integer(), Key::binary()} | {error, Reason::atom()}).

create(UserId, Title, Text) when is_integer(UserId) and
                                       is_binary(Title) and
                                       is_binary(Text) ->
    Id = get_next_id(),
    DateTime = {date(), time()},
    Article = #article{id=Id, title=Title, text=Text, created_at=DateTime,
                       updated_at=DateTime, user_id=UserId},
    save(UserId, Id, Article).

%%--------------------------------------------------------------------
%% @doc
%% 記事を更新します。
%% @end
%%--------------------------------------------------------------------
-spec(update(UserId::integer(), Id::integer(), 
             Title::binary(), Text::binary()) -> 
             {ok, Id::integer(), Key::binary()} | {error, atom()}).
update(UserId, Id, Title, Text) when is_integer(UserId) and
                                       is_binary(Title) and
                                       is_binary(Text) ->
    Key = get_message_Key(Id),
    case eredis_pool:q(?DB_SRV, ["GET", Key]) of
        {ok, undefined} ->
            {error, not_found};
        {ok, ArticleBin} ->
            DateTime = {date(), time()},
            Article = binary_to_term(ArticleBin),
            NewArticle = Article#article{title = Title, 
                                         text = Text,
                                         updated_at = DateTime},
            save(UserId, Id, NewArticle);
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 受け取ったIdの記事を削除します。
%% @end
%%--------------------------------------------------------------------
-spec(delete(UserId::integer(), Id::integer()) ->
             {ok, deleted} | {error, Reason::atom()}).

delete(UserId, Id) ->
    Key = get_message_Key(Id),
    ListKey = get_users_list_key(UserId),

    case eredis_pool:q(?DB_SRV, ["LREM", ListKey, 0, Key]) of
        {ok, _} ->
            case eredis_pool:q(?DB_SRV, ["DEL", Key]) of
                {ok, _}         -> {ok, deleted};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.    

%%--------------------------------------------------------------------
%% @doc
%% 記事のリストを取得します。
%% @end
%%--------------------------------------------------------------------
-spec(get_list(UserId::integer(), StartCount::integer(), 
               EndCount::integer()) -> {ok, list()} | {error, atom()}).

get_list(UserId, StartCount, EndCount) when is_integer(UserId) and
                                            is_integer(StartCount) and
                                            is_integer(EndCount) ->
    ListKey = get_users_list_key(UserId),

    case eredis_pool:q(?DB_SRV, ["LRANGE", ListKey, 
                                 0 - EndCount, 0 - StartCount]) of
        {ok, []} -> 
            {ok, []};
        {ok, KeyList} ->
            case eredis_pool:q(?DB_SRV, ["MGET" | KeyList]) of
                {ok, BinList} -> 
                    Articles = 
                        lists:map(fun(Bin) ->
                                          case Bin of
                                              undefined -> undefined;
                                              BinData -> binary_to_term(BinData)
                                          end 
                                  end, BinList),
                        {ok, Articles};
                Error ->
                    Error
            end;

        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

save(UserId, Id, Article) ->
    Key = get_message_Key(Id),
    ListKey = get_users_list_key(UserId),
    case eredis_pool:q(?DB_SRV, 
                       ["SET", Key, term_to_binary(Article)]) of
        {ok, <<"OK">>}  ->
            case eredis_pool:q(?DB_SRV, ["RPUSH", ListKey, Key]) of
                {ok, _}         -> {ok, Id, Key};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> 
            {error, Reason}
    end.    

get_next_id() ->
    {ok, MaxIdBin} = eredis_pool:q(?DB_SRV, ["INCR", ?MaxIdKey]),
    list_to_integer(binary_to_list(MaxIdBin)).

get_message_Key(Id) when is_integer(Id) ->
    list_to_binary("art_" ++ integer_to_list(Id));

get_message_Key(Id) when is_binary(Id) ->
    list_to_binary("art_" ++ binary_to_list(Id)).

get_users_list_key(Id) when is_integer(Id) ->
    list_to_binary("art_list_" ++ integer_to_list(Id)).
    
