%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(msb3_util).

%% API
-export([sleep/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 一定時間処理を停止します。
%% @end
%%--------------------------------------------------------------------
-spec(sleep(MSec::integer()) -> ok ).

sleep(MSec) when is_integer(MSec) ->
    receive
    after MSec -> ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================