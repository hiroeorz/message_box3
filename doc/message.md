

#Module message#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@u752230.xgsfmg14.imtp.tachikawa.mopera.net`](mailto:shin@u752230.xgsfmg14.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_message-1">get_message/1</a></td><td>
指定されたIdのメッセージを返します。.</td></tr><tr><td valign="top"><a href="#get_message_list-1">get_message_list/1</a></td><td>
指定されたIdリストに対応するメッセージを取得します。.</td></tr><tr><td valign="top"><a href="#save_message-2">save_message/2</a></td><td>
受け取ったメッセージを保存します。.</td></tr><tr><td valign="top"><a href="#save_message-3">save_message/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="get_message-1"></a>

###get_message/1##




<pre>get_message(Id::integer() | string() | binary()) -&gt; {ok, #message{id = undefined | integer(), text = undefined | binary(), created_at = undefined | tuple(), in_reply_to = undefined | integer(), user_id = undefined | integer()}} | {error, Reason::binary()}</pre>
<br></br>





指定されたIdのメッセージを返します。<a name="get_message_list-1"></a>

###get_message_list/1##




<pre>get_message_list(MessageIdList::[integer()] | [binary()]) -&gt; {ok, [tuple()]} | {error, Reason::binary()}</pre>
<br></br>





指定されたIdリストに対応するメッセージを取得します。<a name="save_message-2"></a>

###save_message/2##




<pre>save_message(UserId::integer(), Text::string()) -&gt; {ok, Id::integer(), Key::binary()} | {error, Reason::binary()}</pre>
<br></br>





受け取ったメッセージを保存します。<a name="save_message-3"></a>

###save_message/3##




<pre>save_message(UserId::integer(), Text::string(), InReplyTo::integer() | undefined) -&gt; {ok, Id::integer(), Key::binary()} | {error, Reason::binary()}</pre>
<br></br>


