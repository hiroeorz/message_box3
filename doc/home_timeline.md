

#Module home_timeline#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@mac-hiroe-orz-17.local`](mailto:shin@mac-hiroe-orz-17.local)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_home_to_followers-2">add_home_to_followers/2</a></td><td>
受け取ったIdのユーザの全フォロワーのhomeに受け取ったメッセージへのキーを保存する。
処理は非同期に行います。.</td></tr><tr><td valign="top"><a href="#add_message_key-2">add_message_key/2</a></td><td>
ホームタイムラインのリストの末尾にメッセージのキーを追加する。.</td></tr><tr><td valign="top"><a href="#get_timeline-2">get_timeline/2</a></td><td>
ホームタイムラインのリストのメッセージを取得する。.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_home_to_followers-2"></a>

###add_home_to_followers/2##




<pre>add_home_to_followers(UserId::integer(), MsgKey::binary()) -&gt; ok</pre>
<br></br>





受け取ったIdのユーザの全フォロワーのhomeに受け取ったメッセージへのキーを保存する。
処理は非同期に行います。<a name="add_message_key-2"></a>

###add_message_key/2##




<pre>add_message_key(UserId::integer(), MsgKey::binary()) -&gt; ok</pre>
<br></br>





ホームタイムラインのリストの末尾にメッセージのキーを追加する。<a name="get_timeline-2"></a>

###get_timeline/2##




<pre>get_timeline(UserId::integer(), Count::integer()) -&gt; {ok, [tuple()]} | {error, Reason::binary()}</pre>
<br></br>





ホームタイムラインのリストのメッセージを取得する。