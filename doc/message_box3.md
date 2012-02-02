

#Module message_box3#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`shin@mac-hiroe-orz-17.local`](mailto:shin@mac-hiroe-orz-17.local)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#authenticate-2">authenticate/2</a></td><td>
認証を実行し、以後要求を実行する為のセッションキーを得る。.</td></tr><tr><td valign="top"><a href="#connect_dbsrv-1">connect_dbsrv/1</a></td><td>
Connect to eredis_pool database server.</td></tr><tr><td valign="top"><a href="#create_user-3">create_user/3</a></td><td>
ユーザを新規に作成する。.</td></tr><tr><td valign="top"><a href="#follow-3">follow/3</a></td><td>
他のユーザをフォローする。.</td></tr><tr><td valign="top"><a href="#get_home_timeline-3">get_home_timeline/3</a></td><td>
ユーザーのホームタイムラインを取得する。.</td></tr><tr><td valign="top"><a href="#get_mentions_timeline-3">get_mentions_timeline/3</a></td><td>
ユーザーのリプライタイムラインを取得する。.</td></tr><tr><td valign="top"><a href="#get_sent_timeline-3">get_sent_timeline/3</a></td><td>
ユーザーの送信タイムラインを取得する。.</td></tr><tr><td valign="top"><a href="#get_user-1">get_user/1</a></td><td>
ユーザ情報を取得する.</td></tr><tr><td valign="top"><a href="#send_message-4">send_message/4</a></td><td>
メッセージを送信する。.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>
Start Server。.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>
Stop Server。.</td></tr><tr><td valign="top"><a href="#unfollow-3">unfollow/3</a></td><td>
フォロー中のユーザをフォローリストから外す。.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="authenticate-2"></a>

###authenticate/2##




<pre>authenticate(Name, Password) -&gt; {ok, SessionKey} | {error, password_incollect}</pre>
<ul class="definitions"><li><pre>Name = string()</pre></li><li><pre>Password = string()</pre></li><li><pre>SessionKey = string()</pre></li></ul>




認証を実行し、以後要求を実行する為のセッションキーを得る。
<a name="connect_dbsrv-1"></a>

###connect_dbsrv/1##




<pre>connect_dbsrv(Node) -&gt; true | false</pre>
<ul class="definitions"><li><pre>Node = node()</pre></li></ul>




Connect to eredis_pool database server.
<a name="create_user-3"></a>

###create_user/3##




<pre>create_user(Name, Mail, Password) -&gt; {ok, UserId} | {error, Reason}</pre>
<ul class="definitions"><li><pre>Name = string()</pre></li><li><pre>Mail = string()</pre></li><li><pre>Password = string()</pre></li><li><pre>UserId = integer()</pre></li><li><pre>Reason = binary()</pre></li></ul>




ユーザを新規に作成する。
<a name="follow-3"></a>

###follow/3##




<pre>follow(UserId, SessionKey, FollowUserId) -&gt; {ok, {follow, FollowCount}, {follower, FollowerCount}} | {error, session_expired}</pre>
<ul class="definitions"><li><pre>UserId = integer()</pre></li><li><pre>SessionKey = string()</pre></li><li><pre>FollowUserId = integer()</pre></li><li><pre>FollowCount = integer()</pre></li><li><pre>FollowerCount = integer()</pre></li></ul>




他のユーザをフォローする。
<a name="get_home_timeline-3"></a>

###get_home_timeline/3##




<pre>get_home_timeline(UserId, SessionKey, Count) -&gt; {ok, [tuple()]} | {error, Reason}</pre>
<ul class="definitions"><li><pre>UserId = integer()</pre></li><li><pre>SessionKey = string()</pre></li><li><pre>Count = integer()</pre></li><li><pre>Reason = binary()</pre></li></ul>




ユーザーのホームタイムラインを取得する。
<a name="get_mentions_timeline-3"></a>

###get_mentions_timeline/3##




<pre>get_mentions_timeline(UserId, SessionKey, Count) -&gt; {ok, [tuple()]} | {error, Reason::binary()}</pre>
<ul class="definitions"><li><pre>UserId = integer()</pre></li><li><pre>SessionKey = string()</pre></li><li><pre>Count = integer()</pre></li></ul>




ユーザーのリプライタイムラインを取得する。
<a name="get_sent_timeline-3"></a>

###get_sent_timeline/3##




<pre>get_sent_timeline(UserId, SessionKey, Count) -&gt; {ok, [tuple()]} | {error, Reason}</pre>
<ul class="definitions"><li><pre>UserId = integer()</pre></li><li><pre>SessionKey = string()</pre></li><li><pre>Count = integer()</pre></li><li><pre>Reason = binary()</pre></li></ul>




ユーザーの送信タイムラインを取得する。
<a name="get_user-1"></a>

###get_user/1##




<pre>get_user(UserId) -&gt; {ok, User} | {error, not_found}</pre>
<ul class="definitions"><li><pre>UserId = integer()</pre></li><li><pre>User = #user{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), icon_url = string(), lat = string(), lng = string(), created_at = undefined | tuple()}</pre></li></ul>




ユーザ情報を取得する
<a name="send_message-4"></a>

###send_message/4##




<pre>send_message(UserId, SessionKey, Text, InReplyTo::InReplyTo | undefined) -&gt; {ok, MessageId} | {error, session_expired}</pre>
<ul class="definitions"><li><pre>UserId = integer()</pre></li><li><pre>SessionKey = string()</pre></li><li><pre>Text = string()</pre></li><li><pre>InReplyTo = integer()</pre></li><li><pre>MessageId = integer()</pre></li></ul>




メッセージを送信する。
<a name="start-0"></a>

###start/0##




<pre>start() -&gt; ok</pre>
<br></br>





Start Server。
<a name="start_link-0"></a>

###start_link/0##




<pre>start_link() -&gt; {ok, Pid} | ignore | {error, Error}</pre>
<ul class="definitions"><li><pre>Pid = pid()</pre></li><li><pre>Error = atom()</pre></li></ul>

<a name="stop-0"></a>

###stop/0##




<pre>stop() -&gt; ok</pre>
<br></br>





Stop Server。
<a name="unfollow-3"></a>

###unfollow/3##




<pre>unfollow(UserId, SessionKey, FollowUserId) -&gt; {ok, {follow, FollowCount}, {follower, FollowerCount}} | {error, session_expired}</pre>
<ul class="definitions"><li><pre>UserId = integer()</pre></li><li><pre>SessionKey = string()</pre></li><li><pre>FollowUserId = integer()</pre></li><li><pre>FollowCount = integer()</pre></li><li><pre>FollowerCount = integer()</pre></li></ul>




フォロー中のユーザをフォローリストから外す。
