

#Module msb3_session#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net`](mailto:shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_new_session-2">add_new_session/2</a></td><td>新たなセッションをユーザのセッションリストに追加する.</td></tr><tr><td valign="top"><a href="#check_session_expire-2">check_session_expire/2</a></td><td>
セッションが有効かどうか確認する
無効であればユーザのセッションリストから削除する.</td></tr><tr><td valign="top"><a href="#update_expire-2">update_expire/2</a></td><td>セッションの有効期限を延長する.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_new_session-2"></a>

###add_new_session/2##




<pre>add_new_session(UserId::integer(), SessionKey::string()) -&gt; ok</pre>
<br></br>




新たなセッションをユーザのセッションリストに追加する<a name="check_session_expire-2"></a>

###check_session_expire/2##




<pre>check_session_expire(UserId::integer(), SessionKey::string()) -&gt; ok | expired</pre>
<br></br>





セッションが有効かどうか確認する
無効であればユーザのセッションリストから削除する<a name="update_expire-2"></a>

###update_expire/2##




<pre>update_expire(UserId::integer(), SessionKey::string()) -&gt; ok</pre>
<br></br>




セッションの有効期限を延長する