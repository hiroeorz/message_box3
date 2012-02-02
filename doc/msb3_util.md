

#Module msb3_util#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Authors:__ Hiroe Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_crypted_password-2">create_crypted_password/2</a></td><td>保存の為に暗号化されたパスワードを生成します。.</td></tr><tr><td valign="top"><a href="#create_session_key-2">create_session_key/2</a></td><td>与えられたIdとパスワードからセッションキーを生成します。.</td></tr><tr><td valign="top"><a href="#sleep-1">sleep/1</a></td><td>一定時間処理を停止します。.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="create_crypted_password-2"></a>

###create_crypted_password/2##




<pre>create_crypted_password(User::#user{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), icon_url = string(), lat = string(), lng = string(), created_at = undefined | tuple()}, Password::string()) -&gt; CryptedPassword::string()</pre>
<br></br>




保存の為に暗号化されたパスワードを生成します。<a name="create_session_key-2"></a>

###create_session_key/2##




<pre>create_session_key(Id::integer(), Password::string()) -&gt; SessionKey::string()</pre>
<br></br>




与えられたIdとパスワードからセッションキーを生成します。<a name="sleep-1"></a>

###sleep/1##




<pre>sleep(MSec::integer()) -&gt; ok</pre>
<br></br>




一定時間処理を停止します。