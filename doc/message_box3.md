

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
認証を実行し、以後要求を実行する為のセッションキーを得る。.</td></tr><tr><td valign="top"><a href="#create_user-3">create_user/3</a></td><td>
ユーザを新規に作成する。.</td></tr><tr><td valign="top"><a href="#get_user-1">get_user/1</a></td><td>
ユーザ情報を取得する.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="authenticate-2"></a>

###authenticate/2##




<pre>authenticate(Name::string(), Password::string()) -&gt; {ok, SessionKey::string()} | {error, password_incollect}</pre>
<br></br>





認証を実行し、以後要求を実行する為のセッションキーを得る。
<a name="create_user-3"></a>

###create_user/3##




<pre>create_user(Name::string(), Mail::string(), Password::string()) -&gt; {ok, UserId::integer()} | {error, Reason::binary()}</pre>
<br></br>





ユーザを新規に作成する。
<a name="get_user-1"></a>

###get_user/1##




<pre>get_user(UserId::integer()) -&gt; {ok, User::#user{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), icon_url = string(), lat = string(), lng = string(), created_at = undefined | tuple()}} | {error, not_found}</pre>
<br></br>





ユーザ情報を取得する
<a name="start_link-0"></a>

###start_link/0##




<pre>start_link() -&gt; {ok, Pid::pid()} | ignore | {error, Error::atom()}</pre>
<br></br>


