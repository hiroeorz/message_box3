

#Module msb3_login_server#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net`](mailto:shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#authenticate-2">authenticate/2</a></td><td>プロセスプールに対してauthenticate/3を要求する。.</td></tr><tr><td valign="top"><a href="#authenticate-3">authenticate/3</a></td><td>認証を行い、認証にパスしたらセッションキーを新たに生成して返す。.</td></tr><tr><td valign="top"><a href="#login-2">login/2</a></td><td>
ユーザIdとセッションの組み合わせが既に認証済みか確認する
確認がとれた場合はセッションの有効期限を延ばす.</td></tr><tr><td valign="top"><a href="#login-3">login/3</a></td><td>
ユーザIdとセッションの組み合わせが既に認証済みか確認する
確認がとれた場合はセッションの有効期限を延ばす.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="authenticate-2"></a>

###authenticate/2##




<pre>authenticate(Name::string(), Password::string()) -&gt; {ok, SessionKey::string()} | {error, password_incollect}</pre>
<br></br>




プロセスプールに対してauthenticate/3を要求する。<a name="authenticate-3"></a>

###authenticate/3##




<pre>authenticate(Pid::pid(), Name::string(), Password::string()) -&gt; {ok, SessionKey::string()} | {error, password_incollect}</pre>
<br></br>




認証を行い、認証にパスしたらセッションキーを新たに生成して返す。<a name="login-2"></a>

###login/2##




`login(UserId, SessionKey) -> any()`




ユーザIdとセッションの組み合わせが既に認証済みか確認する
確認がとれた場合はセッションの有効期限を延ばす<a name="login-3"></a>

###login/3##




`login(Name_Or_Pid, UserId, SessionKey) -> any()`




ユーザIdとセッションの組み合わせが既に認証済みか確認する
確認がとれた場合はセッションの有効期限を延ばす<a name="start_link-1"></a>

###start_link/1##




<pre>start_link(Name_Or_Args::list() | atom()) -&gt; {ok, Pid::pid()} | ignore | {error, Error::atom()}</pre>
<br></br>





Starts the server
<a name="stop-0"></a>

###stop/0##




<pre>stop() -&gt; ok</pre>
<br></br>


<a name="stop-1"></a>

###stop/1##




<pre>stop(Name_OR_Pid::atom() | pid()) -&gt; ok</pre>
<br></br>


