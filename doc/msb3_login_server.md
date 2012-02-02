

#Module msb3_login_server#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net`](mailto:shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#authenticate-2">authenticate/2</a></td><td>認証を行い、認証にパスしたらセッションキーを新たに生成して返す。.</td></tr><tr><td valign="top"><a href="#login-2">login/2</a></td><td>
ユーザIdとセッションの組み合わせが既に認証済みか確認する
確認がとれた場合はセッションの有効期限を延ばす.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="authenticate-2"></a>

###authenticate/2##




<pre>authenticate(Name, Password) -&gt; {ok, SessionKey} | {error, password_incollect}</pre>
<ul class="definitions"><li><pre>Name = string()</pre></li><li><pre>Password = string()</pre></li><li><pre>SessionKey = string()</pre></li></ul>



認証を行い、認証にパスしたらセッションキーを新たに生成して返す。<a name="login-2"></a>

###login/2##




<pre>login(UserId, SessionKey) -&gt; ok | expired</pre>
<ul class="definitions"><li><pre>UserId = integer()</pre></li><li><pre>SessionKey = string()</pre></li></ul>




ユーザIdとセッションの組み合わせが既に認証済みか確認する
確認がとれた場合はセッションの有効期限を延ばす