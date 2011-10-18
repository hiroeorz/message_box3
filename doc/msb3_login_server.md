

#Module msb3_login_server#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net`](mailto:shin@u720170.xgsfmg6.imtp.tachikawa.mopera.net)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#authenticate-3">authenticate/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="authenticate-3"></a>

###authenticate/3##




<pre>authenticate(Pid::pid(), Name::string(), Password::string()) -&gt; {ok, SessionKey::string()} | {error, password_incollect}</pre>
<br></br>


<a name="start_link-0"></a>

###start_link/0##




<pre>start_link() -&gt; {ok, Pid} | ignore | {error, Error}</pre>
<br></br>





Starts the server
