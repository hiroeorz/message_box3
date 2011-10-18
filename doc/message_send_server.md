

#Module message_send_server#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Hiroe Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_message-3">add_message/3</a></td><td>
プロセスプールからワーカーを一つ取り出してadd_message/4の処理を行います。.</td></tr><tr><td valign="top"><a href="#add_message-4">add_message/4</a></td><td>受け取ったメッセージを保存します。.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_message-3"></a>

###add_message/3##




<pre>add_message(UserId::integer(), Text::list(), InReplyTo::integer()) -&gt; {ok, MessageId::integer(), MessgeKey::binary()} | {error, Reason::binary()}</pre>
<br></br>





プロセスプールからワーカーを一つ取り出してadd_message/4の処理を行います。<a name="add_message-4"></a>

###add_message/4##




<pre>add_message(Name_OR_Pid::pid() | atom(), UserId::integer(), Text::list(), InReplyTo::integer()) -&gt; {ok, MessageId::integer(), MessgeKey::binary()} | {error, Reason::binary()}</pre>
<br></br>




受け取ったメッセージを保存します。<a name="start_link-1"></a>

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


