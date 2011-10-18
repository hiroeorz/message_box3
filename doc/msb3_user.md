

#Module msb3_user#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Authors:__ Hiroe Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_user-3">add_user/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_id_list-1">get_id_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_user-1">get_user/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_user-3"></a>

###add_user/3##




<pre>add_user(Name::string(), Mail::string(), Password::string()) -&gt; {ok, UserId::integer()} | {error, Reason::binary()}</pre>
<br></br>


<a name="get_id-1"></a>

###get_id/1##




<pre>get_id(Name::string()) -&gt; {ok, integer()} | {error, not_found}</pre>
<br></br>


<a name="get_id_list-1"></a>

###get_id_list/1##




<pre>get_id_list(NameList::[string()]) -&gt; [integer()]</pre>
<br></br>


<a name="get_user-1"></a>

###get_user/1##




<pre>get_user(Id_OR_Name::integer() | string()) -&gt; {ok, User::#user{id = undefined | integer(), name = undefined | string(), longname = string(), email = undefined | string(), password = undefined | binary(), icon_url = string(), lat = string(), lng = string(), created_at = undefined | tuple()}} | {error, not_found}</pre>
<br></br>


