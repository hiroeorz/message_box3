

#Module article#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Authors:__ Hiroe Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create-3">create/3</a></td><td>
受け取った記事を保存します。.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>
受け取ったIdの記事を削除します。.</td></tr><tr><td valign="top"><a href="#get_list-3">get_list/3</a></td><td>
記事のリストを取得します。.</td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td>
記事を更新します。.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="create-3"></a>

###create/3##




<pre>create(UserId::integer(), Title::binary(), Text::binary()) -&gt; {ok, Id::integer(), Key::binary()} | {error, Reason::atom()}</pre>
<br></br>





受け取った記事を保存します。<a name="delete-2"></a>

###delete/2##




<pre>delete(UserId::integer(), Id::integer()) -&gt; {ok, deleted} | {error, Reason::atom()}</pre>
<br></br>





受け取ったIdの記事を削除します。<a name="get_list-3"></a>

###get_list/3##




<pre>get_list(UserId::integer(), StartCount::integer(), EndCount::integer()) -&gt; {ok, list()} | {error, atom()}</pre>
<br></br>





記事のリストを取得します。<a name="update-4"></a>

###update/4##




<pre>update(UserId::integer(), Id::integer(), Title::binary(), Text::binary()) -&gt; {ok, Id::integer(), Key::binary()} | {error, atom()}</pre>
<br></br>





記事を更新します。