

#Module user_relation#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Authors:__ Hiroe Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_follow-2">add_follow/2</a></td><td>フォローを追加する.</td></tr><tr><td valign="top"><a href="#add_follower-2">add_follower/2</a></td><td>フォロワーを追加する.</td></tr><tr><td valign="top"><a href="#delete_follow-2">delete_follow/2</a></td><td>フォローを削除する.</td></tr><tr><td valign="top"><a href="#delete_follower-2">delete_follower/2</a></td><td>フォロワーを削除する.</td></tr><tr><td valign="top"><a href="#follow_count-1">follow_count/1</a></td><td>フォロー数を返す.</td></tr><tr><td valign="top"><a href="#follower_count-1">follower_count/1</a></td><td>フォロワー数を返す.</td></tr><tr><td valign="top"><a href="#get_followers-1">get_followers/1</a></td><td>フォロワーIDのリストを取得する.</td></tr><tr><td valign="top"><a href="#get_follows-1">get_follows/1</a></td><td>フォローIDのリストを取得する.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_follow-2"></a>

###add_follow/2##




<pre>add_follow(UserId::integer(), FollowUserId::integer()) -&gt; {ok, Count::integer()}</pre>
<br></br>




フォローを追加する<a name="add_follower-2"></a>

###add_follower/2##




<pre>add_follower(UserId::integer(), FollowUserId::integer()) -&gt; {ok, Count::integer()}</pre>
<br></br>




フォロワーを追加する<a name="delete_follow-2"></a>

###delete_follow/2##




<pre>delete_follow(UserId::integer(), FollowUserId::integer()) -&gt; {ok, Count::integer()}</pre>
<br></br>




フォローを削除する<a name="delete_follower-2"></a>

###delete_follower/2##




<pre>delete_follower(UserId::integer(), FollowUserId::integer()) -&gt; {ok, Count::integer()}</pre>
<br></br>




フォロワーを削除する<a name="follow_count-1"></a>

###follow_count/1##




<pre>follow_count(UserId::integer()) -&gt; integer()</pre>
<br></br>




フォロー数を返す<a name="follower_count-1"></a>

###follower_count/1##




<pre>follower_count(UserId::integer()) -&gt; integer()</pre>
<br></br>




フォロワー数を返す<a name="get_followers-1"></a>

###get_followers/1##




<pre>get_followers(UserId::integer()) -&gt; [integer()]</pre>
<br></br>




フォロワーIDのリストを取得する<a name="get_follows-1"></a>

###get_follows/1##




<pre>get_follows(UserId::integer()) -&gt; [integer()]</pre>
<br></br>




フォローIDのリストを取得する