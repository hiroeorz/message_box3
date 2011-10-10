
-record(message, {id           ::integer(),
		  text         ::binary(),
		  created_at   ::tuple(),
                  in_reply_to  ::integer(),
                  user_id      ::integer()
                 }).      
