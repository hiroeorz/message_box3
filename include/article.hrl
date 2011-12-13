
-record(article, {id           ::integer(),
                  title        ::binary(),
		  text         ::binary(),
		  created_at   ::tuple(),
		  updated_at   ::tuple(),
                  user_id      ::integer()
                 }).      
