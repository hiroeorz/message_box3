
-record(user, {id            ::integer(),
               name          ::string(),
               longname = "" ::string(),
               email         ::string(),
               password      ::binary(),
               icon_url = "" ::string(),
               lat = ""      ::string(),
               lng = ""      ::string(),
               created_at    ::tuple()}).
