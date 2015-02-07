{application, matchmaker,
 [{description, "A matchmaker server."},
  {vsn, "0.0.1"},
  {modules, [ matchmaker
            , matchmaker_app
            , matchmaker_pool
            , matchmaker_server
            , matchmaker_sup
            ]},
  {registered, [ matchmaker_sup
               ]},
  {mod, {matchmaker_app, []}},
  {applications, [ kernel
                 , stdlib
                 ]},
  {env, []}
 ]}.
