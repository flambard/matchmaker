{application, matchmaker,
 [{description, "A matchmaker server."},
  {vsn, "0.0.1"},
  {modules, [ matchmaker
            , matchmaker_app
            , matchmaker_pool
            , matchmaker_sup
            ]},
  {registered, [ matchmaker
               ]},
  {mod, {matchmaker_app, []}},
  {applications, [ kernel
                 , stdlib
                 ]},
  {env, []}
 ]}.
