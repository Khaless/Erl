{application, erl_http,
 [{description, "erl_http"},
  {vsn, "0.01"},
  {modules, [
    erl_http,
    erl_http_app,
    erl_http_sup,
    erl_http_web,
    erl_http_deps
  ]},
  {registered, []},
  {mod, {erl_http_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
