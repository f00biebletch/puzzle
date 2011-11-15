{application, ndexe,
 [{description, "ndexe"},
  {vsn, %%NDEXE_VERSION%%},
  {modules, [
    ndexe_app,
    executor,
    executor_sup,
    ndexe_deps,
    tpt_run
  ]},
  {registered, []},
  {mod, {ndexe_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
