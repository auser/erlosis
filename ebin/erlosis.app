{application, erlosis,
 [
  {description, "Erlosis app"},
  {vsn, "0.1"},
  {id, "erlosis"},
  {modules,      []},
  {registered,   []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {erlosis_app, []}},
  {env, [
    {config_file, "env/gitosis.conf"}
  ]}
 ]
}.