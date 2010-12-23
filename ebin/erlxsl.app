{application,
 erlxsl,
 [{description,"C-XSLT Bindings for Erlang/OTP"},
  {vsn,"0.0.1"},
  {mod, {erlxsl_app, []}},
  {modules,
   [erlxsl_app,
    erlxsl_fast_log,
    erlxsl_marshall,
    erlxsl_port_server,
    erlxsl_sup]},
  {registered,[erlxsl_port_server,erlxsl_fast_logger]},
  {applications,[kernel,stdlib,sasl]},
  {env,[
	{driver_options, [
		{driver, "default_provider"}
	]}
  ]}]}.