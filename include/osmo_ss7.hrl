
-type role()	   :: asp | sg.

-record(sigtran_peer, {
	ip,
	port		:: 1..65535,
	point_code
}).

-record(sigtran_link, {
	type		:: atom(),
	name		:: string(),
	linkset_name	:: string(),
	sls		:: non_neg_integer(),
	local		:: #sigtran_peer{},
	remote		:: #sigtran_peer{},
	role		:: role()
}).


