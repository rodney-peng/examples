Info = ets:module_info().

% get compile options from module info
[ O1 ] = [ L || { compile, L } <- Info ].
{ compile, O2 } = lists:keyfind( compile, 1, Info ).
O1 == O2.

% get time from compile options
[ T1 ] = [ T || { time, T } <- O1 ].
{ time, T2 } = lists:keyfind( time, 1, O2 ).
T1 == T2.
