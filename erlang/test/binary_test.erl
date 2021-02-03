-module(binary_test).
-export([ reverse/1 ]).

%reverse( Bin ) when is_bitstring( Bin ) -> isbits;
reverse( Bin ) when is_binary( Bin ) -> rev_bin( Bin, <<>> );
reverse( B ) -> error.

rev_bin( <<Byte:1/binary,Remainder/binary>>, New ) ->
  io:format( "B:~p~nR:~p~n", [ Byte, Remainder ] ),
  rev_bin( Remainder, <<Byte/binary,New/binary>> );
rev_bin( <<>>, New ) -> New.
