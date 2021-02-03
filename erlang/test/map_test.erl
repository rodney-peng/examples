-module(map_test).
-export([count_char/1, print/1]).

count_char(Str) -> count_char( Str, #{} ).

count_char( [H|T], Map ) ->
  case maps:find( H, Map ) of
    { ok, C } -> count_char( T, Map#{ H := C+1 } );
    error -> count_char( T, Map#{ H => 1 } )
  end;
count_char( [], Map ) -> Map.

print(Map) -> printList( maps:to_list(Map) ).

printList( [ {Key,Value} | T ] ) ->
  io:format( "~c(~w): ~w~n", [ Key, Key, Value ] ),
  printList( T );
printList( [] ) -> ok.
