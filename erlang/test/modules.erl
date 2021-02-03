-module(modules).
-export([ foreach/1, print/0, export_most/0, most_common_export/0 ]).

all() -> code:all_loaded().

foreach( Fun ) -> foreach( Fun, 1, all() ).

foreach( Fun, No, [Head|Tail] ) ->
  Fun( No, Head ),
  foreach( Fun, No+1, Tail );
foreach( _Fun, No, [] ) -> io:format( "Total ~w~n", [ No-1 ] ).

print() -> foreach( fun print/2 ). 

print( No, Mod ) -> io:format( "~w: ~p~n", [ No, Mod ] ).

% the module exports the most functions
export_most() ->
  Exports = [ { Mod, length( Mod:module_info(exports) ) } || { Mod, _Loaded } <- all() ],
%  export_most( { none, 0 }, Exports ).
  lists:reverse(lists:ukeysort( 2, Exports )).

export_most( { _MaxMod, MaxExports }, [ { _Mod, Num } = Head | Tail ] ) when Num > MaxExports ->
  export_most( Head, Tail );
export_most( Max, [_Head | Tail] ) -> export_most( Max, Tail );
export_most( Max, [] ) -> Max.

% the function most common in all modules
most_common_export() ->
  ExportLists = [ mod_exports( Mod:module_info(exports), #{} ) || { Mod, _Loaded } <- all() ],
  ExportList = consolidate_exports( ExportLists, #{} ),
%  lists:foreach( fun({ Fun, Count }) -> io:format( "~w:~w~n", [ Fun, Count ] ) end, ExportList ).
%  export_most( { none, 0 }, ExportList ).
  lists:reverse(lists:ukeysort( 2, ExportList )).

% the name with different arities counts only once
mod_exports( [ { Fun, _Arity } | Tail ], Map ) ->
  case maps:find( Fun, Map ) of
    { ok, N } -> mod_exports( Tail, Map#{ Fun := N+1 } );
    error -> mod_exports( Tail, Map#{ Fun => 1 } )
  end;
mod_exports( [], Map ) -> maps:keys(Map).

consolidate_exports( [ Exports | Modules ], Map ) ->
  consolidate_exports( Exports, Modules, Map );
consolidate_exports( [], Map ) -> maps:to_list(Map).

%-define(ExcludeList,[ module_info, module, start, init ]).

-ifdef(ExcludeList).

consolidate_exports( [ Fun | Exports ], Modules, Map ) ->
  Excluded = lists:any( fun(Ex) -> Ex =:= Fun end, ?ExcludeList ),
  if
    not Excluded ->
      case maps:find( Fun, Map ) of
        { ok, N } -> consolidate_exports( Exports, Modules, Map#{ Fun := N+1 } );
        error -> consolidate_exports( Exports, Modules, Map#{ Fun => 1 } )
      end;
    Excluded -> consolidate_exports( Exports, Modules, Map )
  end; 
consolidate_exports( [], Modules, Map ) ->
  consolidate_exports( Modules, Map ).

-else.

consolidate_exports( [ Fun | Exports ], Modules, Map ) ->
  case maps:find( Fun, Map ) of
    { ok, N } -> consolidate_exports( Exports, Modules, Map#{ Fun := N+1 } );
    error -> consolidate_exports( Exports, Modules, Map#{ Fun => 1 } )
  end;
consolidate_exports( [], Modules, Map ) ->
  consolidate_exports( Modules, Map ).

-endif.
