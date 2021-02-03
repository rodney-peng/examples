-module(file_test).
-export([start/0, server/0, list/2, get/2, copy/3, stop/1]).

start() ->
  io:format( "Server started.~n" ),                        
  spawn( file_test, server, [] ).

server() ->
  receive
    { Client, list, Dir } -> Client ! file:list_dir( Dir );
    { Client, get, Filename } -> Client ! file:read_file( Filename );
    { Client, copy, FNFrom, FNTo } -> Client ! file:copy( FNFrom, FNTo ); 
    { Client, exit } -> io:format( "Server exits.~n" ), exit(normal)
  end,
  server().

list(Server, Dir) ->
  Server ! { self(), list, Dir },
  receive Response -> Response end.

get(Server, Filename) ->
  Server ! { self(), get, Filename },
  receive Response -> Response end.

copy(Server, FNFrom, FNTo) ->
  Server ! { self(), copy, FNFrom, FNTo },
  receive Response -> Response end.

stop(Server) ->
  io:format( "Client sends exit.~n" ),
  Server ! { self(), exit }.
