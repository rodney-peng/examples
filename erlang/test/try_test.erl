-module(try_test).
-export([start/0, start1/0, start2/0, start3/0]).

start() ->
  try true of
    Value -> io:format("try~n")
  catch
    Type:Why -> io:format("catch~n")
  after
    io:format("after~n")
  end.
% return ok

start1() ->
  try 1
  catch
    _ -> io:format("catch~n")
  after
    io:format("after~n")
  end.
% return try expression

dummy() -> throw("dummy").

start2() ->
  try dummy() of
    _ -> io:format("try~n")
  catch
    _:_ -> io:format("catch~n") 
  after
    io:format("after~n")
  end.
% return ok

start3() ->
  try dummy()
  catch
    Type: Why -> { Type, Why, erlang:get_stacktrace() }  
  end.
