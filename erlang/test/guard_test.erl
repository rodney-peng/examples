-module(guard_test).
-export([test/2, test1/2, test2/2]).

test(X,Y) -> test1(X,Y) =:= test2(X,Y).

test1(X,Y) when X < 2, Y < 3 -> case1;
test1(X,Y) -> case2.

test2(X,Y) when (X < 2) and (Y < 3) -> case1;
test2(X,Y) -> case2.
