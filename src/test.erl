-module(test).
-compile({parse_transform, tt}).

-export([this_is_a_test/1]).
-export([this_is_a_test/2]).
-export([fail/0]).

this_is_a_test(A) -> 
    tt:assert(lists:member(A, [1,2,3])).

this_is_a_test(A, B) -> 
    tt:assert(lists:member(A, B)).


fail() -> 
   this_is_a_test(99, [1,2,3]).