-module(tt_str_test).
-include_lib("eunit/include/eunit.hrl").

contains_success_test() -> 
    ok = tt_str:contains("abc", "a").

contains_fail_test_() -> 
    ?_assertError("Failed asserting that 'd' is in 'abc'", tt_str:contains("abc", "d")).

