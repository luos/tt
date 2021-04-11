-module(tt_assert_inline_value_test).

-compile({parse_transform, tt}).

-include_lib("eunit/include/eunit.hrl").


given_true_inline_asserting_it_succeeds_test() -> 
    tt:assert(true).

given_false_variable_asserting_it_fails_test() -> 
    try 
        run_failure()
    of 
        _ -> throw("Shouldn't happen")
    catch 
        error:{assert, Error} -> 
            io:format("Error ~p", [Error]),
            Expression = proplists:get_value(expression, Error, not_found),
            tt_str:contains(Expression, "Condition failed: tt:assert(false)")
    end.


run_failure() -> 
    tt:assert(false).