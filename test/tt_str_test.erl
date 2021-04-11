-module(tt_str_test).

-include_lib("eunit/include/eunit.hrl").

contains_success_test() ->
    ok = tt_str:contains("abc", "a"),
    ok = tt_str:contains(<<"abc">>, <<"a">>).

contains_fail_test_() ->
    [?_assertError("Failed asserting that 'd' is in 'abc'", tt_str:contains("abc", "d")),
     ?_assertError("Failed asserting that 'd' is in 'abc'",
                   tt_str:contains(<<"abc">>, <<"d">>))].

contains_strictness_test() ->
    Caught = (catch tt_str:contains(<<"a">>, "a")),
    io:format("Caught: ~p", [Caught]),
    {'EXIT', {Error, _}} = Caught,
    tt_str:contains(Error, "The arguments are not of the same type [binary, printable list]").
