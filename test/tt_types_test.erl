-module(tt_types_test).

-include_lib("eunit/include/eunit.hrl").

is_proplist_given_proplists_returns_true_test() ->
    ?assert(tt_types:is_proplist([{a, b}])),
    ?assert(tt_types:is_proplist([{a, b}, hello])).

is_proplist_given_non_proplists_returns_false_test() ->
    ?assertNot(tt_types:is_proplist("hello world")),
    ?assertNot(tt_types:is_proplist([{a, b, c}])).

given_a_value_returns_the_useful_type_test() ->
    ?assertEqual(<<"empty list">>, tt_types:useful_type_of([])).
