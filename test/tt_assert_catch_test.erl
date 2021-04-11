-module(tt_assert_catch_test).

-compile({parse_transform, tt}).

-include_lib("eunit/include/eunit.hrl").

test_fail(Argument) ->
    false.

% [{clause,9,[],[],
% [{match,10,
%      {tuple,10,[{atom,10,throw},{var,10,'Result'}]},
%      {'catch',10,
%          {call,10,
%              {remote,10,{atom,10,tt},{atom,10,assert}},
%              [{call,10,
%                   {atom,10,test_fail},
%                   [{integer,10,3}]}]}}}]}]
given_an_inline_as_argument_displays_variable_value_test() ->
    {throw, _Result} = (catch tt:assert(test_fail(3))).



% given_an_inline_as_argument_displays_variable_value_test() ->
%     {failed, Exception} = run(fun() -> 
%         tt:assert(test_fail(3)) 
%     end),
%     ?debugFmt("~p",[Exception]).
