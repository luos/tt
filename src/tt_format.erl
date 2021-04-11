-module(tt_format).

-export([printable_variable/1, printable_list/1, printable/1]).

printable_variable(Variable) when is_list(Variable) ->
    printable_list(Variable);
printable_variable(Variable)  ->
    printable(Variable).

printable_list(List) when is_list(List) ->
    Sublist = lists:sublist(List, 25),
    printable(Sublist).


printable(Term) -> 
    erlang:iolist_to_binary(io_lib:format("~p", [Term])).

