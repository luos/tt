-module(tt_str).

-export([contains/2]).

contains(Haystack, Needle) when is_list(Haystack), is_list(Needle) ->
    case string:find(Haystack, Needle) of
        nomatch ->
            erlang:error(
                lists:flatten(
                    io_lib:format("Failed asserting that '~s' is in '~s'", [Needle, Haystack])));
        _ ->
            ok
    end;
contains(Haystack, Needle) when is_binary(Haystack), is_binary(Needle) ->
    case binary:match(Haystack, Needle) of
        nomatch ->
            erlang:error(
                lists:flatten(
                    io_lib:format("Failed asserting that '~s' is in '~s'", [Needle, Haystack])));
        _ ->
            ok
    end;
contains(Haystack, Needle) ->
    HaystackType = tt_types:useful_type_of(Haystack),
    NeedleType = tt_types:useful_type_of(Needle),
    erlang:error(
        lists:flatten(
            io_lib:format("The arguments are not of the same type [~s, ~s] ~n"
                          "Haystack:~n~p~n~n Needle:~n~p~n",
                          [HaystackType, NeedleType, Haystack, Needle]))).
