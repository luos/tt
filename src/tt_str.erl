-module(tt_str).
-export([contains/2]).
contains(Haystack, Subject) when is_list(Haystack), is_list(Subject) ->
    case string:find(Haystack, Subject) of
        nomatch ->
            erlang:error(
                lists:flatten(io_lib:format("Failed asserting that '~s' is in '~s'", [Subject, Haystack])));
        _ ->
            ok
    end.
