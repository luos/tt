-module(tt_types).

-export([useful_type_of/1, is_proplist/1]).

%
% Returns a useful printable type for the user to help in
% resolving test failures quicker.
useful_type_of(Value) when is_atom(Value) ->
    <<"atom">>;
useful_type_of(Value) when is_binary(Value) ->
    <<"binary">>;
useful_type_of([]) ->
    <<"empty list">>;
useful_type_of(Value) when is_list(Value) ->
    case io_lib:printable_list(Value) of
        true ->
            <<"printable list">>;
        false ->
            case is_proplist(Value) of
                true ->
                    <<"proplist">>;
                false ->
                    <<"list">>
            end
    end;
useful_type_of(Value) when is_integer(Value) ->
    <<"integer">>;
useful_type_of(Value) when is_float(Value) ->
    <<"float">>;
useful_type_of(Value) when is_function(Value) ->
    <<"function">>;
useful_type_of(_Value) ->
    <<"unknown">>.

% crude check for proplistness to print
is_proplist(List) when is_list(List) ->
    lists:all(fun ({_, _}) ->
                      true;
                  (V) when is_atom(V) ->
                      true;
                  (_) ->
                      false
              end,
              List).
