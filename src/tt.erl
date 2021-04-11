-module(tt).

%-compile({parse_transform, parse_trans_codegen}).

-pt_pp_src(true).

-export([parse_transform/2, format_error/1]).

-compile(export_all).

-define(MAX_LIST_DISPLAY_LEN, application:get_env(tt, max_list_display_len, 25)).

assert(_Result) ->
    throw("Reached unreachable code").

assert(Result, Context, Variables) ->
    case Result of
        true ->
            io:format("Ok ~p~n", [Context]);
        _ ->
            handle_fail(Result, Context, Variables),
            io:format("Not ok~n ", [])
    end.

assert_equal(Expected, Current) ->
    {expected, Expected, current, Current}.

parse_transform(Forms, _Options) ->
    io:format("Forms: ~p~n", [Forms]),
    Transformed = lists:flatmap(fun transform_form/1, Forms),
    io:format("~nTransformed: ~p~n", [Transformed]),
    Transformed.

transform_form({function, Line, FunctionName, Arity, Inside} = _Def) ->
    io:format("Fn~n"),
    [{function, Line, FunctionName, Arity, transform_fn_def_body(Inside)}];
% %transform_form({call,_,{remote,_,{atom,_,tt},{atom,_,assert}}, Inside} = Form) ->
% %    io:format("Call: ~p~n", [Form]),
% %    [Form];
transform_form(Form) ->
    [Form].

transform_fn_def_body(Inside) ->
    lists:map(fun transform_fn_clause/1, Inside).

transform_fn_clause({clause, Line1, Args, [], Body}) ->
    io:format("Fn clause"),
    {Body2, _FunContext} = lists:foldl(fun transform_fn_body_form/2, {[], #{}}, Body),
    {clause, Line1, Args, [], lists:reverse(Body2)}.

transform_fn_body_form({call,
                        Line,
                        {remote, Line2, {atom, Line3, tt}, {atom, Line4, assert}},
                        Args} =
                           F,
                       {Forms, FunContext}) ->
    ArgVars = extract_assert_call_context(Args),
    io:format("Call vars: ~p", [ArgVars]),
    CallContext =
        erl_syntax:revert(
            erl_syntax:abstract(#{callsite => Args})),
    VarNames = maps:get(vars, ArgVars, []),

    VariableList =
        lists:foldr(fun(E, Acc) ->
                       {cons, 8, {tuple, 8, [binary_literal(E), fun_returning_variable(E)]}, Acc}
                    end,
                    {nil, 8},
                    VarNames),
    Args2 = Args ++ [CallContext, VariableList],
    Form = {call, Line, {remote, Line2, {atom, Line3, tt}, {atom, Line4, assert}}, Args2},
    {[Form | Forms], FunContext};
transform_fn_body_form({'catch', Line, Form}, {Forms, FunContext}) ->
    {[Inner], _} = transform_fn_body_form(Form, {[], #{}}),
    {[{'catch', Line, Inner} | Forms], FunContext};
transform_fn_body_form({match, Line1, MatchLeftSide, Form} = _F, {Forms, FunContext}) ->
    io:format("Match from ~p~n", [_F]),
    {[Inner], _} = transform_fn_body_form(Form, {[], FunContext}), %
    Match = {match, Line1, MatchLeftSide, Inner},
    io:format("Match result ~p~n", [Match]),
    {[Match | Forms], FunContext};
transform_fn_body_form(Other, {Forms, FunContext}) ->
    io:format("Not transformed: ~p~n", [Other]),
    {[Other | Forms], FunContext}.

extract_assert_call_context([{call,
                              _,
                              {remote, _, {atom, _, _Mod}, {atom, _, _Fun}},
                              CallArgs}]) ->
    lists:foldl(fun(Arg, #{vars := Vars} = _Context) ->
                   Vars2 =
                       case Arg of
                           {var, _, Name} -> [Name | Vars];
                           _ -> Vars
                       end,
                   #{vars => Vars2}
                end,
                #{vars => []},
                CallArgs);
extract_assert_call_context(Statements) when is_list(Statements) ->
    Result = lists:filtermap(fun
        ({var, _, VarName}) -> 
            {true, VarName};
        (_) -> false
    end, Statements),
    #{vars => Result};
extract_assert_call_context(Statement) ->
    io:format("Couldn't extract call context: ~p", [Statement]),
    #{vars => []}.

format_error(E) ->
    case io_lib:deep_char_list(E) of
        true ->
            E;
        _ ->
            io_lib:write(E)
    end.

handle_fail(Result, #{callsite := Callsite} = Context, Variables) ->
    VariableMap = maps:from_list(Variables),
    io:format("Syntax: ~p~n", [Callsite]),
    io:format("Context: ~p~n", [Context]),
    CallStr =
        case callsite_info(Callsite) of
            {fn_call, [Mod, Fun, FnArgs]} ->
                FnArgsStrs = lists:map(fun(Arg) -> call_arg_to_str(Arg, VariableMap) end, FnArgs),
                FnArgsStr = ktn_binary:join(FnArgsStrs, <<", ">>),
                Expression = io_lib:format("Condition failed: ~p:~p(~s)~n", [Mod, Fun, FnArgsStr]),
                lists:flatten(Expression);
            {inline, FnArgs} ->
                FnArgsStrs = lists:map(fun(Arg) -> call_arg_to_str(Arg, VariableMap) end, FnArgs),
                FnArgsStr = ktn_binary:join(FnArgsStrs, <<", ">>),
                Expression = io_lib:format("Condition failed: tt:assert(~s)~n", [FnArgsStr]),
                lists:flatten(Expression);
            [uknown_module, unknown_fn, [unknown_args]] ->
                "uknown_module:unknown_fn(>unknown values<)"
        end,

    erlang:error({assert,
                  [{module, example_test},
                   {line, 8},
                   {expression, CallStr},
                   {expected, true},
                   case not Result of
                       true ->
                           {value, false};
                       _ ->
                           {not_boolean, Result}
                   end]}),
    ok.

% Here we are trying to extract callsite information. This can be basically any Erlang
% expression.
% For now we just assume that the user is calling tt:assert/1 but later this fn may be changed
% to handle more arguments.
callsite_info([{call,
                _Line,
                {remote, _Line2, {atom, _Line3, Mod}, {atom, _Line4, Fun}},
                Args}]) ->
    {fn_call, [Mod, Fun, values_from_fn_args(Args)]};
% The argument is either a variable or a literal
callsite_info([Arg]) when is_tuple(Arg) ->
    case Arg of
        {var, _, VarName} ->
            {inline, [{variable, erlang:atom_to_binary(VarName)}]};
        _ ->
            {inline, [{inline, Arg}]}
    end;
callsite_info(_) ->
    [uknown_module, unknown_fn, [unknown_args]].

%-type fn_arg() :: {variable, VariableName :: binary()} | {call, [M :: atom(), F :: atom(), [fn_arg()]]}.
values_from_fn_args(Args) ->
    io:format("These are the values: ~p", [Args]),
    lists:map(fun(Arg) -> value_from_syntax(Arg) end, Args).

value_from_syntax({var, _Line, VarName}) ->
    {variable, erlang:atom_to_binary(VarName)};
value_from_syntax({call, _, {remote, _, {atom, _, Mod}, {atom, _, Fun}}, _Args}) ->
    {remote_call, {Mod, Fun, []}};
value_from_syntax(ValueSyntax) ->
    {inline, ValueSyntax}.

call_arg_to_str({variable, Name}, Variables) when is_binary(Name) ->
    io:format("Name: ~p Variables: ~p", [Name, Variables]),
    Value =
        case maps:is_key(Name, Variables) of
            true ->
                Fn = maps:get(Name, Variables, undefined),
                tt_format:printable_variable(Fn());
            false ->
                <<"?">>
        end,
    <<"var ", Name/binary, " = ", Value/binary>>;
call_arg_to_str({remote_call, {M, F, _A}}, _Variables) ->
    Str = io_lib:format("~s:~s()", [M, F]),
    erlang:iolist_to_binary(Str);
call_arg_to_str({inline, Syntax}, _Variables) ->
    case erl_syntax:is_proper_list(Syntax) of
        true ->
            Elements = erl_syntax:list_elements(Syntax),
            Str = format_inline_term(Elements),
            erlang:iolist_to_binary(
                io_lib:format("~s", [Str]));
        false ->
            case Syntax of
                {integer, _, Value} ->
                    erlang:iolist_to_binary(
                        io_lib:format("~B", [Value]));
                {atom, _, Value} ->
                    erlang:iolist_to_binary(
                        io_lib:format("~p", [Value]));
                _ ->
                    erlang:iolist_to_binary(
                        io_lib:format("unknown ~p", [Syntax]))
            end
    end;
call_arg_to_str(unknown_args, _) ->
    erlang:iolist_to_binary(
        io_lib:format(">unknown values<", [])).

format_inline_term({integer, _, V}) ->
    erlang:integer_to_binary(V);
format_inline_term(Term) when is_list(Term) ->
    Elems = lists:sublist(Term, ?MAX_LIST_DISPLAY_LEN),
    Elems2 = lists:map(fun format_inline_term/1, Elems),
    Joined = ktn_binary:join(Elems2, <<", ">>),
    <<"[", Joined/binary, "]">>.

fun_returning_variable(Name) when is_atom(Name) ->
    {'fun', 9, {clauses, [{clause, 9, [], [], [{var, 9, Name}]}]}}.

binary_literal(Content) when is_atom(Content) ->
    {bin, 8, [{bin_element, 8, {string, 8, erlang:atom_to_list(Content)}, default, default}]}.
