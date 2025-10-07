%
% Test AtomVM's round behavior with -2.5
%
-module(test_atomvm_float).

-export([start/0]).

start() ->
    io:format("=== AtomVM Float Rounding Test ===~n"),
    
    % Test the exact same case as roundfloat.erl
    TestValue = -2.5,
    io:format("Testing round(~p)~n", [TestValue]),
    
    try
        Result = round(TestValue),
        io:format("round(~p) = ~p~n", [TestValue, Result]),
        Result
    catch
        error:badarg ->
            io:format("round(~p) threw badarg~n", [TestValue]),
            -1;
        Error:Reason ->
            io:format("round(~p) threw ~p:~p~n", [TestValue, Error, Reason]),
            1
    end,
    
    % Test other problematic values
    TestValues = [-2.5, -1.5, -0.5, 0.5, 1.5, 2.5],
    io:format("~nTesting multiple values:~n"),
    lists:foreach(fun(V) ->
        try
            R = round(V),
            io:format("round(~p) = ~p~n", [V, R])
        catch
            _Error:_Reason ->
                io:format("round(~p) threw error~n", [V])
        end
    end, TestValues),
    
    % Test the exact pattern from roundfloat.erl
    io:format("~nTesting roundfloat.erl pattern:~n"),
    TestResult = to_int(id(id([-2.5, 0]))),
    io:format("to_int(id(id([-2.5, 0]))) = ~p~n", [TestResult]),
    TestResult.

to_int(A) ->
    try round(id(A)) of
        B -> B
    catch
        error:badarg -> -1;
        _:_ -> 1
    end.

id([I | _T]) -> id(I);
id(I) -> I.