%%%-------------------------------------------------------------------
%%% @author Paweł
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. lut 2023 22:24
%%%-------------------------------------------------------------------
-module(onp).
-author("Paweł").

%% API
-export([onp/1]).

onp(ONP) -> solve(ONP).

solve(ONP) -> solve(convertTokensToNumbersList(tokenize(ONP)), []).

tokenize(ONP) -> string:tokens(ONP, " ").

convertTokensToNumbersList([H|T]) -> [stringToNumber(H)|convertTokensToNumbersList(T)];
convertTokensToNumbersList([]) -> [].

stringToNumber(X) -> stringToNumber(X, string:to_integer(X), string:to_float(X)).
stringToNumber(Sign, {error, _}, {error, _}) -> Sign;
stringToNumber(_, {Number, _}, {error, _}) -> Number;
stringToNumber(_, _, {Number, _}) -> Number.

solve(["+"| T1], [A,B| T2]) -> solve(T1, [B + A| T2]);
solve(["-"| T1], [A,B| T2]) -> solve(T1, [B - A| T2]);
solve(["*"| T1], [A,B| T2]) -> solve(T1, [B * A| T2]);
solve(["/"| T1], [A,B| T2]) -> solve(T1, [B / A| T2]);
solve(["pow"| T1], [A,B| T2]) -> solve(T1, [math:pow(B, A)| T2]);
solve(["sqrt"| T1], [A| T2]) -> solve(T1, [math:sqrt(A)| T2]);
solve(["sin"| T1], [A| T2]) -> solve(T1, [math:sin(A)| T2]);
solve(["cos"| T1], [A| T2]) -> solve(T1, [math:cos(A)| T2]);
solve(["tan"| T1], [A| T2]) -> solve(T1, [math:tan(A)| T2]);
solve([Number|T], Stack) -> solve(T, [Number|Stack]);
solve([], [Result|_]) -> Result.
