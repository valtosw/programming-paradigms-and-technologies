% part 1 (I-a). 19
% Залишити у списку елементи, що входять у нього по одному разу.

count(_, [], 0).
count(X, [X|T], N) :-
    count(X, T, N1),
    N is N1 + 1.
count(X, [Y|T], N) :-
    X \= Y,
    count(X, T, N).

unique(List, Result) :-
    unique(List, List, Result).

unique([], _, []).
unique([H|T], Full, Result) :-
    count(H, Full, N),       
    unique(T, Full, Rest),
    ( N =:= 1 -> Result = [H|Rest] ; Result = Rest ).

show_array(List) :-
    write('['),
    write_elements(List),
    write(']').

write_elements([]).
write_elements([X]) :- write(X).
write_elements([H|T]) :-
    write(H), write(', '),
    write_elements(T).

run_test(N, Input, Expected) :-
    unique(Input, Result),
    format("Test ~w:~n", [N]),
    write("  Input:    "), show_array(Input), nl,
    write("  Expected: "), show_array(Expected), nl,
    write("  Result:   "), show_array(Result), nl,
    ( Result = Expected -> write("  PASS\n\n") ; write("  FAIL\n\n") ).

main :-
    write("Choose input type:\n"),
    write("1 - Int\n"),
    write("2 - Double\n"),
    write("3 - String\n"),
    read(_Choice),

    write("Enter elements (for example, [1,2,3]):\n"),
    read(Input),

    unique(Input, Result),
    write("Result: "), show_array(Result), nl, nl,

    run_test(1, [1,2,3,2,4,5,1], [3,4,5]),
    run_test(2, [a,b,a,c], [b,c]),
    run_test(3, [1.1,2.2,1.1,3.3], [2.2,3.3]),
    run_test(4, [(1,2),(2,3),(1,2)], [(2,3)]).
