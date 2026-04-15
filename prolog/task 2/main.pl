% part 2 (II). 27
% Розбити заданий список на кілька списків, записуючи у перший список значення, які менші за 2^0 , у другий
% – які менші за 2^1 та не потрапили до попереднього списку, у третій – які менші за 2^2 та не потрапили до
% двох попередніх списків, у четвертий – які менші за 2^3 та не потрапили до попередніх списків і т.д.

bucket_index(X, K) :-
    bucket_index(X, 0, K).

bucket_index(X, K, K) :-
    Pow is 2^K,
    X < Pow, !.
bucket_index(X, K, Result) :-
    K1 is K + 1,
    bucket_index(X, K1, Result).

tag_elements([], []).
tag_elements([X|Xs], [(K,X)|Rest]) :-
    bucket_index(X, K),
    tag_elements(Xs, Rest).

sort_by_bucket(Pairs, Sorted) :-
    predsort(compare_bucket, Pairs, Sorted).

compare_bucket(Delta, (K1,X1), (K2,X2)) :-
    compare(DK, K1, K2),
    ( DK = '=' -> compare(Delta, X1, X2) ; Delta = DK ).

group_by_bucket([], []).
group_by_bucket([(K,X)|Rest], [[(K,X)|Group]|Groups]) :-
    take_same_bucket(K, Rest, Group, Remaining),
    group_by_bucket(Remaining, Groups).

take_same_bucket(_, [], [], []).
take_same_bucket(K, [(K,X)|Rest], [(K,X)|Group], Remaining) :-
    take_same_bucket(K, Rest, Group, Remaining).
take_same_bucket(K, [(K2,X)|Rest], [], [(K2,X)|Rest]) :-
    K \= K2.

strip_indices([], []).
strip_indices([Group|Gs], [Values|Rest]) :-
    strip_group(Group, Values),
    strip_indices(Gs, Rest).

strip_group([], []).
strip_group([(_,X)|Xs], [X|Ys]) :-
    strip_group(Xs, Ys).

split_by_powers_of_two(Input, Result) :-
    tag_elements(Input, Tagged),
    sort_by_bucket(Tagged, Sorted),
    group_by_bucket(Sorted, Grouped),
    strip_indices(Grouped, Result).

show_array(List) :-
    write('['),
    write_elements(List),
    write(']').

write_elements([]).
write_elements([X]) :- write(X).
write_elements([H|T]) :-
    write(H), write(', '),
    write_elements(T).

show_2d_array([]) :-
    write('[]').
show_2d_array(List) :-
    write('['),
    write_2d_elements(List),
    write(']').

write_2d_elements([]).
write_2d_elements([X]) :- show_array(X).
write_2d_elements([H|T]) :-
    show_array(H), write(', '),
    write_2d_elements(T).

run_test(N, Input, Expected) :-
    split_by_powers_of_two(Input, Result),
    format("Test ~w:~n", [N]),
    write("  Input:    "), show_array(Input), nl,
    write("  Expected: "), show_2d_array(Expected), nl,
    write("  Result:   "), show_2d_array(Result), nl,
    ( Result = Expected -> write("  PASS\n\n") ; write("  FAIL\n\n") ).

main :-
    run_test(1, [0,1,2,3,4],
        [[0],[1],[2,3],[4]]),

    run_test(2, [5,1,8,2],
        [[1],[2],[5],[8]]),

    run_test(3, [0.5,1.5,3.5,7.5],
        [[0.5],[1.5],[3.5],[7.5]]),

    run_test(4, [10,3,6,1],
        [[1],[3],[6],[10]]).