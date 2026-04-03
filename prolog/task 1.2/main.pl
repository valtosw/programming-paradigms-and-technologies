% part 1 (I-b). 57
% Вставити елементи одного списку у впорядкований за спаданням другий список. 
% Сформувати список з номерами позицій, які займають елементи першого списку після вставки.

insert_and_get_positions(List1, List2, Positions) :-
    tag_list(List1, 1, 0, Tagged1),
    length(List1, Len1),
    tag_list(List2, 2, Len1, Tagged2),
    append(Tagged1, Tagged2, Combined),
    predsort(compare_elements, Combined, Sorted),
    find_positions(Sorted, 1, IdxPosPairs),
    keysort(IdxPosPairs, SortedIdxPosPairs),
    extract_values(SortedIdxPosPairs, Positions).

tag_list([], _, _, []).
tag_list([H|T], Type, Idx, [H-Type-Idx|Rest]) :-
    NextIdx is Idx + 1,
    tag_list(T, Type, NextIdx, Rest).

compare_elements(Delta, V1-_T1-I1, V2-_T2-I2) :-
    compare(R, V1, V2),
    ( R == (=) -> compare(Delta, I1, I2) 
    ; R == (<) -> Delta = (>)          
    ; Delta = (<)
    ).

find_positions([], _, []).
find_positions([_-Type-Idx | T], Pos, [Idx-Pos | Rest]) :-
    Type == 1, !,
    NextPos is Pos + 1,
    find_positions(T, NextPos, Rest).
find_positions([_ | T], Pos, Rest) :-
    NextPos is Pos + 1,
    find_positions(T, NextPos, Rest).

extract_values([], []).
extract_values([_-V|T], [V|TR]) :-
    extract_values(T, TR).

show_array(List) :-
    write('['),
    write_elements(List),
    write(']').

write_elements([]).
write_elements([X]) :- write(X).
write_elements([H|T]) :-
    write(H), write(', '),
    write_elements(T).

run_test(N, List1, List2, Expected) :-
    insert_and_get_positions(List1, List2, Result),
    format("Test ~w:~n", [N]),
    write("  List1:    "), show_array(List1), nl,
    write("  List2:    "), show_array(List2), nl,
    write("  Expected: "), show_array(Expected), nl,
    write("  Result:   "), show_array(Result), nl,
    ( Result = Expected -> write("  PASS\n\n") ; write("  FAIL\n\n") ).

main :-
    run_test(1, [3, 1], [4, 2, 5], [3, 5]),
    run_test(2, [1.5, 3.0], [2.5, 0.5], [3, 1]),
    run_test(3, ["banana", "apple"], ["cherry", "date"], [3, 4]),
    run_test(4, [7, 7], [7], [1, 2]).