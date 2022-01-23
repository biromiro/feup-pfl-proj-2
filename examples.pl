% for experiments

:- consult('logic.pl').
:- consult('io.pl').

example_initial([
    [empty, empty, empty, empty],
    [empty, empty, empty, empty],
    [empty, empty, empty, empty],
    [empty, empty, empty, empty]
    ]-circle).

example_intermediate([
    [circle, empty, circle, empty],
    [empty, cross, empty, cross],
    [circle, empty, empty, cross],
    [empty, empty, circle, empty]
    ]-cross).

example_final([
    [circle, cross, circle, cross],
    [cross, cross, circle, cross],
    [circle, circle, empty, cross],
    [circle, empty, circle, cross]
    ]-circle). % circle is the winner here, as the only 2 empty squares are surrounded by more than one friendly checker, thus resulting in an illegal move.
                 even if it were cross's turn, circle would still win, as cross has a valid move on 2/2 - one friendly connection.