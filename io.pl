encoding(utf8).

:- multifile
    user:message_property/2.

user:message_property(warning, color(fg(blue))).

test :-
 print_message(warning,'here').


side_chars(first, ['\x250F\', '\x2533\', '\x2513\']).
side_chars(last,  ['\x2517\', '\x253B\', '\x251B\']).

checker_char(empty, ' ').
checker_char(red, 'X').
checker_char(blue, 'O').

display_box_edges(0, _, _).
display_box_edges(N, Mid, Edge) :- N > 0,
                                   NewN is N-1,
                                   write(Mid),
                                   write(Edge),
                                   display_box_edges(NewN, Mid, Edge).

display_intermediate_line(Size, LeftEdge, Mid, RightEdge) :- write(LeftEdge),
                                                             write('\x2501\\x2501\\x2501\'),
                                                             NewSize is Size-1,
                                                             display_box_edges(NewSize, Mid, '\x2501\\x2501\\x2501\'),
                                                             write(RightEdge).

display_edge_line(Size, Side) :- side_chars(Side, [LeftCorner, Mid, RightCorner]),
                                 display_intermediate_line(Size, LeftCorner, Mid, RightCorner), nl.

display_checker_line([]).
display_checker_line([Checker | T]) :- checker_char(Checker, Char),
                                       format(' ~a ', [Char]),
                                       write('\x2503\'),
                                       display_checker_line(T).

display_lines([]).
display_lines([GSLine | GSTail]) :- write('\x2503\'), display_checker_line(GSLine), nl, GSTail = [], !.
display_lines([GSLine | GSTail]) :- length(GSLine, Size),
                                    display_intermediate_line(Size, '\x2503\', '\x2503\', '\x2503\'), nl,
                                    display_lines(GSTail).

display_game(GameState) :- [GSH | _] = GameState,
                           length(GSH, Size),
                           display_edge_line(Size, first),
                           display_lines(GameState),
                           display_edge_line(Size, last).