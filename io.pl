side_chars(first, ['/', 'T', '\\']).
side_chars(last,  ['\\', '1', '/']).

display_box_edges(0, _, _).
display_box_edges(N, Mid, Edge) :- N > 0,
                                   NewN is N-1,
                                   write(Mid),
                                   write(Edge),
                                   display_box_edges(NewN, Mid, Edge).

display_intermediate_line(Size, LeftEdge, Mid, RightEdge) :- write(LeftEdge),
                                                             write('m'),
                                                             NewSize is Size-1,
                                                             display_box_edges(NewSize, Mid, 'm'),
                                                             write(RightEdge).

display_edge_line(Size, Side) :- side_chars(Side, [LeftCorner, Mid, RightCorner]),
                                 display_intermediate_line(Size, LeftCorner, Mid, RightCorner), nl.

display_checker_line([]).
display_checker_line([Checker | T]) :- write(o),
                                       write('|'),
                                       display_checker_line(T).

display_lines([]).
display_lines([GSLine | GSTail]) :- write('|'),
                                    display_checker_line(GSLine), nl,
                                    length(GSLine, Size),
                                    display_intermediate_line(Size, '|', '+', '|'), nl,
                                    display_lines(GSTail).

display_game(GameState) :- [GSH | _] = GameState,
                           length(GSH, Size),
                           display_edge_line(Size, first),
                           display_lines(GameState),
                           display_edge_line(Size, last).