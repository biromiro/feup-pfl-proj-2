encoding(utf8).

% menu I/O

welcome_message :- format('Welcome to Pathway!~nLet\'s begin!~n~n', []).

:- dynamic gamemode/1, size/1.
gamemode(h/h).
size(6).

valid_gamemode(P1/P2) :- valid_player_type(P1), valid_player_type(P2).

valid_player_type(T) :- nonvar(T),
                        T = h.
valid_player_type(T-Val) :- nonvar(T),
                            T = pc,
                            1 is Val; 2 is Val.

next_player(P1, P2) :- gamemode(P1/P2); gamemode(P2/P1).

get_gamemode :- format('Alright, tell me the intended gamemode in the form "P1/P2.", where either can be "h" or "(pc-[1/2])".~n', []),
                read(Gamemode),
                valid_gamemode(Gamemode),
                retract(gamemode(_)),
                assert(gamemode(Gamemode)),
                !.
get_gamemode :- error_message.

get_boardsize :- format('What board size? Recommended: 6 / 8~n', []),
                 read(Size),
                 integer(Size),
                 Size > 0,
                 retract(size(_)),
                 assert(size(Size)),
                 !.
get_boardsize :- error_message.

error_message :- format('Whoops, it seems an error has occurred. Sending to main menu...~n', []).


% board display

side_chars(first, ['\x250F\', '\x2533\', '\x2513\']).
side_chars(last,  ['\x2517\', '\x253B\', '\x251B\']).

checker_char(empty, ' ').
checker_char(cross, 'X').
checker_char(circle, 'O').

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

display_game(Board-_) :- length(Board, Size),
                         display_edge_line(Size, first),
                         display_lines(Board),
                         display_edge_line(Size, last).