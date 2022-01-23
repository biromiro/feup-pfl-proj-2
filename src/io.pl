encoding(utf8).

%% menu I/O

% welcome_message/0
% display welcome message
welcome_message :- format('Welcome to Pathway!~nLet\'s begin!~n~n', []).

:- dynamic gamemode/1, size/1.
gamemode(h/h).                 %default gamemode is human vs human
size(6).                       %default size is 6x6

% valid_gamemode(+Gamemode)
% validate Gamemode input from user.
valid_gamemode(P1/P2) :- ground(P1), ground(P2),
                         valid_player_type(P1), valid_player_type(P2).

% valid_player_type(+T)
% validate player type T input from user.
valid_player_type(h).
valid_player_type(pc-Val) :- 1 is Val; 2 is Val.

% next_player(?P1, ?P2)
% P2 is the next player on the game loop after P1.
next_player(P1, P2) :- gamemode(P1/P2); gamemode(P2/P1).

% get_gamemode/0
% get new gamemode from user and save it.
get_gamemode :- format('Alright, tell me the intended gamemode in the form "P1/P2.", where either can be "h" or "(pc-[1/2])".~n', []),
                read(Gamemode),
                valid_gamemode(Gamemode),
                retract(gamemode(_)),
                assert(gamemode(Gamemode)),
                !.
get_gamemode :- error_message.

% get_boardsize/0
% get new boardsize from user and save it.
get_boardsize :- format('What board size? Recommended: 6 / 8~n', []),
                 read(Size),
                 integer(Size),
                 Size > 0,
                 retract(size(_)),
                 assert(size(Size)),
                 !.
get_boardsize :- error_message.

error_message :- format('Whoops, it seems an error has occurred. Sending to main menu...~n', []).


%% board display

% side_chars(?Line, ?[LeftCorner, Mid, RightCorner])
% for first and last lines, get the characters used to represent them.
side_chars(first, ['\x250F\', '\x2533\', '\x2513\']).
side_chars(last,  ['\x2517\', '\x253B\', '\x251B\']).

% checker_char(?Checker, ?Char)
% for each Checker, get the Char used to represent it.
checker_char(empty, ' ').
checker_char(cross, 'X').
checker_char(circle, 'O').

% display_game(+GameState)
% display a given GameState:
% - top edge line;
% - each line with checkers;
% - bottom edge line.
display_game(Board-_) :- length(Board, Size),
                         display_edge_line(Size, first),
                         display_lines(Board),
                         display_edge_line(Size, last).

% display_edge_line(+Size, +Side)
% display an edge line, either the first or the last.
display_edge_line(Size, Side) :- side_chars(Side, [LeftCorner, Mid, RightCorner]),
                                 display_intermediate_line(Size, LeftCorner, Mid, RightCorner), nl.

% display_lines(+Board)
% display all lines except the edge lines, alternating between the checker boxes and intermediate lines.
display_lines([]).
display_lines([BoardLine | T]) :- write('\x2503\'), display_checker_line(BoardLine), nl, T = [], !.
display_lines([BoardLine | T]) :- length(BoardLine, Size),
                                  display_intermediate_line(Size, '\x2503\', '\x2503\', '\x2503\'), nl,
                                  display_lines(T).

% display_checker_line(+BoardRow)
% display the checker line representation for a given board row.
display_checker_line([]).
display_checker_line([Checker | T]) :- checker_char(Checker, Char),
                                       format(' ~a ', [Char]),
                                       write('\x2503\'),
                                       display_checker_line(T).

% display_intermediate_line(+Size, +LeftEdge, +Mid, +RightEdge)
% display a board line of size Size with the given character to represent the left edge, the mid-line box edges and the right edge.
display_intermediate_line(Size, LeftEdge, Mid, RightEdge) :- write(LeftEdge),
                                                             write('\x2501\\x2501\\x2501\'),
                                                             NewSize is Size-1,
                                                             display_box_edges(NewSize, Mid, '\x2501\\x2501\\x2501\'),
                                                             write(RightEdge).

% display_box_edges(+N, +Mid, +Edge)
% display mid-line box edges.
display_box_edges(0, _, _).
display_box_edges(N, Mid, Edge) :- N > 0,
                                   NewN is N-1,
                                   write(Mid),
                                   write(Edge),
                                   display_box_edges(NewN, Mid, Edge).
