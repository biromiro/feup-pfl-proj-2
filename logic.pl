:- use_module(library(lists)).

% initial_state(+Size, ?GameState)
% generate a GameState with dimensions SizexSize and circle as starting player checker.
initial_state(Size, Board-circle) :- Size > 0,
                                     repeat(Size, empty, Row),
                                     repeat(Size, Row, Board).

% repeat(+N, ?Elem, ?RList)
% RList is a list composed of N times Elem.
repeat(0, _, []).
repeat(N, Elem, [Elem | T]) :- N > 0,
                               N1 is N-1,
                               repeat(N1, Elem, T).

% move(+GameState, ?Pos, -NewGameState) or move(+GameState, -Pos, +NewGameState) or move(-GameState, +Pos, +NewGameState)
% by placing the adequate checker at Pos, GameState will turn into NewGameState. checks if:
% - the players are adequately swapped;
% - the spot was previously empty;
% - the only difference between the two boards is at that spot;
% - the spot now has the right checker;
% - it was a legal move.

move(Board-Player, Row/Col, NewBoard-NewPlayer) :- swap_player(Player, NewPlayer),                 %valid player and new player
                                                   checker(Board, Row/Col, empty),                 %previously empty
                                                   nth0(Row, Board, BoardRow),
                                                   nth0(Row, NewBoard, NewBoardRow),
                                                   select(BoardRow, Board, NewBoardRow, NewBoard), %boards only diverge on this row
                                                   nth0(Col, NewBoardRow, Player),                 %Row/Col of the new board has the new checker        
                                                   select(empty, BoardRow, Player, NewBoardRow),   %on this row, boards only diverge on empty->player_checker
                                                   legal_move(NewBoard, Row/Col).                  %connections are ok

% swap_player(?P1, ?P2)
swap_player(cross, circle).
swap_player(circle, cross).

% checker(+Board, ?Pos, ?Checker) or checker(?Board, +Pos, ?Checker) or checker(?Board, ?Pos, +Checker)
% checker Checker is at position Pos on Board.
checker(Board, Row/Col, Checker) :- nth0(Row, Board, GameRow),
                                    nth0(Col, GameRow, Checker).

% legal_move(+Board, +Pos)
% true if the checker placed at Pos is legal:
% - has no connections at all
%   OR
% - has one and only one friendly connection.
legal_move(Board, Pos) :- adjacents(Pos, Adjacents),
                          legal_move(Board, Pos, Adjacents).
legal_move(Board, Pos, Adjacents) :- no_connections(Board, Pos, Adjacents);
                                     one_friendly_connection(Board, Pos, Adjacents).

% adjacents(+Pos, -Adjacents)
% generate orthogonal Adjacents to position Pos.
adjacents(Pos, Adjacents) :- findall(Adj, adjacent(Pos, Adj), Adjacents).

% adjacent(+Pos, ?Adjacent)
% Adjacent is orthogonally adjacent to Pos.
adjacent(Row/Col, Row/AdjCol) :- AdjCol is Col-1.
adjacent(Row/Col, AdjRow/Col) :- AdjRow is Row+1.
adjacent(Row/Col, Row/AdjCol) :- AdjCol is Col+1.
adjacent(Row/Col, AdjRow/Col) :- AdjRow is Row-1.

% no_connections(+Board, +Pos, +Adjacents)
% there are no connections between Pos and Adjacents on the Board.
no_connections(_, _, []).
no_connections(Board, Pos, [H | T]) :- \+ connection(Board, Pos, H, _),
                                       no_connections(Board, Pos, T).

% one_friendly_connection(+Board, +Pos, +Adjacents)
% there is one and only one friendly connection between Pos and Adjacents on the Board.
one_friendly_connection(Board, Pos, [H | T]) :- connection(Board, Pos, H, friendly),
                                                no_friendly_connections(Board, Pos, T).
one_friendly_connection(Board, Pos, [H | T]) :- connection(Board, Pos, H, enemy),
                                                one_friendly_connection(Board, Pos, T).
one_friendly_connection(Board, Pos, [H | T]) :- \+ connection(Board, Pos, H, _),
                                                one_friendly_connection(Board, Pos, T).

% no_friendly_connections(+Board, +Pos, +Adjacents)
% there are no friendly connections between Pos and Adjacents on the Board.
no_friendly_connections(_, _, []).
no_friendly_connections(Board, Pos, [H | T]) :- \+ connection(Board, Pos, H, friendly),
                                                no_friendly_connections(Board, Pos, T).

% connection(+Board, +Pos1, +Pos2, +Adjacents)
% there is a connection of type Connection between Pos1 and Pos2 on the Board.
connection(Board, Pos1, Pos2, Connection) :- checker(Board, Pos1, Checker1),
                                             checker(Board, Pos2, Checker2),
                                             connection(Checker1, Checker2, Connection).

% connection(?Checker1, ?Checker2, ?Connection)
% Checker1 and Checker2 form a Connection.
connection(circle, circle, friendly).
connection(circle, cross, enemy).
connection(cross, circle, enemy).
connection(cross, cross, friendly).

% valid_moves(+GameState, -Moves)
% generate all valid moves for a given game state.
valid_moves(GameState, Moves) :- findall(Move, move(GameState, Move, _NewState), Moves).

% value(+GameState, +Player, -Value)
% derive an integer Value from a given GameState for a Player - the smaller the better.
% since the goal is to run out of valid moves, the Value is calculated as the difference between the Player's and the opponent's number of moves.
value(Board-_, Player, Value) :- valid_moves(Board-Player, MyMoves),
                                 swap_player(Player, OtherPlayer),
                                 valid_moves(Board-OtherPlayer, TheirMoves),
                                 length(MyMoves, MyValue),
                                 length(TheirMoves, TheirValue),
                                 Value is MyValue-TheirValue.

% game_over(+GameState, ?Winner).
% calculate the Winner for a given GameState
game_over(Board-cross, cross) :- \+ move(Board-cross, _, _).
game_over(Board-circle, circle) :- \+ move(Board-circle, _, _).
