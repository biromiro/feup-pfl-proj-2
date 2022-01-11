:- use_module(library(lists)).

% repeat(+N, ?Elem, ?RList)
repeat(0, _, []).
repeat(N, Elem, [Elem | T]) :- N > 0,
                               N1 is N-1,
                               repeat(N1, Elem, T).

% initial_state(+Size, -GameState)
initial_state(Size, Board-circle) :- Size > 0,
                                     repeat(Size, empty, Row),
                                     repeat(Size, Row, Board).

checker(Board, Row/Col, Checker) :- nth0(Row, Board, GameRow),
                                    nth0(Col, GameRow, Checker).

connection(circle, circle, friendly).
connection(circle, cross, enemy).
connection(cross, circle, enemy).
connection(cross, cross, friendly).

connection(Board, Pos1, Pos2, Connection) :- checker(Board, Pos1, Checker1),
                                             checker(Board, Pos2, Checker2),
                                             connection(Checker1, Checker2, Connection).

adjacent(Row/Col, left, Row/AdjCol) :- AdjCol is Col-1.
adjacent(Row/Col, up, AdjRow/Col) :- AdjRow is Row+1.
adjacent(Row/Col, right, Row/AdjCol) :- AdjCol is Col+1.
adjacent(Row/Col, down, AdjRow/Col) :- AdjRow is Row-1.

genAdjacents(_Pos, [], Adjacents, Adjacents).
genAdjacents(Pos, [Direction | DirectionT], Aggregate, Adjacents) :- adjacent(Pos, Direction, Adj),
                                                                     genAdjacents(Pos, DirectionT, [Adj | Aggregate], Adjacents).

adjacents(Pos, Adjacents) :- genAdjacents(Pos, [left, up, right, down], [], Adjacents).

noConnections(_, _, []).
noConnections(Board, Pos, [H | T]) :- \+ connection(Board, Pos, H, _),
                                      noConnections(Board, Pos, T).

noFriendlyConnections(_, _, []).

noFriendlyConnections(Board, Pos, [H | T]) :- \+ connection(Board, Pos, H, friendly),
                                              noFriendlyConnections(Board, Pos, T).

oneFriendlyConnection(Board, Pos, [H | T]) :- connection(Board, Pos, H, friendly),
                                              noFriendlyConnections(Board, Pos, T).
oneFriendlyConnection(Board, Pos, [H | T]) :- connection(Board, Pos, H, enemy),
                                              oneFriendlyConnection(Board, Pos, T).
oneFriendlyConnection(Board, Pos, [H | T]) :- \+ connection(Board, Pos, H, _),
                                              oneFriendlyConnection(Board, Pos, T).

legalMove(Board, Pos, Adjacents) :- noConnections(Board, Pos, Adjacents);
                                    oneFriendlyConnection(Board, Pos, Adjacents).
legalMove(Board, Pos) :- adjacents(Pos, Adjacents),
                         legalMove(Board, Pos, Adjacents).

swap_player(cross, circle).
swap_player(circle, cross).

move(Board-Player, Row/Col, NewBoard-NewPlayer) :- swap_player(Player, NewPlayer),                 %valid player and new player
                                                   checker(Board, Row/Col, empty),                 %previously empty
                                                   nth0(Row, Board, BoardRow),
                                                   nth0(Row, NewBoard, NewBoardRow),
                                                   select(BoardRow, Board, NewBoardRow, NewBoard), %boards only diverge on this row
                                                   nth0(Col, NewBoardRow, Player),                 %Row/Col of the new board has the new checker        
                                                   select(empty, BoardRow, Player, NewBoardRow),   %on this row, boards only diverge on empty->player_checker
                                                   legalMove(NewBoard, Row/Col).                   %connections are ok

% gameOver(+GameState, -Winner).

gameOver(Board-cross, cross) :- \+ move(Board-cross, _, _).
gameOver(Board-circle, circle) :- \+ move(Board-circle, _, _).