:- use_module(library(lists)).

% repeat(+N, ?Elem, ?RList)
repeat(0, _, []).
repeat(N, Elem, [Elem | T]) :- N > 0,
                               N1 is N-1,
                               repeat(N1, Elem, T).

% initial_state(+Size, -GameState)
initial_state(Size, GameState) :- Size > 0,
                                  repeat(Size, empty, Row),
                                  repeat(Size, Row, GameState).

checker(GameState, Row/Col, Checker) :- nth0(Row, GameState, GameRow),
                                        nth0(Col, GameRow, Checker).

connection(blue, blue, friendly).
connection(blue, red, enemy).
connection(red, blue, enemy).
connection(red, red, friendly).

connection(GameState, Pos1, Pos2, Connection) :- checker(GameState, Pos1, Checker1),
                                                 checker(GameState, Pos2, Checker2),
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
noConnections(GameState, Pos, [H | T]) :- \+ connection(GameState, Pos, H, _),
                                            noConnections(GameState, Pos, T).

noFriendlyConnections(GameState, Pos, [H | T]) :- \+ connection(GameState, Pos, H, friendly),
                                                  noFriendlyConnections(GameState, Pos, T).

oneFriendlyConnection(GameState, Pos, [H | T]) :- connection(GameState, Pos, H, friendly),
                                                  noFriendlyConnections(GameState, Pos, T).
oneFriendlyConnection(GameState, Pos, [H | T]) :- connection(GameState, Pos, H, enemy),
                                                  oneFriendlyConnection(GameState, Pos, T).
oneFriendlyConnection(GameState, Pos, [H | T]) :- \+ connection(GameState, Pos, H, _),
                                                  oneFriendlyConnection(GameState, Pos, T).

legalMove(GameState, Pos) :- adjacents(Pos, Adjacents),
                             legalMove(GameState, Pos, Adjacents).
legalMove(GameState, Pos, Adjacents) :- noConnections(GameState, Pos, Adjacents); oneFriendlyConnection(GameState, Pos, Adjacents).

checkerExists(blue).
checkerExists(red).

move(GameState, Row/Col-Checker, NewGameState) :- checkerExists(Checker),
                                                  nth0(Row, GameState, GameStateRow),
                                                  nth0(Row, NewGameState, NewGameStateRow),
                                                  select(empty, GameStateRow, Checker, NewGameStateRow),
                                                  nth0(Col, NewGameStateRow, Checker),
                                                  select(GameStateRow, GameState, NewGameStateRow, NewGameState),
                                                  legalMove(NewGameState, Row/Col).