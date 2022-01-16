:-consult('io.pl').
:-consult('logic.pl').

:- use_module(library(random)).

% start and menu dispatcher

play :- welcome_message,
        main_menu.

main_menu :- repeat,
             format('What would you like to do? "gamemode.", "boardsize.", "game." or "leave."?~n', []),
             read(Selection),
             (
                 Selection = leave;
                 main_menu(Selection), fail
             ).

main_menu(gamemode) :- get_gamemode.
main_menu(boardsize) :- get_boardsize.
main_menu(game) :- play_game.


% game cycle

play_game :- size(Size),
             initial_state(Size, GameState),
             display_game(GameState),
             gamemode(StartingPlayerType/_),
             game_cycle(GameState-StartingPlayerType).

game_cycle(GameState-PlayerType) :- game_over(GameState, Winner), !,
                                    congratulate(Winner, PlayerType).
game_cycle(GameState-PlayerType) :- choose_move(GameState, PlayerType, Move),
                                    move(GameState, Move, NewGameState),
                                    next_player(PlayerType, NextPlayerType),
                                    display_game(NewGameState), !,
                                    game_cycle(NewGameState-NextPlayerType).

choose_move(Board-Player, h, Row/Col) :- repeat,
                                         format('~n~w, please choose the coordinates for your play! ("Row/Col.", zero-based, up->down, left->right)~n', [Player]),
                                         read(Row/Col),
                                         nonvar(Row), nonvar(Col),
                                         move(Board-Player, Row/Col, _).
                                          
choose_move(GameState, pc-Level, Move):- valid_moves(GameState, Moves),
                                         choose_move(Level, GameState, Moves, Move).

choose_move(1, _GameState, Moves, Move) :- random_select(Move, Moves, _).
choose_move(2, Board-Player, Moves, Move) :- setof(Value-Mv, NewState^( member(Mv, Moves),
                                                                     move(Board-Player, Mv, NewState),
                                                                     value(NewState, Player, Value) ), [_V-Move|_]).

congratulate(Winner, h) :- next_player(h, pc-_),
                           format('Congratulations, ~w! You have beat the machine :)~n~n', [Winner]).
congratulate(Winner, h) :- next_player(h, h),
                           swap_player(Winner, Loser),
                           format('Congratulations to ~w! ~w, you\'ll get them next time :)~n~n', [Winner, Loser]).
congratulate(Winner, pc-Level) :- next_player(pc-Level, h),
                                  swap_player(Winner, Loser),
                                  format('The industrial revolution and its consequences have been a disaster for the human race. I\'m sorry, ~w.~n~n', [Loser]).
congratulate(Winner, pc-Level) :- next_player(pc-Level, pc-_),
                                  swap_player(Winner, Loser),
                                  format('In a battle of pure steel, ~w came out on top. ~w will come for revenge...~n~n', [Winner, Loser]), !.
