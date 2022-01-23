:-consult('io.pl').
:-consult('logic.pl').

:- use_module(library(random)).

%% start and menu dispatcher

% play/0
% main predicate - start the application.
play :- welcome_message,
        main_menu.

% main_menu/0
% menu loop - dispatch the various menu commands.
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


%% game loop

% play_game/0
% initialize the game state and display it;
% start the game cycle.
play_game :- size(Size),
             initial_state(Size, GameState),
             display_game(GameState),
             gamemode(StartingPlayerType/_),
             game_cycle(GameState-StartingPlayerType).

% game_cycle(+GameStep)
% take a game step:
% - check for and handle game over
% - choose a move for the current player
% - apply such move
% - switch players
% - display new board
% then take the next one.
game_cycle(GameState-PlayerType) :- game_over(GameState, Winner), !,
                                    congratulate(Winner, PlayerType).
game_cycle(GameState-PlayerType) :- ask_move(GameState, PlayerType, Move),
                                    move(GameState, Move, NewGameState),
                                    next_player(PlayerType, NextPlayerType),
                                    display_game(NewGameState), !,
                                    game_cycle(NewGameState-NextPlayerType).

% ask_move(+GameState, +PlayerType, -Move)
% get player move for current game step, according to player type (human or pc), and level of pc player:
% - if human, ask in a human-friendly way and validate input;
% - if pc, generate moves using valid_moves and then apply choose_move with adequate Level to get the choice.
ask_move(Board-Player, h, Row/Col) :- repeat,
                                         format('~n~w, please choose the coordinates for your play! ("Row/Col.", zero-based, up->down, left->right)~n', [Player]),
                                         read(Row/Col),
                                         nonvar(Row), nonvar(Col),
                                         move(Board-Player, Row/Col, _).
ask_move(GameState, pc-Level, Move) :- choose_move(Level, GameState, Move).

% choose_move(+Level, +GameState, -Move)
% given a GameState, select a Move:
% - one at random if Level=1;
% - the immediately best one according to the value function value/3 if Level=2.
choose_move(1, GameState, Move) :- valid_moves(GameState, Moves),
                                   random_select(Move, Moves, _).
choose_move(2, Board-Player, Move) :- setof(Value-Mv, NewState^(move(Board-Player, Mv, NewState),
                                                                value(NewState, Player, Value) ), [_V-Move|_]).

% congratulate(+Winner, +PlayerType)
% write custom game end message according to the checker and player type of the winner and the loser.
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
