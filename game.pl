:-consult('io.pl').
:-consult('logic.pl').

:- dynamic gamemode/1, difficulty/2, size/1.


gamemode_input('H').
gamemode_input('PC').

get_gamemode :- format('Ok fofo manda-me o gamemode da forma "P1/P2." ta bem?~n', []),
                read(P1/P2),
                gamemode_input(P1),
                gamemode_input(P2),
                assertz(gamemode(P1/P2)),
                retract(gamemode(_)).
get_gamemode :- format('Oh meu ganda burro eu pedi com gentileza, vais ao menu principal e tentas outra vez que ja n te aturo.~n', []).

welcome_message :- format('Bem vindo, gamer no. 21!~nfull L9 no respect~n', []).

main_menu(gamemode) :- get_gamemode.
main_menu(difficulty) :- get_difficulty.

main_menu :- repeat,
             format('Ok bro que queres fazer? "gamemode", "difficulty", "boardsize", "play" ou "leave"?~n', []),
             read(Selection),
             (
                 Selection = leave;
                 main_menu(Selection), fail
             ).

play :- welcome_message,
        main_menu.
