:- module(index, [
    next_player/2,
    check_game_over/1,
    check_game_winner/2,
    is_bot_turn/1
]).

:- use_module(auxiliary).
:- use_module(creategame).
:- use_module(findmoves).
:- use_module(processmove).
:- use_module(gametypes).



% Passa a vez para o próximo jogador
next_player(GameState, NewGameState) :-
    GameState = game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, _, _, End, _, _, _),
    findall(Color, member(player(Color, _, _), Players), Colors),
    get_next_player(Colors, CurrentPlayer, NextPlayer),
    ( check_game_over(GameState) -> Winner = CurrentPlayer ; Winner = black ),
    NewGameState = game_state(Players, SpecialTiles, Pieces, Blockades, NextPlayer, -1, false, End, 0, false, Winner).

% Verifica se o jogo acabou: todos os jogadores devem ter todas as suas peças finalizadas
check_game_over(GameState) :-
    GameState = game_state(Players, _, Pieces, _, _, _, _, _, _, _, _),
    forall(
        member(player(Color, _, _), Players),
        (
            include(==(Color), Pieces, PlayerPieces),
            forall(member(piece(_, Color, _, _, _, _, Finished), PlayerPieces), Finished == true)
        )
    ).

% Verifica quem é o vencedor do jogo: primeiro jogador com todas as peças finalizadas
check_game_winner(GameState, WinnerColor) :-
    GameState = game_state(Players, _, Pieces, _, _, _, _, _, _, _, _),
    findall(Color,
        (
            member(player(Color, _, _), Players),
            findall(piece(Id, Color, Pos, Path, Step, Blocked, true), member(piece(Id, Color, Pos, Path, Step, Blocked, true), Pieces), FinishedPieces),
            length(FinishedPieces, Count),
            Count >= 4
        ),
        Winners),
    ( Winners = [WinnerColor | _] -> true ; WinnerColor = black ).

% Verifica se é a vez de um bot
is_bot_turn(GameState) :-
    GameState = game_state(Players, _, _, _, CurrentPlayer, _, _, _, _, _, _),
    member(player(CurrentPlayer, true, _), Players).
