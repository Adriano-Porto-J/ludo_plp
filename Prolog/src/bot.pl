:- module(bot, [bot_choose_move/2]).

:- use_module(findmoves).
:- use_module(processmove).
:- use_module(auxiliary).
:- use_module(gametypes).
:- use_module(library(pairs)).

% Escolhe a melhor jogada possível para o bot
bot_choose_move(GameState, NewGameState) :-
    GameState = game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, DiceRolled, ProcessingMove, End, Sixes, WasLucky, Winner),
    findmoves: get_available_moves(GameState, AvailableMoves),
    choose_best_move(GameState, AvailableMoves, BestMove),
    (
        BestMove \= none ->
        apply_move(GameState, BestMove, NewGameState)
        ;
        NewGameState = GameState  % Nenhuma jogada válida
    ),
    
    % Condição de parada do gameCycle temporária
    read(A),
    (A == "fim" -> NewGameState = [] ; write("\nPróximo Jogador...")).

% Escolhe a melhor jogada de acordo com prioridade:
% 1. Capturar inimigo
% 2. Finalizar peça
% 3. Avançar mais casas
choose_best_move(_, [], none).
choose_best_move(game_state(A,B,C,D,Color,E,F,G,H,I,J), Moves, BestMove) :-
    GameState = game_state(A,B,C,D,Color,E,F,G,H,I,J), 
    prioritize_captures(GameState, Moves, CaptureMoves),
    ( CaptureMoves = [BestMove|_] -> true
     ; prioritize_finish(Color,Moves, GameState, FinishMoves),
       ( FinishMoves = [BestMove|_] -> true
       ; prioritize_distance(Moves, [BestMove|_])
       )
     ).

% Prioridade 1: Capturar peça inimiga
prioritize_captures(GameState, Moves, Captures) :-
    include(captures_enemy(GameState), Moves, Captures).

captures_enemy(game_state(_, _, Pieces, _, _, _, _, _, _, _, _), (_, To)) :-
     member(piece(_, PieceColor, To, _, _, _, false), Pieces),
     \+ safe_tile(To),  
     PieceColor \= none.  

% prioritize_finish(+Color, +Moves, +GameState, -FinishMoves)
prioritize_finish(Color, Moves, GameState, FinishMoves) :-
    auxiliary: finish_area_start(Color, StartFinish),
    auxiliary: finish_area_end(Color, EndFinish),
    include(is_in_finish_range(StartFinish, EndFinish), Moves, FinishMoves).

% is_in_finish_range(+StartFinish, +EndFinish, +(From, To))
is_in_finish_range(StartFinish, EndFinish, (_, To)) :-
    To >= StartFinish,
    To =< EndFinish.

% Prioridade 3: Maior distância percorrida
prioritize_distance(Moves, SortedMoves) :-
    map_list_to_pairs(move_distance, Moves, Pairs),
    keysort(Pairs, Sorted),
    reverse(Sorted, Descending),
    pairs_values(Descending, SortedMoves).

move_distance((Start, End), Distance) :-
   (End >= Start -> Distance is End - Start ; Distance is 52 - Start + End).

% Aplica o movimento escolhido
apply_move(GameState, (Start, End), NewGameState) :-
    processmove:process_move(GameState, (Start, End), NewGameState).
