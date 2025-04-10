:- module(processmove, [process_move/3,process_lucky_move/3]).

:- use_module(gametypes).
:- use_module(auxiliary).
:- use_module(findmoves).

% process_move(+GameState, +(From, To), -NewGameState)
process_move(game_state(Players, SpecialTiles, Pieces, _, CurrentColor, D, _, End, SixesInRow, WasLuckyMove, WinnerColor), 
             (From, To),
             NewGameState) :-

    include(has_color(CurrentColor), Pieces, PlayerPieces),
    member(Piece, PlayerPieces),
    auxiliary:piece_position(Piece, From), % Impede que ele continue buscando por uma peça após encontrar a peça correta.
    finish_area_start(CurrentColor, FinalStart),
    finish_area_end(CurrentColor, FinalEnd),
    % Impede entrada em posições bloqueadas
    find_blockades(Pieces, Blockades),
    ( member((_, To), Blockades) ->
        % Não pode mover para uma posição bloqueada
        write("\nlVoce moveu para uma casa com um blockade, logo voce perdeu a rodada\nl"),
        NewGameState = game_state(Players, SpecialTiles, Pieces, Blockades, CurrentColor, D, false, End, SixesInRow, WasLuckyMove, WinnerColor)
    ;
        (in_starting_area(Piece) ->
            move_piece_to_board(Piece, Pieces, CurrentColor, NewPieces)
        ; To =:= -1 ->
            move_piece_captured(Piece, Pieces, NewPieces)
        ; To =:= FinalEnd ->
            piece_finish(Piece, Pieces, To, NewPieces)
        ; (To >= FinalStart, From < FinalStart) ->
            get_player_by_color(Players, CurrentColor, Player),
            move_piece_to_finish_area(Piece, Pieces, Player, From, To, NewPieces)
        ; (From >= FinalStart, To < FinalEnd) ->
            move_piece_in_finish_area(Piece, Pieces, From, To, NewPieces)
        ;
            move_piece_in_board(Piece, Pieces, From, To, CurrentColor, NewPieces)
        ),
        handle_capture(To, CurrentColor, NewPieces, FinalPieces),
        find_blockades(FinalPieces, NewBlockades),

        ( check_winner(FinalPieces, Winner) ->
            write("\nO jogador de cor "), write(Winner), write(" ganhou a partida!!"),nl,
            FinalWinnerColor = Winner
        ; FinalWinnerColor = WinnerColor 
        ),
        NewGameState = game_state(Players, SpecialTiles, FinalPieces, NewBlockades, CurrentColor, D, false, End, SixesInRow, WasLuckyMove, FinalWinnerColor)
    ),
    !.


find_blockades(Pieces, Blockades) :-
    findall((Color, Pos),
        (
            member(piece(_, Color, Pos, _, _, _, _), Pieces),
            finish_area_end(Color, End),
            Pos >= 0, Pos< End,
            findall(C, (member(piece(_, C, Pos, _, _, _, _), Pieces), C == Color), Colors),
            length(Colors, Count),
            Count >= 2
        ),
    RawBlockades),
    list_to_set(RawBlockades, Blockades).

update_piece_position(piece(ID, Color, _, _, _, _, _), NewPos, NewWalked, InStart, InFinish, Finished, Pieces, [Updated | Rest]) :-
    Updated = piece(ID, Color, NewPos, NewWalked, InStart, InFinish, Finished),
    exclude(is_same_piece(ID, Color), Pieces, Rest).

is_same_piece(ID, Color, piece(ID, Color, _, _, _, _, _)).

% Movimentos

move_piece_to_board(Piece, Pieces, Color, NewPieces) :-
    starting_pos_by_color(Color, StartPos),
    update_piece_position(Piece, StartPos, 0, false, false, false, Pieces, NewPieces).

move_piece_captured(Piece, Pieces, NewPieces) :-
    update_piece_position(Piece, -1, 0, true, false, false, Pieces, NewPieces).

move_piece_to_finish_area(piece(ID, Color, _, Walked, _, _, _), Pieces, _, From, To, NewPieces) :-
    auxiliary:finish_area_start(Color, FinishStart),
    DistanceToFinish is (FinishStart - From + 48) mod 48,
    StepsInFinishArea is To - FinishStart,
    Steps is DistanceToFinish + StepsInFinishArea,
    NewWalked is Walked + Steps,
    update_piece_position(piece(ID, Color, _, _, _, _, _), To, NewWalked, false, true, false, Pieces, NewPieces).


% move_piece_in_board(+Piece, +Pieces, +From, +To, +Color, -NewPieces)
move_piece_in_board(piece(ID, Color, _, Walked, _, _, _), Pieces, From, To, _, NewPieces) :-
    Distance is (To - From + 48) mod 48,
    NewWalked is Walked + Distance,
    update_piece_position(piece(ID, Color, _, _, _, _, _), To, NewWalked, false, false, false, Pieces, NewPieces).

% move_piece_in_finish_area(+Piece, +Pieces, +From, +To, -NewPieces)
move_piece_in_finish_area(piece(ID, Color, _, Walked, _, _, _), Pieces, From, To, NewPieces) :-
    Steps is To - From,
    NewWalked is Walked + Steps,
    update_piece_position(piece(ID, Color, _, _, _, _, _), To, NewWalked, false, true, false, Pieces, NewPieces).

% piece_finish(+Piece, +Pieces, +To, -NewPieces)
piece_finish(piece(Id, Color, _, _, IsBlocked, InFinish, _), Pieces, To, NewPieces) :-
    % Criar a peça atualizada com finished = true
    NewPiece = piece(Id, Color, To, 0, IsBlocked, InFinish, true),
    
    % Substituir a peça na lista
    select(piece(Id, Color, _, _, IsBlocked, InFinish, _), Pieces, NewPiece, NewPieces).

% process_lucky_move(+GameState, +PieceToKillPos, -NewGameState)
process_lucky_move(game_state(Players, SpecialTiles, Pieces, _, CurrentColor, _, _, End, SixesInRow, _, WinnerColor), PieceToKillPos, NewGameState) :-
    member(Piece, Pieces),
    piece_position(Piece, PieceToKillPos),
    Piece = piece(ID, _, _, _, _, _, _),
    newPos = -1 * ID,
    update_piece_position(Piece,newPos , 0, true, false, false, Pieces, NewPieces),
    find_blockades(NewPieces, NewBlockades),
    NewGameState = game_state(Players, SpecialTiles, NewPieces, NewBlockades, CurrentColor, false, false, End, SixesInRow, false, WinnerColor).

% handle_capture(+To, +CurrentColor, +PiecesIn, -PiecesOut, +SpecialTiles)
 handle_capture(Pos, CurrentColor, PiecesIn, PiecesOut) :-
     % Tenta encontrar uma peça inimiga na posição Pos
     (
         member(P, PiecesIn),
         auxiliary:piece_position(P, Pos),
         piece_color(P, Color),
         Color \= CurrentColor
     ->
         % Captura: move a peça inimiga para a base
         write("Jogador "), write(CurrentColor), write(" capturou uma peca do jogador "), write(Color), write(" na casa "), write(Pos), nl,
         move_piece_captured(P, PiecesIn, PiecesOut)
     ;
         % Sem captura: PiecesOut = PiecesIn
         PiecesOut = PiecesIn
     ).
 
 handle_capture(_, _, Pieces, Pieces).

% check_winner(+Pieces, -WinnerColor)
% Verifica se algum jogador venceu (tem 4 peças finalizadas).
check_winner(Pieces, WinnerColor) :-
    % Agrupa peças por cor
    findall(Color, member(piece(_, Color, _, _, _, _, _), Pieces), ColorsDup),
    sort(ColorsDup, Colors),
    member(Color, Colors),
    include(auxiliary:has_color(Color), Pieces, PlayerPieces),
    include(auxiliary:is_piece_finished, PlayerPieces, FinishedPieces),
    length(FinishedPieces, 4),
    WinnerColor = Color.
