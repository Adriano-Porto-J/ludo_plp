:- module(findmoves, [
    get_available_moves/2, 
    find_lucky_moves/2,
    filter_safe_moves/3 
    ]).

:- use_module(gametypes).
:- use_module(auxiliary).

% get_available_moves(+GameState, -Moves)
get_available_moves(GameState, AvailableMoves) :-

    GameState = game_state(_, SpecialTiles, _, Blockades, _, DiceRolled, _, _, _, _, _),
    %Pega o player da rodada
    get_player_pieces(GameState, PlayerPieces),

    %Atribui a PiecesInStartingArea as peças do player que estao na area de inicio 
    include(in_starting_area, PlayerPieces, PiecesInStartingArea),

    %Atribui a PiecesOnBoard as peças do player que estao no tabuleiro sem ser na area final  
    include(in_board, PlayerPieces, PiecesInBoard),

    %Atribui a PiecesInFinishArea as peças do player que estao na area final 
    include(in_finish_area_not_finished, PlayerPieces, PiecesInFinishArea),

    %Verifica se o player tirou o dado 6 e se ele tem peças na area inicial, caso tenho é uma possibilidade de jogada
    get_moves_from_start(DiceRolled, PiecesInStartingArea, MovesFromStart),
    
    %Verifica as peças que estão no tabuleiro
    get_moves_on_board(PiecesInBoard, DiceRolled, SpecialTiles, Blockades, MovesOnBoard),
    
    %Verifica as pecas que estao na area final
    get_moves_in_finish_area(PiecesInFinishArea, Blockades, DiceRolled, MovesInFinishArea),

    append([MovesFromStart, MovesOnBoard, MovesInFinishArea], AllMoves),
    filter_safe_moves(GameState, AllMoves, AvailableMoves).

% get_moves_from_start(+DiceRolled, +PiecesInStartingArea, -MovesFromStart)
get_moves_from_start(6, PiecesInStartingArea, MovesFromStart) :-
    findall((Pos, StartPos),
        (
        member(Piece, PiecesInStartingArea),
        piece_position(Piece, Pos),
        piece_color(Piece, Color),
        starting_pos_by_color(Color, StartPos)
        ),
        MovesFromStart).
get_moves_from_start(_,_,[]).

% get_moves_on_board(+Pieces, +DiceRoll, +SpecialTiles, +Blockades, -Moves)
get_moves_on_board([], _, _, _, []).
get_moves_on_board(Pieces, DiceRoll, SpecialTiles, Blockades, Moves) :-
    findall((Start, Final),
        (
            member(Piece, Pieces),
            piece_position(Piece, Start),
            walked_amount(Piece, Walked),
            piece_color(Piece, Color),
            TempEnd is (Start + DiceRoll) mod 48,
            \+ is_blocked(Blockades, (Start, TempEnd)),
            apply_special_tile(SpecialTiles, (Start, TempEnd), (Start, AfterTile)),
            NewWalked is Walked + DiceRoll,
            handle_move_to_finish_area((Start, AfterTile), NewWalked, Color, (Start, Final))
        ),
        Moves
    ).

% get_moves_in_finish_area(+Pieces, +Blockades, +DiceRoll, -Moves)
get_moves_in_finish_area([], _, _, []).
get_moves_in_finish_area([Piece | Rest], Blockades, DiceRoll, Moves) :-
    piece_position(Piece, Start),
    piece_color(Piece, Color),
    finish_area_end(Color, FinishEnd),

    NewPos is Start + DiceRoll,
    get_moves_in_finish_area(Rest, Blockades, DiceRoll, OtherMoves),
    (
        NewPos =< FinishEnd,
        \+ is_blocked(Blockades, (Start, NewPos)) -> !, %Mov valido
        Moves = [(Start, NewPos) | OtherMoves]
        ;Moves = OtherMoves  % Mov inválido
    ).


% handle_move_to_finish_area(+StartEnd, +TotalWalked, +Color, -Result)
handle_move_to_finish_area((Start, End), WalkedTotal, Color, (Start, Final)) :-
    ( WalkedTotal > 48 ->
        finish_area_start(Color, StartFinish),
        Offset is max(0, WalkedTotal - 49),
        Final is StartFinish + Offset
    ; Final = End
    ).

% apply_special_tile(+SpecialTiles, +(Start, End), -(Start, NewEnd))
apply_special_tile(SpecialTiles, (Start, End), (Start, NewEnd)) :-
    ( member(special_tile(Type, PositionTile), SpecialTiles), PositionTile =:= End -> !,
        ( Type = boost   -> NewEnd is (End + 3) mod 48
        ; Type = decline -> NewEnd is max(0, End - 3)
        ; Type = death   -> NewEnd is -1
        ; NewEnd = End )
    ; NewEnd = End
    ).

% find_lucky_moves(+GameState, -VulnerablePositions)
%Verifica as peças vulneraveis
find_lucky_moves(game_state(_, SpecialTiles, Pieces, _, CurrentColor, _, _, _, _, _, _), Positions) :-
    include(\P^(piece_color(P, C), C \= CurrentColor), Pieces, EnemyPieces),
    exclude(in_finish_area_not_finished, EnemyPieces, PiecesNotFinished),
    exclude(in_starting_area, PiecesNotFinished, InGameEnemies),
    findall(Pos,
        (
            member(Piece, InGameEnemies),
            piece_position(Piece, Pos),
            \+ (member(special_tile(safe, Pos), SpecialTiles))
        ),
        Positions).

% filter_safe_moves(+GameState, +Moves, -Filtered)
% Retira de moves, os movimentos que resultam em casas safes ocupadas
filter_safe_moves(game_state(_, SpecialTiles, Pieces, _, _, _, _, _, _, _, _), Moves, Filtered) :-
    findall(M,
        (
            member(M, Moves),
            \+ is_capturing_on_safe_tile(SpecialTiles, Pieces, M)
        ),
        Filtered).
