:- module(auxiliary, [ 
    get_player_pieces/2,
    get_next_player/3,
    get_player_by_color/3,
    get_piece_with_most_tiles_walked/2,
    can_move/1,
    has_color/2,
    piece_position/2,
    piece_color/2,
    starting_pos_by_color/2,
    finish_area_end/2, 
    finish_area_start/2,
    in_starting_area/1,
    in_finish_area_not_finished/1,
    in_board/1,
    is_blocked/3,
    is_capturing_on_safe_tile/3,
    walked_amount/2,
    get_enemies/3,
    get_pieces_locations/2,
    getToFromMove/2,
    write_special_tiles/1,
    is_piece_finished/1
    ]).

% Retorna todas as peças do jogador atual no tabuleiro
get_player_pieces(game_state(_, _, Pieces, _, CurrentColor, _, _, _, _, _, _), PlayerPieces) :-
    include(has_color(CurrentColor), Pieces, PlayerPieces).

has_color(Color, piece(_, Color, _, _, _, _, _)).

% NextColor é a cor próximo jogador pela ordem definida pela lista
get_next_player(Players,CurrentColor,NextColor) :-
    get_player_index(Players,CurrentColor,Index),
    NextIndex is Index + 1,
    length(Players,PlayersLength),
    NextIndexNormalized is mod(NextIndex,PlayersLength),
    nth0(NextIndexNormalized,Players,player(NextColor,_,_)).

get_player_index([player(Color,_,_)|_],Color,Index) :- Index is 0,!.
get_player_index([_|Rest],Color,Index) :- get_player_index(Rest,Color,NextIndex), Index is NextIndex + 1.

% Retorna um jogador específico com base na cor
get_player_by_color([player(Color, IsBot, StartPos) | _], Color, player(Color, IsBot, StartPos)) :- !.
get_player_by_color([_ | Rest], Color, Player) :-
    get_player_by_color(Rest, Color, Player).

% Encontra a peça que andou mais casas, ignorando as que estão na área final ou já finalizaram
get_piece_with_most_tiles_walked(Pieces, PieceWithMost) :-
    include(valid_piece_for_movement, Pieces, ValidPieces),
    ( ValidPieces = [] -> PieceWithMost = none
    ; max_tiles_walked_piece(ValidPieces, PieceWithMost)
    ).


get_enemies([],_,[]).
get_enemies([piece(Id, Color, Position, Walked, InStart, InFinish, Finished)|Rest],PlayerColor,
[piece(Id, Color, Position, Walked, InStart, InFinish, Finished)|Enemies]):-
    Color \== PlayerColor,
    get_enemies(Rest,PlayerColor,Enemies).
get_enemies([piece(Id, Color, Position, Walked, InStart, InFinish, Finished)|Rest],PlayerColor,Enemies):-
    Color == PlayerColor,
    get_enemies(Rest,PlayerColor,Enemies).
   

get_pieces_locations([],[]).
get_pieces_locations([piece(_, _, Position, _, _, _, _)|Rest],[Position|Locations]):-
    Position >= 0,
    get_pieces_locations(Rest,Locations).
get_pieces_locations([piece(_, _, Position, _, _, _, _)|Rest],Locations):-
    Position < 0,
    get_pieces_locations(Rest,Locations).

getToFromMove((_,To),To).

can_move(piece(_, _, _, _, _, false, false)).

walked_amount(piece(_, _, _, Walked, _, _, _), Walked).
    
max_tiles_walked_piece([P], P).
max_tiles_walked_piece([P1, P2 | Rest], MaxPiece) :-
    ( P1.tiles_walked >= P2.tiles_walked -> max_tiles_walked_piece([P1 | Rest], MaxPiece)
    ; max_tiles_walked_piece([P2 | Rest], MaxPiece)
    ).

% Posição inicial por cor
starting_pos_by_color(red, 0).
starting_pos_by_color(yellow, 12).
starting_pos_by_color(blue, 24).
starting_pos_by_color(green, 36).

% Início da área final
finish_area_start(red, 48).
finish_area_start(yellow, 54).
finish_area_start(blue, 60).
finish_area_start(green, 66).

% Fim da área final
finish_area_end(red, 53).
finish_area_end(yellow, 59).
finish_area_end(blue, 65).
finish_area_end(green, 71).

% Verifica se a peça esta na area inicial
in_starting_area(piece(_, _, _, _, true, _, _)).

% Verifica se a peça esta na parte final e nao terminou
in_finish_area_not_finished(piece(_, _, _, _, false, true, false)).

% Verifica se esta no tabuleiro, sem ser na area final e na area inicial.
in_board(piece(_, _, _, _, false, false, false)).

% Verdadeiro se a peça foi finalizada (chegou ao final do percurso).
is_piece_finished(piece(_, _, _, _, _, _, true)).

% Pega a pos da peça
piece_position(piece(_, _, Pos, _, _, _, _), Pos).

% Pega a cor da peça
piece_color(piece(_, Color, _, _, _, _, _), Color).

% Verifica se há um bloqueio entre Start e End feito por outra cor
is_blocked(Blockades, (Start, End), CurrentColor) :-
    member((BlockColor, BlockPos), Blockades),
    BlockColor \= CurrentColor,
    is_between(Start, End, BlockPos),
    !.

is_between(Start, End, Pos) :-
        ( Start < End ->
            Pos > Start,
            Pos < End
        ; Start > End ->
            (Pos > Start ; Pos < End)
        ; false ).
    
% is_capturing_on_safe_tile(+SpecialTiles, +Pieces, +(From, To))
is_capturing_on_safe_tile(SpecialTiles, Pieces, (From, To)) :-
    member(special_tile(safe, To), SpecialTiles),   % To é uma casa segura
    member(piece(_, Color1, From, _, _, _, _), Pieces),
    member(piece(_, Color2, To, _, _, _, _), Pieces),
    Color1 \= Color2. 

write_special_tiles([]).
write_special_tiles([special_tile(Type, Pos) | Rest]) :-
     write("Casa especial do tipo: "), write(Type),
     write(", na posicao: "), write(Pos), write("."), nl,
     write_special_tiles(Rest).
