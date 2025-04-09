:- module(gametypes, []).
% Cores dos jogadores
color(red).
color(green).
color(blue).
color(yellow).
color(black).

% Tipos de casas especiais
tile_type(normal).
tile_type(safe).
tile_type(boost).
tile_type(decline).
tile_type(death).
tile_type(lucky).

% Casa especial no tabuleiro
special_tile(TileType, TilePosition).

% Peça do jogador
piece(Id, Color, Position, Walked, InStart, InFinish, Finished).

% Jogador
player(Color, IsBot, StartingPosition).

% Estado do jogo
game_state(
    Players, SpecialTiles, Pieces, Blockades,
    CurrentPlayer, DiceRolled, ProcessingMove,
    End, SixesInRow, WasLuckyMove, WinnerColor).
