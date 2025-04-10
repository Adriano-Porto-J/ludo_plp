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
%special_tile(TileType, TilePosition).
special_tile(_, _).

% Peça do jogador
%piece(Id, Color, Position, Walked, InStart, InFinish, Finished).
piece(_, _, _, _, _, _, _).

% Jogador
%player(Color, IsBot, StartingPosition).
player(_, _, _).

% Estado do jogo
%game_state(Players, SpecialTiles, Pieces, Blockades,CurrentPlayer, DiceRolled, ProcessingMove,End, SixesInRow, WasLuckyMove, WinnerColor).
game_state(_,_,_,_,_,_,_,_,_,_,_).