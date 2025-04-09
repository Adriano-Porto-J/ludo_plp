:- module(creategame, []).
% Criação do estado inicial do jogo

% GameState(Players,SpecialTiles,Pieces,?,CurrentPlayer,?,?,?,?,?,State,?,WinnerColor)?
create_game_state(NumPlayers, NumBots, game_state(Players, SpecialTiles, Pieces, [], red, -1, false, false, 0, false, black)) :-
    define_colors(NumPlayers, Colors),
    define_players(Colors, NumPlayers, NumBots, PlayerOrBot),
    maplist(create_player, PlayerOrBot, Players),
    findall(Piece, (member(Color, Colors), create_pieces(Color, PieceList), member(Piece, PieceList)), Pieces),
    create_special_tiles(SpecialTiles).

% Define as cores dos jogadores com base no número de jogadores
define_colors(2, [red, blue]).
define_colors(3, [red, yellow, blue]).
define_colors(4, [red, yellow, blue, green]).


define_players(Colors, NumPlayers, NumBots, PlayerOrBot) :-
    % Número de jogadores humanos
    NumHumans is NumPlayers - NumBots,

    % Divide Colors entre jogadores humanos e bots
    split_players_and_bots(Colors, NumHumans, PlayerColors, BotColors),

    % Cria tuplas indicando se é bot ou não
    maplist(marcar_jogador_humano, PlayerColors, Jogadores),
    maplist(marcar_bot, BotColors, Bots),

    % Junta a lista final de (Color, IsBot)
    append(Jogadores, Bots, PlayerOrBot).

% Separa as N primeiras cores para jogadores humanos, e o resto para bots
split_players_and_bots(Colors, NumPlayers, PlayerColors, BotColors) :-
    length(PlayerColors, NumPlayers),
    append(PlayerColors, BotColors, Colors).

% Marca uma cor como jogador humano (IsBot = false)
marcar_jogador_humano(Color, (Color, false)).

% Marca uma cor como bot (IsBot = true)
marcar_bot(Color, (Color, true)).


% Cria um jogador com base na cor e se é bot
create_player((Color, IsBot), player(Color, IsBot, StartingPos)) :-
    auxiliary:starting_pos_by_color(Color, StartingPos).

% Cria as 4 peças iniciais de um jogador
create_pieces(Color, [
    piece(1, Color, -1, 0, true, false, false),
    piece(2, Color, -2, 0, true, false, false),
    piece(3, Color, -3, 0, true, false, false),
    piece(4, Color, -4, 0, true, false, false)
]).

% Cria a lista de casas especiais
create_special_tiles([
    special_tile(safe, 1),
    special_tile(safe, 7),
    special_tile(decline, 11),
    special_tile(safe, 13),
    special_tile(safe, 25),
    special_tile(lucky, 28),
    special_tile(safe, 33),
    special_tile(safe, 37),
    special_tile(boost, 40),
    special_tile(death, 41),
    special_tile(safe, 46)
]).
