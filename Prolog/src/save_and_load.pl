:- module(save_and_load, [save_game/2, load_game/2]).

:- use_module(library(http/json)).
:- use_module(library(lists)).

save_game(FileName, GameState) :-
    game_state_to_serializable(GameState, Serializable),
    setup_call_cleanup(
        open(FileName, write, Stream, [encoding(utf8)]),
        json_write(Stream, Serializable),
        close(Stream)
    ),
    format('Jogo salvo com sucesso em ~w~n', [FileName]).

load_game(FileName, GameState) :-
    setup_call_cleanup(
        open(FileName, read, Stream, [encoding(utf8)]),
        json_read(Stream, Serializable),
        close(Stream)
    ),
    serializable_to_game_state(Serializable, GameState),
    format('Jogo carregado com sucesso de ~w~n', [FileName]).

% Converte o estado de jogo em um termo serializável para JSON
game_state_to_serializable(
    game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, DiceRolled,
               ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor),
    json{
        players: PlayersSerializable,
        special_tiles: SpecialTilesSerializable,
        pieces: PiecesSerializable,
        blockades: BlockadesSerializable,
        current_player: CurrentPlayer,
        dice_rolled: DiceRolled,
        processing_move: ProcessingMove,
        end: End,
        sixes_in_row: SixesInRow,
        was_lucky_move: WasLuckyMove,
        winner_color: WinnerColor
    }) :-
    maplist(player_to_serializable, Players, PlayersSerializable),
    maplist(special_tile_to_serializable, SpecialTiles, SpecialTilesSerializable),
    maplist(piece_to_serializable, Pieces, PiecesSerializable),
    convert_blockades(Blockades, BlockadesSerializable).

% Serialização individual dos elementos
player_to_serializable(player(Color, IsBot, StartPos), [Color, IsBot, StartPos]).
special_tile_to_serializable(special_tile(Type, Position), [Type, Position]).
piece_to_serializable(
    piece(Number, Color, Position, Walked, InStart, InFinish, Finished),
    [Number, Color, Position, Walked, InStart, InFinish, Finished]).

% Converte blockades (lista de tuplas) para lista de dicts JSON
convert_blockades([], []).
convert_blockades([(Color, Tile)|Rest], [json{color: Color, tile: Tile}|ConvertedRest]) :-
    convert_blockades(Rest, ConvertedRest).

% Reverso: transforma JSON lido em GameState
serializable_to_game_state(
    json{
        players: PlayersSerializable,
        special_tiles: SpecialTilesSerializable,
        pieces: PiecesSerializable,
        blockades: BlockadesSerializable,
        current_player: CurrentPlayer,
        dice_rolled: DiceRolled,
        processing_move: ProcessingMove,
        end: End,
        sixes_in_row: SixesInRow,
        was_lucky_move: WasLuckyMove,
        winner_color: WinnerColor
    },
    game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, DiceRolled,
               ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor)) :-
    maplist(serializable_to_player, PlayersSerializable, Players),
    maplist(serializable_to_special_tile, SpecialTilesSerializable, SpecialTiles),
    maplist(serializable_to_piece, PiecesSerializable, Pieces),
    convert_blockades_back(BlockadesSerializable, Blockades).

serializable_to_player([Color, IsBot, StartPos], player(Color, IsBot, StartPos)).
serializable_to_special_tile([Type, Position], special_tile(Type, Position)).
serializable_to_piece(
    [Number, Color, Position, Walked, InStart, InFinish, Finished],
    piece(Number, Color, Position, Walked, InStart, InFinish, Finished)).

% Converte lista de dicts JSON em lista de tuplas para o Prolog
convert_blockades_back([], []).
convert_blockades_back([json{color: Color, tile: Tile}|Rest], [(Color, Tile)|ConvertedRest]) :-
    convert_blockades_back(Rest, ConvertedRest).
