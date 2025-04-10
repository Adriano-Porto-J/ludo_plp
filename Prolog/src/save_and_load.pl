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


game_state_to_serializable(
    game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, DiceRolled,
               ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor),
    json{
        players: PlayersSerializable,
        special_tiles: SpecialTilesSerializable,
        pieces: PiecesSerializable,
        blockades: Blockades,
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
    maplist(piece_to_serializable, Pieces, PiecesSerializable).


player_to_serializable(player(Color, IsBot, StartPos), [Color, IsBot, StartPos]).


special_tile_to_serializable(special_tile(Type, Position), [Type, Position]).


piece_to_serializable(
    piece(Number, Color, Position, Walked, InStart, InFinish, Finished),
    [Number, Color, Position, Walked, InStart, InFinish, Finished]).

serializable_to_game_state(
    json{
        players: PlayersSerializable,
        special_tiles: SpecialTilesSerializable,
        pieces: PiecesSerializable,
        blockades: Blockades,
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
    maplist(serializable_to_piece, PiecesSerializable, Pieces).


serializable_to_player([Color, IsBot, StartPos], player(Color, IsBot, StartPos)).


serializable_to_special_tile([Type, Position], special_tile(Type, Position)).

serializable_to_piece(
    [Number, Color, Position, Walked, InStart, InFinish, Finished],
    piece(Number, Color, Position, Walked, InStart, InFinish, Finished)).
