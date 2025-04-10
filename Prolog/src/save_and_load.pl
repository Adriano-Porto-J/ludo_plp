:- module(save_and_load, [save_game/2, load_game/2]).

:- use_module(library(http/json)).
:- use_module(library(lists)).

% -----------------------
% SALVAR O JOGO
% -----------------------

save_game(FileName, GameState) :-
    game_state_to_serializable(GameState, Serializable),
    setup_call_cleanup(
        open(FileName, write, Stream, [encoding(utf8)]),
        json_write(Stream, Serializable),
        close(Stream)
    ),
    format('Jogo salvo com sucesso em ~w~n', [FileName]).

% -----------------------
% CARREGAR O JOGO
% -----------------------

load_game(FileName, GameState) :-
    setup_call_cleanup(
        open(FileName, read, Stream, [encoding(utf8)]),
        json_read(Stream, Serializable),
        close(Stream)
    ),
    format('Conteúdo lido do JSON:\n~w\n', [Serializable]),
    serializable_to_game_state(Serializable, GameState),
    format('Jogo carregado com sucesso de ~w~n', [FileName]).

% -----------------------
% SERIALIZAÇÃO PARA JSON
% -----------------------

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
piece_to_serializable(piece(Number, Color, Position, Walked, InStart, InFinish, Finished),
                      [Number, Color, Position, Walked, InStart, InFinish, Finished]).

% -----------------------
% DESSERIALIZAÇÃO DO JSON
% -----------------------

serializable_to_game_state(Dict, GameState) :-
    (Dict = json(List) -> dict_create(RealDict, _, List) ; RealDict = Dict),
    get_dict(players, RealDict, PlayersSerializable),
    get_dict(special_tiles, RealDict, SpecialTilesSerializable),
    get_dict(pieces, RealDict, PiecesSerializable),
    get_dict(blockades, RealDict, Blockades),
    get_dict(current_player, RealDict, CurrentPlayer),
    get_dict(dice_rolled, RealDict, DiceRolled),
    get_dict(processing_move, RealDict, ProcessingMove),
    get_dict(end, RealDict, End),
    get_dict(sixes_in_row, RealDict, SixesInRow),
    get_dict(was_lucky_move, RealDict, WasLuckyMove),
    get_dict(winner_color, RealDict, WinnerColor),
    maplist(serializable_to_player, PlayersSerializable, Players),
    maplist(serializable_to_special_tile, SpecialTilesSerializable, SpecialTiles),
    maplist(serializable_to_piece, PiecesSerializable, Pieces),
    GameState = game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, DiceRolled,
                           ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor).

serializable_to_player([Color, IsBot, StartPos], player(Color, IsBot, StartPos)).
serializable_to_special_tile([Type, Position], special_tile(Type, Position)).
serializable_to_piece([Number, Color, Position, Walked, InStart, InFinish, Finished],
                      piece(Number, Color, Position, Walked, InStart, InFinish, Finished)).
