:- module(save_and_load, [
    save_game/2,
    load_game/2,
    game_state_to_json/2,
    json_to_game_state/2
]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% Registrando os termos como JSON
:- json_object
    piece_json(id:integer, color:atom, position:integer, walked:integer, in_start:boolean, in_finish:boolean, finished:boolean),
    player_json(color:atom, is_bot:boolean, starting_position:integer),
    special_tile_json(type:atom, position:integer),
    blockade_json(color:atom, position:integer),
    game_state_json(
        players:list(player_json),
        special_tiles:list(special_tile_json),
        pieces:list(piece_json),
        blockades:list(blockade_json),
        current_player:atom,
        dice_rolled:integer,
        processing_move:boolean,
        end:boolean,
        sixes_in_row:integer,
        was_lucky_move:boolean,
        winner_color:atom
    ).

% Salvar o estado do jogo em JSON
save_game(File, GameState) :-
    open(File, write, Stream),
    game_state_to_json(GameState, JSON),
    json_write(Stream, JSON),
    close(Stream),
    writeln("Jogo salvo com sucesso.").

% Carregar o estado do jogo de um arquivo JSON
load_game(File, GameState) :-
    open(File, read, Stream),
    catch(
        json_read(Stream, JSON),
        Error,
        (close(Stream), throw(Error))
    ),
    (json_to_game_state(JSON, GameState) ->
        close(Stream), writeln("Jogo carregado com sucesso.")
    ;
        close(Stream),
        throw(invalid_game_state)
    ).

% Conversão: termo Prolog -> JSON
game_state_to_json(
    game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer,
               DiceRolled, ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor),
    game_state_json{
        players: Players,
        special_tiles: SpecialTiles,
        pieces: Pieces,
        blockades: Blockades,
        current_player: CurrentPlayer,
        dice_rolled: DiceRolled,
        processing_move: ProcessingMove,
        end: End,
        sixes_in_row: SixesInRow,
        was_lucky_move: WasLuckyMove,
        winner_color: WinnerColor
    }
).

% Conversão: JSON -> termo Prolog
json_to_game_state(
    game_state_json{
        players: Players,
        special_tiles: SpecialTiles,
        pieces: Pieces,
        blockades: Blockades,
        current_player: CurrentPlayer,
        dice_rolled: DiceRolled,
        processing_move: ProcessingMove,
        end: End,
        sixes_in_row: SixesInRow,
        was_lucky_move: WasLuckyMove,
        winner_color: WinnerColor
    },
    game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer,
               DiceRolled, ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor)
).
