:- module(main, []).

rollDice(D):-
    % Intervalo [1,7), 1 <= D < 7
    random(1,7,D),
    write("\nVoce Rolou: "),
    write(D).

rollDiceBot(D):-
    % Intervalo [1,7), 1 <= D < 7
    random(1,7,D).

rollDiceDebug(D) :- 
    nl, write("Funcao rollDiceDebug executada, digite o valor do dado: "),
    read(D), nl.

infoPiece(piece(Num,Color,Pos,Walked, InStart, InFinish, Finished)):-
    term_string(Num,Ntxt),
    term_string(Color,Ctxt),
    term_string(Pos,Ptxt),
    term_string(Walked,Wtxt),
    term_string(InStart,Stxt),
    term_string(InFinish,IFtxt),
    term_string(Finished,Ftxt),
    atomics_to_string(["\n","Peca",Ctxt,Ntxt,"|","Pos:",Ptxt,"| Casas andadas:", Wtxt, "| Na base: ",Stxt,
    "| Reta Final:",IFtxt,"| Finalizada:",Ftxt]," ",R),
    write(R).

player_turn(game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, _, ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor),NewGameState):-
    (write("\nJogador Atual: "),
     write(CurrentPlayer),
     maplist(infoPiece, Pieces),
     rollDiceDebug(D)),
    % Interacao Jogador

    findmoves:get_available_moves(game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, D, ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor),
        AvailableMoves),
    (AvailableMoves == [] -> 
        (
        write("\nNenhum movimento disponivel, digite qualquer coisa para continuar: "),
        read(_),
        TempGameState = game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, D, ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor)
        ) 
        ;
        (
        write(AvailableMoves),
        write("\nEscolha um Movimento (0 - n): "),
        read(ChosenMove),
        nth1(ChosenMove, AvailableMoves, MoveToPlay),
        processmove:process_move(
            game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, D, ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor),
            MoveToPlay,
            TempGameState
        )
        )
    ),
    write("Passando para o proximo jogador..."),

    % Condição de parada do gameCycle temporária
    read(A),
    (A == "fim" -> NewGameState = [] ;
    NewGameState = TempGameState
    ).

gameCycle([]) :- !.
gameCycle(game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, DiceRolled, ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor)) :-
    auxiliary:get_player_by_color(Players,CurrentPlayer,player(_,Bot,_)),
    (Bot == true -> 
        (rollDiceBot(DiceBot),
        bot:bot_choose_move(game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, DiceBot, ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor),
        game_state(Players, SpecialTiles, NewPieces, NewBlockades, CurrentPlayer, D, NProcessingMove, NEnd, NSixesInRow, NWasLuckyMove, NWinnerColor)))
        ; 
        player_turn(game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, DiceRolled, ProcessingMove, 
        End, SixesInRow, WasLuckyMove, WinnerColor),game_state(Players, SpecialTiles, NewPieces, NewBlockades, CurrentPlayer, D, NProcessingMove, NEnd, NSixesInRow, NWasLuckyMove, NWinnerColor))
    ),
    auxiliary:get_next_player(Players, CurrentPlayer, NextPlayer),

    % Verificar se última jogada resultou em um 6 e seguir fluxo previsto
    FSixInRow is NSixesInRow + 1,
    (D =:= 6 -> 
        (NSixesInRow =:= 2 -> (
            gameCycle(game_state(Players, SpecialTiles, NewPieces, NewBlockades, NextPlayer, D, NProcessingMove, NEnd, 0, NWasLuckyMove, NWinnerColor))
            ) ; (
                gameCycle(game_state(Players, SpecialTiles, NewPieces, NewBlockades, CurrentPlayer, D, NProcessingMove, NEnd, FSixInRow, NWasLuckyMove, NWinnerColor))
            )
        ) ; (
            gameCycle(game_state(Players, SpecialTiles, NewPieces, NewBlockades, NextPlayer, D, NProcessingMove, NEnd, NSixesInRow, NWasLuckyMove, NWinnerColor))
        )
    ).
    
init :-
    use_module(gametypes),
    use_module(creategame),
    use_module(auxiliary),
    use_module(save_and_load),
    use_module(findmoves),
    use_module(processmove),
    use_module(bot),
    write("-------------- Ludo Modificado -------------------\n"),
    % Implementar a parte de loading do save
    write("Deseja carregar um jogo salvo? (y/n) "),
    read(Load),
    (Load == 'y' -> (save_and_load:load_game(File,GameState), gameCycle(GameState)) ; % Load ainda não funcional
        write("Quantos Jogadores? (2 ou 4): "),
        read(NumPlayers),
        % Verificar validade da entrada

        write("\nQuantos Bots? (0, 1, 2 ou 3): "),
        read(NumBots),
        % Verificar validade da entrada
        creategame:create_game_state(NumPlayers,NumBots,GameState),
        write("\nO jogo começou! Boa sorte!"),

        write("\nNum Jogadores: "),
        write(NumPlayers),
        write("\nNum Bots: "),
        write(NumBots),

        gameCycle(GameState)).

