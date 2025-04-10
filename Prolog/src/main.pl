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
        write("\nNenhum movimento disponível. Pressione Enter para continuar..."), nl,
        read(_),
        TempGameState = game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, D, ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor)
        ) 
        ;
        (
        auxiliary:write_special_tiles(SpecialTiles),
        write(AvailableMoves),
        write("\nEscolha um Movimento: "),
        read(ChosenMove),
        nth1(ChosenMove, AvailableMoves, MoveToPlay),
        processmove:process_move(
            game_state(Players, SpecialTiles, Pieces, Blockades, CurrentPlayer, D, ProcessingMove, End, SixesInRow, WasLuckyMove, WinnerColor),
            MoveToPlay,
            TempGameState
        ),
        auxiliary:getToFromMove(MoveToPlay,To),

        ( member(special_tile(Type, To), SpecialTiles) ->
        ( Type = lucky ->
            write("\nSua peça caiu na casa Lucky! Selecione a posição de uma peça inimiga para trazer de volta à base (0 - n): "), 
            auxiliary:get_enemies(Pieces,CurrentPlayer,Enemies),
            auxiliary:get_pieces_locations(Enemies,Locations),
            write(Locations),
            read(luckyTargetPos), 
            %Eliminar peça
            write("\nPeça inimiga retornada à base com sucesso!")
        ; Type = boost ->
            write("\nBoost ativado! Sua peça andará 3 casas a mais automaticamente.\n")
            % Aqui você pode adicionar lógica para movimentar 3 casas extras se quiser
        ; Type = death ->
            write("\nOh não! Sua peça caiu na casa da Morte e voltará para a base.\n")
            % Aqui você pode aplicar a lógica de retornar a peça para a base
        ; Type = decline ->
            write("\nVocê caiu numa casa Decline. Algo de ruim pode acontecer aqui...\n")
            % Lógica de decline (se tiver)
        ; Type = safe ->
            write("\nCasa segura! Nenhum jogador pode ser capturado aqui.\n")
        ; true
        )
        ; true
        )
    ),
    write("Passando para o proximo turno(Digite qualquer coisa para prosseguir)"), nl,
    write("Caso queira encerrar a partida digite fim"),

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
    % Salvar jogo após o turno
        save_and_load:save_game("save.json",
        game_state(Players, SpecialTiles, NewPieces, NewBlockades, NextPlayer, D, NProcessingMove, NEnd, NSixesInRow, NWasLuckyMove, NWinnerColor)),

    % Verificar se última jogada resultou em um 6 e seguir fluxo previsto
    FSixInRow is NSixesInRow + 1,
    (D =:= 6 -> 
        (NSixesInRow =:= 2 -> (
            NewGameState = game_state(Players, SpecialTiles, NewPieces, NewBlockades, NextPlayer, D, NProcessingMove, NEnd, 0, NWasLuckyMove, NWinnerColor)
            ) ; (
                NewGameState = game_state(Players, SpecialTiles, NewPieces, NewBlockades, CurrentPlayer, D, NProcessingMove, NEnd, FSixInRow, NWasLuckyMove, NWinnerColor)
            )
        ) ; (
            NewGameState = game_state(Players, SpecialTiles, NewPieces, NewBlockades, NextPlayer, D, NProcessingMove, NEnd, NSixesInRow, NWasLuckyMove, NWinnerColor)
        )
    ),
    gameCycle(NewGameState).
    
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
    (Load == 'y' -> (save_and_load:load_game("save.json",GameState), gameCycle(GameState)) ; % Load ainda não funcional
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

