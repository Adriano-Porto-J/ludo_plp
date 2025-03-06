module Interface.Rendering where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified GameTypes
import Game.CreateGame
import Game.ProcessMove
import Game.BotLogic
import Game.Index
import Game.Auxiliary (printGameState)
import Game.LoadSaveState 
import Interface.RenderBoard
import Interface.DrawOnBoard
import Interface.Index
import Interface.DrawMenu
import Interface.DrawGameUI
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)

-- Lógica
initialGameState :: GameTypes.GameState -- Estado Inicial do Jogo
initialGameState = (Game.CreateGame.createGameState 4 2) { GameTypes.screenState = GameTypes.MenuInicial }

transformGameIO :: Event -> GameTypes.GameState -> IO GameTypes.GameState
transformGameIO (EventKey (MouseButton LeftButton) Up _ (x, y)) gameState
    -- Menu Inicial: Se o clique for no botão "Novo Jogo"
    | GameTypes.screenState gameState == GameTypes.MenuInicial && x >= -100 && x <= 100 && y >= 25 && y <= 75 =
        return gameState { GameTypes.screenState = GameTypes.MenuPlayers }

     -- Menu Inicial: Se o clique for no botão "Carregar Jogo Salvo"
    | GameTypes.screenState gameState == GameTypes.MenuInicial && x >= -150 && x <= 150 && y >= -75 && y <= -25 = do
        maybeGameState <- loadGameState
        case maybeGameState of
            Just savedState -> return savedState  -- Se houver um jogo salvo, carrega ele
            Nothing -> return gameState { GameTypes.screenState = GameTypes.MenuInicial }  -- Caso contrário, apenas muda para JogoEmAndamento

    -- Menu de Jogadores: Se o clique for na seleção de quantidade de jogadores 1
    | GameTypes.screenState gameState == GameTypes.MenuPlayers && x >= -125 && x <= -75 && y >= 55  && y <= 105 =
        return gameState { GameTypes.screenState = GameTypes.MenuBotsUmPlayer }  -- Muda para o estado de menuBotsUmPlayer

    -- Menu de Jogadores: Se o clique for na seleção de quantidade de jogadores 2
    | GameTypes.screenState gameState == GameTypes.MenuPlayers && x >= 75 && x <= 125 && y >= 55 && y <= 105 =
        return gameState { GameTypes.screenState = GameTypes.MenuBotsDoisPlayer }  -- Muda para o estado de menuBotsDoisPlayer

    -- Menu de Jogadores: Se o clique for na seleção de quantidade de jogadores 3
    | GameTypes.screenState gameState == GameTypes.MenuPlayers && x >= -125 && x <= -75 && y >= -65 && y <= -15 =
        return gameState { GameTypes.screenState = GameTypes.MenuBotsTresPlayer }  -- Muda para o estado de menuBotsTresPlayer

    -- Menu de Jogadores: Se o clique for na seleção de quantidade de jogadores 4
    | GameTypes.screenState gameState == GameTypes.MenuPlayers && x >= 75 && x <= 125 && y >= -65 && y <= -15 =
        let newGameState = Game.CreateGame.createGameState 4 0
        in return newGameState { GameTypes.screenState = GameTypes.JogoEmAndamento }  -- Muda para o estado de jogoEmAndamento

    -- MenuBotsUmplayer: Processa o clique baseado no número de jogadores = 1 e número de bots
    | GameTypes.screenState gameState == GameTypes.MenuBotsUmPlayer && x >= -125 && x <= -75 && y >= 55 && y <= 105 = 
        let newGameState = Game.CreateGame.createGameState 2 1 
        in return newGameState { GameTypes.screenState = GameTypes.JogoEmAndamento}
    | GameTypes.screenState gameState == GameTypes.MenuBotsUmPlayer && x >= 75 && x <= 125 && y >= 55 && y <= 105 = 
        let newGameState = Game.CreateGame.createGameState 3 2 --falha
        in return newGameState { GameTypes.screenState = GameTypes.JogoEmAndamento }
    | GameTypes.screenState gameState == GameTypes.MenuBotsUmPlayer && x >= -125 && x <= -75 && y >= -65 && y <= -15 = 
        let newGameState = Game.CreateGame.createGameState 4 3
        in return newGameState { GameTypes.screenState = GameTypes.JogoEmAndamento }
    
    -- MenuBotsDoisplayer: Processa o clique baseado no número de jogadores = 2 e número de bots
    | GameTypes.screenState gameState == GameTypes.MenuBotsDoisPlayer && x >= -120 && x <= -75 && y >= 55 && y <= 105 = 
        let newGameState = Game.CreateGame.createGameState 2 0
        in return newGameState { GameTypes.screenState = GameTypes.JogoEmAndamento }
    | GameTypes.screenState gameState == GameTypes.MenuBotsDoisPlayer && x >= 75 && x <= 125 && y >= 55 && y <= 105 = 
        let newGameState = Game.CreateGame.createGameState 3 1 
        in return newGameState { GameTypes.screenState = GameTypes.JogoEmAndamento }
    | GameTypes.screenState gameState == GameTypes.MenuBotsDoisPlayer && x >= -125 && x <= -75 && y >= -65 && y <= -15 = 
        let newGameState = Game.CreateGame.createGameState 4 2 
        in return newGameState { GameTypes.screenState = GameTypes.JogoEmAndamento }

    -- MenuBotsTresplayer: Processa o clique baseado no número de jogadores = 3 e número de bots
    | GameTypes.screenState gameState == GameTypes.MenuBotsTresPlayer && x >= -120 && x <= -75 && y >= 55 && y <= 105 = 
        let newGameState = Game.CreateGame.createGameState 3 0
        in return newGameState { GameTypes.screenState = GameTypes.JogoEmAndamento }
    | GameTypes.screenState gameState == GameTypes.MenuBotsTresPlayer && x >= 75 && x <= 125 && y >= 55 && y <= 105  = 
        let newGameState = Game.CreateGame.createGameState 4 1
        in return newGameState { GameTypes.screenState = GameTypes.JogoEmAndamento }
   

    -- Jogo em Andamento: Processa os cliques para rolar o dado ou mover as peças
    | GameTypes.screenState gameState == GameTypes.JogoEmAndamento &&
        x >= 3.5 * cellSize && x <= 6.5 * cellSize && y >= -9 * cellSize && y <= -7 * cellSize = do
            saveGameState gameState
            return gameState
    | GameTypes.screenState gameState == GameTypes.JogoEmAndamento &&
        x > xMin && x < xMax && y > yMin && y < yMax = rolarDadoIO gameState  -- Rola o dado
    | GameTypes.screenState gameState == GameTypes.JogoEmAndamento &&
        x > boardXMin && x < boardXMax && y > boardYMin && y < boardYMax = do
        let selectedPos = selectPosition gameState (x / cellSize) (y / cellSize)        
        putStrLn $ "Casa selecionada " ++ show selectedPos 
        let updatedGameState = handleTurn gameState (selectedPos) -- Seleciona peça
        if GameTypes.wasLuckyMove updatedGameState
            then do
                let validLuckyMoves = Game.Index.getLuckyMoves updatedGameState
                if null validLuckyMoves
                    then do
                        printGameState updatedGameState
                        putStrLn "Lucky Move não disponível." 
                        return (nextPlayer (updatedGameState {GameTypes.diceRolled = -1, GameTypes.wasLuckyMove = False}))
                    else
                        do
                            printGameState updatedGameState
                            putStrLn "LuckyMove"
                            putStrLn $ show validLuckyMoves
                            return updatedGameState
            else do
                printGameState updatedGameState
                return (updatedGameState)
    | otherwise = return gameState  -- Não faz nada para outros cliques
  where
    -- Coordenadas do botão de rolar o dado
    xMin = -7 * cellSize
    xMax = -1 * cellSize
    yMin = -9 * cellSize
    yMax = -7 * cellSize

    -- Coordenadas do tabuleiro
    boardXMin = -6.5 * cellSize
    boardXMax =  6.5 * cellSize
    boardYMin = -6.5 * cellSize
    boardYMax =  6.5 * cellSize

transformGameIO _ gameState = return gameState  -- Não faz nada para outros eventos

handleTurn :: GameTypes.GameState -> Int ->  GameTypes.GameState
handleTurn gameState piece = 
    if GameTypes.wasLuckyMove gameState
        then do
            if Game.Index.isBotTurn gameState
                then botLuckyMove gameState
                else playerLuckyMove gameState piece
        else do
            if Game.Index.isBotTurn gameState
            then botTurn gameState
            else playerTurn gameState piece 

botLuckyMove :: GameTypes.GameState -> GameTypes.GameState
botLuckyMove gameState = do
        let validLuckyMoves = Game.Index.getLuckyMoves gameState
            in if null validLuckyMoves
                then nextPlayer (gameState {GameTypes.diceRolled = -1, GameTypes.wasLuckyMove = False})
                else do
                    let move = Game.BotLogic.getBotLuckyMove validLuckyMoves
                    let newState = Game.Index.processLuckyMove gameState move
                    newState { GameTypes.diceRolled = -1, GameTypes.wasLuckyMove = False }

playerLuckyMove :: GameTypes.GameState -> Int -> GameTypes.GameState
playerLuckyMove gameState piecePos = do
    let validLuckyMoves = Game.Index.getLuckyMoves gameState
    if null validLuckyMoves
        then nextPlayer (gameState {GameTypes.diceRolled = -1, GameTypes.wasLuckyMove = False})
        else
            let moves = filter (\start -> start == piecePos) validLuckyMoves
            in if moves /= []
                then
                    let move = head moves
                        newState = Game.Index.processLuckyMove gameState move
                    in nextPlayer (newState { GameTypes.diceRolled = -1, GameTypes.wasLuckyMove = False })
                else gameState

botTurn :: GameTypes.GameState -> GameTypes.GameState
botTurn gameState =
      if GameTypes.diceRolled gameState /= -1 
        then
         if GameTypes.diceRolled gameState == 6 && GameTypes.sixesInRow gameState < 3
          then
            let availableMoves = Game.Index.getAvailableMoves gameState
            in if availableMoves /= []
              then
                let move = Game.BotLogic.getBestMove gameState availableMoves
                    newState = Game.Index.processMove gameState move
                in if snd move == 25
                     then newState { GameTypes.wasLuckyMove = True }
                     else newState { GameTypes.diceRolled = -1 }
              else nextPlayer gameState
          else
            let availableMoves = Game.Index.getAvailableMoves gameState
            in if availableMoves /= []
              then
                let move = Game.BotLogic.getBestMove gameState availableMoves
                    newState = Game.Index.processMove gameState move
                in if snd move == 25
                     then newState { GameTypes.wasLuckyMove = True }
                     else nextPlayer newState { GameTypes.diceRolled = -1 }
              else nextPlayer gameState
        else gameState  

playerTurn::GameTypes.GameState -> Int -> GameTypes.GameState --Seleciona a peça de acordo com a localização do tabuleiro e realiza a jogada
playerTurn gameState piecePos = case piece of
  Nothing -> let availableMoves = Game.Index.getAvailableMoves gameState
                in if availableMoves == []
                    then nextPlayer gameState
                    else gameState
  Just piece ->
    if GameTypes.processingMove (gameState) == True && piecePos > -5
      then
        if GameTypes.diceRolled gameState == 6 && GameTypes.sixesInRow gameState < 3
          then do
            let availableMoves = Game.Index.getAvailableMoves gameState -- Retorna Movimentos disponíveis
            if availableMoves /= [] -- Verifica se há movimentos disponíveis
              then do
                let moves = filter (\(start, _) -> start == piecePos) availableMoves -- Há movimentos com a peça selecionada
                if moves /= []
                  then do
                    let move = head moves -- se houver movimentos com a peça selecionada faça o movimento
                    let newState = Game.Index.processMove gameState move
                    if snd move == 25
                      then newState { GameTypes.wasLuckyMove = True }
                      else newState { GameTypes.diceRolled = -1 }
                  else gameState -- se não houver retorne para o jogador
              else nextPlayer gameState -- se não há nenhum movimento disponível passe para o próximo
          else do
            let availableMoves = Game.Index.getAvailableMoves gameState
            if availableMoves /= []
              then do
                let moves = filter (\(start, _) -> start == piecePos) availableMoves
                if moves /= []
                  then do
                    let move = head moves
                    let newState = Game.Index.processMove gameState move
                    if snd move == 25
                      then newState { GameTypes.wasLuckyMove = True }
                      else nextPlayer newState { GameTypes.diceRolled = -1 }
                  else gameState
              else nextPlayer gameState
      else gameState
  where
    piece = getPieceByPositionAndColor (GameTypes.pieces gameState) (GameTypes.currentPlayer gameState) piecePos

selectPosition::GameTypes.GameState -> Float -> Float -> Int
selectPosition gameState x y | basePos!!0 < x && basePos!!1 > x && basePos!!2 < y && basePos!!3 > y = -1
                             | (basePos!!0 + 1) < x && (basePos!!1 + 1) > x && basePos!!2 < y && basePos!!3 > y = -2
                             | basePos!!0 < x && basePos!!1 > x && (basePos!!2 - 1) < y && (basePos!!3 - 1) > y = -3
                             | (basePos!!0 + 1) < x && (basePos!!1 + 1) > x && (basePos!!2 - 1) < y && (basePos!!3 - 1) > y = -4
                             | x < -4.5 && x > -5.5 && y < 1.5 && y > 0.5 = 0
                             | x < -3.5 && x > -4.5 && y < 1.5 && y > 0.5 = 1
                             | x < -2.5 && x > -3.5 && y < 1.5 && y > 0.5 = 2
                             | x < -1.5 && x > -2.5 && y < 1.5 && y > 0.5 = 3
                             | x < -0.5 && x > -1.5 && y < 1.5 && y > 0.5 = 4
                             | x < -0.5 && x > -1.5 && y < 2.5 && y > 1.5 = 5
                             | x < -0.5 && x > -1.5 && y < 3.5 && y > 2.5 = 6
                             | x < -0.5 && x > -1.5 && y < 4.5 && y > 3.5 = 7
                             | x < -0.5 && x > -1.5 && y < 5.5 && y > 4.5 = 8
                             | x < -0.5 && x > -1.5 && y < 6.5 && y > 5.5 = 9
                             | x < 0.5 && x > -0.5 && y < 6.5 && y > 5.5 = 10
                             | x < 1.5 && x > 0.5 && y < 6.5 && y > 5.5 = 11
                             | x < 1.5 && x > 0.5 && y < 5.5 && y > 4.5 = 12
                             | x < 1.5 && x > 0.5 && y < 4.5 && y > 3.5 = 13
                             | x < 1.5 && x > 0.5 && y < 3.5 && y > 2.5 = 14
                             | x < 1.5 && x > 0.5 && y < 2.5 && y > 1.5 = 15
                             | x < 1.5 && x > 0.5 && y < 1.5 && y > 0.5 = 16
                             | x < 2.5 && x > 1.5 && y < 1.5 && y > 0.5 = 17
                             | x < 3.5 && x > 2.5 && y < 1.5 && y > 0.5 = 18
                             | x < 4.5 && x > 3.5 && y < 1.5 && y > 0.5 = 19
                             | x < 5.5 && x > 4.5 && y < 1.5 && y > 0.5 = 20
                             | x < 6.5 && x > 5.5 && y < 1.5 && y > 0.5 = 21
                             | x < 6.5 && x > 5.5 && y < 0.5 && y > -0.5 = 22
                             | x < 6.5 && x > 5.5 && y < -0.5 && y > -1.5 = 23
                             | x < 5.5 && x > 4.5 && y < -0.5 && y > -1.5 = 24
                             | x < 4.5 && x > 3.5 && y < -0.5 && y > -1.5 = 25
                             | x < 3.5 && x > 2.5 && y < -0.5 && y > -1.5 = 26
                             | x < 2.5 && x > 1.5 && y < -0.5 && y > -1.5 = 27
                             | x < 1.5 && x > 0.5 && y < -0.5 && y > -1.5 = 28
                             | x < 1.5 && x > 0.5 && y < -1.5 && y > -2.5 = 29
                             | x < 1.5 && x > 0.5 && y < -2.5 && y > -3.5 = 30
                             | x < 1.5 && x > 0.5 && y < -3.5 && y > -4.5 = 31
                             | x < 1.5 && x > 0.5 && y < -4.5 && y > -5.5 = 32
                             | x < 1.5 && x > 0.5 && y < -5.5 && y > -6.5 = 33
                             | x < 0.5 && x > -0.5 && y < -5.5 && y > -6.5 = 34
                             | x < -0.5 && x > -1.5 && y < -5.5 && y > -6.5 = 35
                             | x < -0.5 && x > -1.5 && y < -4.5 && y > -5.5 = 36
                             | x < -0.5 && x > -1.5 && y < -3.5 && y > -4.5 = 37
                             | x < -0.5 && x > -1.5 && y < -2.5 && y > -3.5 = 38
                             | x < -0.5 && x > -1.5 && y < -1.5 && y > -2.5 = 39
                             | x < -0.5 && x > -1.5 && y < -0.5 && y > -1.5 = 40
                             | x < -1.5 && x > -2.5 && y < -0.5 && y > -1.5 = 41
                             | x < -2.5 && x > -3.5 && y < -0.5 && y > -1.5 = 42
                             | x < -3.5 && x > -4.5 && y < -0.5 && y > -1.5 = 43
                             | x < -4.5 && x > -5.5 && y < -0.5 && y > -1.5 = 44
                             | x < -5.5 && x > -6.5 && y < -0.5 && y > -1.5 = 45
                             | x < -5.5 && x > -6.5 && y < 0.5 && y > -0.5 = 46
                             | x < -5.5 && x > -6.5 && y < 1.5 && y > 0.5 = 47
                             | x < -4.5 && x > -5.5 && y < 0.5 && y > -0.5 = 48 --Zona final do vermelho
                             | x < -3.5 && x > -4.5 && y < 0.5 && y > -0.5 = 49
                             | x < -2.5 && x > -3.5 && y < 0.5 && y > -0.5 = 50
                             | x < -1.5 && x > -2.5 && y < 0.5 && y > -0.5 = 51
                             | x < -0.5 && x > -1.5 && y < 0.5 && y > -0.5 = 52
                             | x < 0.5 && x > -0.5 && y < 5.5 && y > 4.5 = 54 --Zona final do amarelo
                             | x < 0.5 && x > -0.5 && y < 4.5 && y > 3.5 = 55
                             | x < 0.5 && x > -0.5 && y < 3.5 && y > 2.5 = 56
                             | x < 0.5 && x > -0.5 && y < 2.5 && y > 1.5 = 57
                             | x < 0.5 && x > -0.5 && y < 1.5 && y > 0.5 = 58
                             | x < 1.5 && x > 0.5 && y < 0.5 && y > -0.5 = 64 --Zona final do azul
                             | x < 2.5 && x > 1.5 && y < 0.5 && y > -0.5 = 63
                             | x < 3.5 && x > 2.5 && y < 0.5 && y > -0.5 = 62
                             | x < 4.5 && x > 3.5 && y < 0.5 && y > -0.5 = 61
                             | x < 5.5 && x > 4.5 && y < 0.5 && y > -0.5 = 60
                             | x < 0.5 && x > -0.5 && y < -0.5 && y > -1.5 = 70 --Zona final do azul
                             | x < 0.5 && x > -0.5 && y < -1.5 && y > -2.5 = 69
                             | x < 0.5 && x > -0.5 && y < -2.5 && y > -3.5 = 68
                             | x < 0.5 && x > -0.5 && y < -3.5 && y > -4.5 = 67
                             | x < 0.5 && x > -0.5 && y < -4.5 && y > -5.5 = 66
                             | otherwise = -5
    where basePos = baseQuadByColorMousePos(baseQuadByColor (GameTypes.currentPlayer (gameState)))

baseQuadByColorMousePos::(Int,Int) -> [Float]
baseQuadByColorMousePos (x,y)           | (x,y) == (-5,5) = [-5.5,-4.5,4.5,5.5] --vermelho
                                        | (x,y) == (4,5) = [3.5,4.5,4.5,5.5] --amarelo
                                        | (x,y) == (-5,-4) = [-5.5,-4.5,-4.5,-3.5] --verde
                                        | otherwise = [3.5,4.5,-4.5,-3.5] --azul

rolarDadoIO :: GameTypes.GameState -> IO GameTypes.GameState
rolarDadoIO gameState = do
    if ((GameTypes.diceRolled gameState) == -1 || ((GameTypes.diceRolled gameState) == -6 && GameTypes.sixesInRow gameState < 3))then do 
        dado <- randomRIO (1, 6)  -- Gera um número aleatório entre 1 e 6
        putStrLn ("Dado rolado: " ++ show dado)  -- Apenas para debug
        return gameState { GameTypes.diceRolled = dado, GameTypes.processingMove = True} -- Atualiza o estado do jogo
    else return gameState
--
-- Desenha a tela do jogo
drawScreen :: GameTypes.GameState -> IO Picture
drawScreen gameState =
   case GameTypes.screenState gameState of
    GameTypes.MenuInicial      -> return drawMenuInicial  -- Desenha o menu inicial
    GameTypes.MenuPlayers      -> return drawMenuPlayers  -- Desenha o menu de seleção de jogadores
    GameTypes.MenuBotsUmPlayer -> return drawMenuBotsUmPlayer   -- Menu para seleção de bots quando há um jogador
    GameTypes.MenuBotsDoisPlayer -> return drawMenuBotsDoisPlayer -- Menu para seleção de bots quando há dois jogadores
    GameTypes.MenuBotsTresPlayer -> return drawMenuBotsTresPlayer -- Menu para seleção de bots quando há três jogadores
    GameTypes.JogoEmAndamento  -> return $ pictures 
                          [ drawBoard                -- Desenha o tabuleiro
                          , drawAllPieces gameState  -- Desenha todas as peças
                          , drawButton               -- Desenha o botão de rolar o dado
                          , drawDice gameState       -- Desenha o dado com o valor atual
                          , drawButtonSaveTheGame    -- Desenha o botão de salvar o estado do jogo
                          , drawPlayerText gameState -- Desenha o texto indicando a vez do jogador  
                          , drawLuckyText gameState -- Desenha o texto indicando que o jogador deve escolher um peão adversário para capturar
                          , drawWinnerPlayer gameState -- Desenha a tela de fim de jogo
                          ]



render :: IO ()
render = playIO window background 30 initialGameState drawScreen transformGameIO (const (return . id))
-- Função principal

--Para um ambiente interativo, devemos utilizar a função play ao invés de render
--Mais sobre em: https://hackage.haskell.org/package/gloss-1.13.2.2/docs/Graphics-Gloss.html#v:play

--render = play background 30 gameState drawBoard inputHandling (const id)
--gameState sendo um tipo de dado que representa o estado do jogo
--drawBoard como sendo uma função que recebe esse tipo de dado e o transforma para Picture (gameState -> Picture)
--inputHandling sendo a função (Event -> gameState -> gameState) que atualiza o estado do jogo de acordo c a entrada
--(const id) sendo uma função que atualiza o gameState a cada segundo que se passa,t id) não faz nada
