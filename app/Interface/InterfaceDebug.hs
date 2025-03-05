{-# LANGUAGE BlockArguments #-}

module Interface.InterfaceDebug where

import Game.LoadSaveState
import GHC.Generics (Generic)
import Game.BotLogic (getBestMove)
import Game.Index
import GameTypes
import qualified System.Console.ANSI as ANSI
import System.Random (randomRIO)
import Text.Read (readMaybe)

initGameTerminal :: IO ()
initGameTerminal = do
  putStrLn "-------------- Interface Terminal -------------------"
  putStrLn "Deseja carregar um jogo salvo? (s/n)"
  loadChoice <- getLine
  case loadChoice of
    "s" -> do
      maybeGame <- loadGameState
      case maybeGame of
        Just game -> do
          putStrLn "\nJogo salvo carregado com sucesso!"
          gameLoop game
        Nothing -> do
          putStrLn "\nNenhum jogo salvo encontrado. Iniciando um novo jogo..."
          startNewGame
    _ -> startNewGame

-- getDiceRoll :: IO Int
-- getDiceRoll = randomRIO (1, 6)
getDiceRoll :: IO Int
getDiceRoll = do
  putStrLn "Digite o valor do dado (1 a 6):"
  input <- getLine
  return (read input)

gameLoop :: GameState -> IO ()
gameLoop gameState = do
  printGameState gameState
  if checkGameOver gameState
    then putStrLn "O jogo terminou! Temos um vencedor!"
    else
      if isBotTurn gameState
        then do
          putStrLn "Vez do bot!"
          botTurn gameState
        else do
          putStrLn "Digite 'r' para rolar o dado"
          putStrLn "Digite 's' para salvar o jogo"
          putStrLn "Digite 'q' para sair"
          command <- getLine
          case command of
            "r" -> playerTurn gameState
            "s" -> do
              saveGameState gameState
              gameLoop gameState
            "q" -> putStrLn "Jogo encerrado. Obrigado por jogar!"
            _ -> do
              putStrLn "Comando inválido, tente novamente."
              gameLoop gameState

startNewGame :: IO ()
startNewGame = do
  jogadoresInt <- getValidNumber "Quantos Jogadores? (2 ou 4)" [2, 4]
  botsInt <- getValidNumber "Quantos Bots? (0, 1, 2 ou 3)" [0, 1, 2, 3]
  let game = createGameState jogadoresInt botsInt
  putStrLn "\nO jogo começou! Boa sorte!"
  gameLoop game

playerTurn :: GameState -> IO ()
playerTurn gameState = do
  diceRoll <- getDiceRoll
  putStrLn $ "\nVocê rolou: " ++ show diceRoll ++ "!"

  -- Atualizar o número de 6 seguidos corretamente
  let newSixesInRow = if diceRoll == 6 then sixesInRow gameState + 1 else 0
  let gameStateWithDice = gameState {diceRolled = diceRoll, sixesInRow = newSixesInRow}

  -- Verificar se tirou 3 seis seguidos
  if newSixesInRow >= 3
    then do
      putStrLn "Você tirou três 6 seguidos! Perde a vez."
      gameLoop (nextPlayer gameStateWithDice {sixesInRow = 0}) -- Resetando `sixesInRow`
    else do
      let availableMoves = filterSafeMoves gameStateWithDice (getAvailableMoves gameStateWithDice)
      if null availableMoves
        then do
          putStrLn "Nenhum movimento disponível! Passando o turno..."
          gameLoop (nextPlayer gameStateWithDice)
        else do
          putStrLn "Escolha um movimento:"
          mapM_ (uncurry printMove) (zip [1 ..] availableMoves)
          chosenIndex <- getMoveChoice (length availableMoves)
          let (from, to) = availableMoves !! chosenIndex
          putStrLn $ "Você escolheu mover de " ++ show from ++ " para " ++ show to
          let updatedGameState = processMove gameStateWithDice (from, to)

          -- Verificar se caiu na casa de sorte
          if to `elem` map tilePosition (filter ((== Lucky) . tileType) (specialTiles gameStateWithDice))
            then do
              putStrLn "Você caiu em uma casa de sorte! Escolha um oponente para voltar para a base:"
              let oponnents = getLuckyMoves updatedGameState
              if null oponnents
                then do
                  putStrLn "Nenhum oponente disponível para voltar para a base. Continuando..."
                  gameLoop (nextPlayer updatedGameState)
                else do
                  putStrLn "Oponentes disponíveis:"
                  mapM_ (uncurry printOponnent) (zip [1 ..] oponnents)

                  putStrLn "Qual oponente mandar de volta para a base?"
                  oponentIndex <- getMoveChoice (length oponnents)
                  let oponent = oponnents !! oponentIndex
                  let luckyProcessedGameState = processLuckyMove updatedGameState oponent
                  if diceRoll == 6
                    then playerTurn luckyProcessedGameState -- Continua jogando se tirou 6
                    else gameLoop (nextPlayer luckyProcessedGameState)
            else
              if diceRoll == 6
                then playerTurn updatedGameState
                else gameLoop (nextPlayer updatedGameState)

botTurn :: GameState -> IO ()
botTurn gameState = do
  diceRoll <- getDiceRoll
  putStrLn $ "O bot rolou: " ++ show diceRoll ++ "!"
  let gameStateWithDice = gameState {diceRolled = diceRoll}
  let updatedSixesInRow = if diceRoll == 6 then sixesInRow gameState + 1 else 0
  let gameStateSixHandled = gameStateWithDice {sixesInRow = updatedSixesInRow}

  if updatedSixesInRow >= 3
    then do
      putStrLn "O bot tirou três 6 seguidos e perdeu a vez."
      let resetGameState = gameStateSixHandled {sixesInRow = 0}
      gameLoop (nextPlayer resetGameState)
    else do
      let availableMoves = filterSafeMoves gameStateSixHandled (getAvailableMoves gameStateSixHandled)

      if null availableMoves
        then do
          putStrLn "O bot não pode se mover. Passando o turno..."
          gameLoop (nextPlayer gameStateSixHandled)
        else do
          let bestMove = getBestMove gameStateSixHandled availableMoves
          let (from, to) = bestMove

          putStrLn $ "O bot moveu de " ++ show from ++ " para " ++ show to

          let updatedGameState = processMove gameStateSixHandled bestMove
          if to `elem` map tilePosition (filter ((== Lucky) . tileType) (specialTiles gameStateSixHandled))
            then do
              putStrLn "O bot caiu em uma casa de sorte e escolheu um oponente para voltar para a base."
              let oponnents = getLuckyMoves updatedGameState
              if null oponnents
                then gameLoop (nextPlayer updatedGameState) -- Garante que o turno passa
                else do
                  let oponent = head oponnents
                  let luckyProcessedGameState = processLuckyMove updatedGameState oponent
                  gameLoop (nextPlayer luckyProcessedGameState) -- Garante que o turno passa
            else
              if diceRoll == 6
                then botTurn updatedGameState
                else gameLoop (nextPlayer updatedGameState)

isBotTurn :: GameState -> Bool
isBotTurn gameState =
  let current = currentPlayer gameState
   in any ((== current) . playerColor) (filter isBot (players gameState))

getMoveChoice :: Int -> IO Int
getMoveChoice numChoices = do
  putStrLn "Digite o número da jogada desejada:"
  input <- getLine
  case readMaybe input of
    Just n | n > 0 && n <= numChoices -> return (n - 1)
    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      getMoveChoice numChoices

-- Exibe o estado do jogo com formatação aprimorada
printGameState :: GameState -> IO ()
printGameState gameState = do
  putStrLn "\nEstado atual do jogo:"
  putStrLn "---------------------------"
  putStrLn "Jogadores:"
  mapM_ printPlayer (players gameState)
  putStrLn "Peças no tabuleiro:"
  printPieces (pieces gameState)
  putStrLn $ "Bloqueios: " ++ show (blockades gameState)
  putStrLn $ "Casas especiais: " ++ show (specialTiles gameState)
  putStrLn $ "Jogador atual: " ++ show (currentPlayer gameState)
  putStrLn $ "Valor do dado: " ++ show (diceRolled gameState)
  putStrLn $ "Jogo finalizado? " ++ show (end gameState)
  putStrLn $ "Seis seguidos: " ++ show (sixesInRow gameState)
  putStrLn "---------------------------"

printPieces :: [Piece] -> IO ()
printPieces pieces = mapM_ printPiece pieces

printPiece :: Piece -> IO ()
printPiece piece = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (playerColorToANSI (pieceColor piece))]
  putStrLn $ "Peça: " ++ show piece
  ANSI.setSGR [ANSI.Reset]

printPlayer :: Player -> IO ()
printPlayer player = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (playerColorToANSI (playerColor player))]
  putStrLn $ "Jogador: " ++ show (playerColor player)
  putStrLn $ "Bot: " ++ show (isBot player)
  putStrLn $ "Posição inicial: " ++ show (startingPos player)
  ANSI.setSGR [ANSI.Reset]

printMove :: Int -> (Int, Int) -> IO ()
printMove index (from, to) = putStrLn $ show index ++ ". De " ++ show from ++ " para " ++ show to

printOponnent :: Int -> Int -> IO ()
printOponnent index oponent = putStrLn $ show index ++ ". Oponente: " ++ show oponent

getValidNumber :: String -> [Int] -> IO Int
getValidNumber prompt validValues = do
  putStrLn prompt
  input <- getLine
  case readMaybe input of
    Just n | n `elem` validValues -> return n
    _ -> do
      putStrLn "entrada inválida. Tente novamente."
      getValidNumber prompt validValues

playerColorToANSI :: Color -> ANSI.Color
playerColorToANSI Red = ANSI.Red
playerColorToANSI Blue = ANSI.Blue
playerColorToANSI Green = ANSI.Green
playerColorToANSI Yellow = ANSI.Yellow
