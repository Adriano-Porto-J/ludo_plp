module Interface.InterfaceDebug where

import Game.Index
import GameTypes
import qualified System.Console.ANSI as ANSI
import System.Random (randomRIO)
import Text.Read (readMaybe)

initGameTerminal :: IO ()
initGameTerminal = do
  putStrLn "-------------- Interface Terminal -------------------"
  jogadoresInt <- getValidNumber "Quantos Jogadores? (2 ou 4)" [2, 4]
  botsInt <- getValidNumber "Quantos Bots? (1, 2 ou 3)" [1, 2, 3]

  let game = createGameState jogadoresInt botsInt
  putStrLn "\nO jogo começou! Boa sorte!"
  gameLoop game

getValidNumber :: String -> [Int] -> IO Int
getValidNumber prompt validValues = do
  putStrLn prompt
  input <- getLine
  case readMaybe input of
    Just n | n `elem` validValues -> return n
    _ -> do
      putStrLn "entrada inválida. Tente novamente."
      getValidNumber prompt validValues

getDiceRoll :: IO Int
getDiceRoll = randomRIO (1, 6)

-- apenas para testes
-- getDiceRoll :: IO Int
-- getDiceRoll = randomRIO (5, 5)

gameLoop :: GameState -> IO ()
gameLoop gameState = do
  putStrLn "\n----------------------------------"
  putStrLn "Digite 'r' para rolar o dado"
  putStrLn "Digite 'q' para sair"
  command <- getLine
  case command of
    "r" -> do
      diceRoll <- getDiceRoll
      putStrLn $ "\nVocê rolou: " ++ show diceRoll ++ "!"

      let gameStateWithDice = gameState {diceRolled = diceRoll}
      let gameStateSixHandled = handleSixesInRow gameStateWithDice
      let availableMoves = getAvailableMoves gameStateSixHandled

      printGameState gameStateSixHandled
      if null availableMoves
        then do
          putStrLn "Nenhum movimento disponível! Passando o turno..."
          ANSI.setSGR [ANSI.Reset]
        else do
          putStrLn "Escolha um movimento:"
          mapM_ (uncurry printMove) (zip [1 ..] availableMoves)
          chosenIndex <- getMoveChoice (length availableMoves)
          let (from, to) = availableMoves !! chosenIndex
          putStrLn $ "Você escolheu mover de " ++ show from ++ " para " ++ show to
          let gameStateMoveProcessed = processMove gameStateSixHandled (from, to)
          putStrLn $ "Movimento processado para jogador: " ++ show (currentPlayer gameStateMoveProcessed)
          let gameStateUpdated = nextPlayer gameStateMoveProcessed --
          gameLoop gameStateUpdated

    -- let gameStateUpdated = nextPlayer gameStateMoveProcessed
    -- gameLoop gameStateUpdated -- Próximo jogador!
    "q" -> putStrLn "Jogo encerrado. Obrigado por jogar!"
    _ -> do
      putStrLn "Comando inválido, tente novamente."
      gameLoop gameState

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

playerColorToANSI :: Color -> ANSI.Color
playerColorToANSI Red = ANSI.Red
playerColorToANSI Blue = ANSI.Blue
playerColorToANSI Green = ANSI.Green
playerColorToANSI Yellow = ANSI.Yellow
