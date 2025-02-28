module InterfaceDebug where

import Game
import GameTypes
import qualified System.Console.ANSI as ANSI
import System.Random (randomRIO)

initGameTerminal :: IO ()
initGameTerminal = do
  putStrLn "-------------- Interface Terminal -------------------"
  putStrLn "Quantos Jogadores? (2 ou 4)"

  jogadores <- getLine
  let jogadoresInt = read jogadores :: Int

  putStrLn "Quantos Bots? (1, 2 ou 3)"
  bots <- getLine
  let botsInt = read bots :: Int

  let game = createGameState jogadoresInt botsInt
  putStrLn "-------------- Início do Jogo -------------------"
  gameLoop game

getDiceRoll :: IO Int
getDiceRoll = randomRIO (1, 6)

gameLoop :: GameState -> IO ()
gameLoop gameState = do
  printGameState gameState
  putStrLn "Digite 'r' para rolar o dado"
  putStrLn "Digite 'q' para sair"
  command <- getLine
  case command of
    "r" -> do
      diceRoll <- getDiceRoll
      putStrLn $ "Valor do dado: " ++ show diceRoll
      -- to be implemented
      -- let availableMoves = getAvailableMoves gameState diceRoll

      -- showAvailableMoves availableMoves

      -- gameLoop newGameState
      gameLoop gameState
    "q" -> return ()
    _ -> do
      putStrLn "Comando inválido"
      gameLoop gameState

-- Função para imprimir o estado atual do jogo
printGameState :: GameState -> IO ()
printGameState gameState = do
  putStrLn "Estado atual do jogo:"
  putStrLn "Jogadores:"
  mapM_ printPlayer (players gameState)
  putStrLn $ "Casas especiais: " ++ show (specialTiles gameState)
  putStrLn $ "Jogador atual: " ++ show (currentPlayer gameState)
  putStrLn $ "Valor do dado: " ++ show (diceRolled gameState)
  putStrLn $ "Fim do jogo: " ++ show (end gameState)
  putStrLn $ "Seis seguidos: " ++ show (sixesInRow gameState)
  putStrLn $ "\n"

printPlayer :: Player -> IO ()
printPlayer player = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (playerColorToANSI (playerColor player))]
  putStrLn $ "Jogador: " ++ show (playerColor player)
  putStrLn $ "  Peças: " ++ show (pieces player)
  putStrLn $ "  É bot: " ++ show (isBot player)
  putStrLn $ "  Posição inicial: " ++ show (startingPos player)
  ANSI.setSGR [ANSI.Reset]

playerColorToANSI :: Color -> ANSI.Color
playerColorToANSI Red = ANSI.Red
playerColorToANSI Blue = ANSI.Blue
playerColorToANSI Green = ANSI.Green
playerColorToANSI Yellow = ANSI.Yellow