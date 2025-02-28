module InterfaceDebug where

import Game
import GameTypes
import qualified System.Console.ANSI as ANSI

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
  printGameState (game)

-- Função para imprimir o estado atual do jogo
printGameState :: GameState -> IO ()
printGameState gameState = do
  putStrLn "Estado atual do jogo:"
  putStrLn "Jogadores:"
  mapM_ printPlayer (players gameState)
  putStrLn $ "Casas especiais: " ++ show (specialTiles gameState)
  putStrLn $ "Jogador atual: " ++ show (currentPlayer gameState)
  putStrLn $ "Valor do dado: " ++ show (rollDice gameState)
  putStrLn $ "Fim do jogo: " ++ show (end gameState)
  putStrLn $ "Seis seguidos: " ++ show (sixesInRow gameState)

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