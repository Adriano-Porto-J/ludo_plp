module InterfaceDebug where

import Game

initGameTerminal :: IO ()
initGameTerminal = do
  putStrLn "Interface Terminal"
  putStrLn "Quantos Jogadores? (2 ou 4)"

  jogadores <- getLine
  let jogadoresInt = read jogadores :: Int

  putStrLn "Quantos Bots? (1, 2 ou 3)"
  bots <- getLine
  let botsInt = read bots :: Int

  let game = createGameState jogadoresInt botsInt