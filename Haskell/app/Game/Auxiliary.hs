module Game.Auxiliary where

import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified System.Console.ANSI as ANSI
import GameTypes

-- Retorna todas as peças do jogador atual no tabuleiro
getPlayerPieces :: GameState -> [Piece]
getPlayerPieces gameState = filter ((== currentPlayer gameState) . pieceColor) (pieces gameState)

-- Retorna um jogador específico com base na cor
getPlayerByColor :: [Player] -> Color -> Player
getPlayerByColor players color = head $ filter ((== color) . playerColor) players

-- Encontra a peça que andou mais casas, ignorando as que estão na área final ou já finalizaram
getPieceWithMostTilesWalked :: [Piece] -> Maybe Piece
getPieceWithMostTilesWalked pieces =
  let piecesNotFinished = filter (\piece -> not (inFinishArea piece) && not (finished piece)) pieces
   in if null piecesNotFinished
        then Nothing
        else Just (maximumBy (comparing tilesWalked) piecesNotFinished)

-- Retorna a posição inicial no tabuleiro para cada cor
startingPosByColor :: Color -> Int
startingPosByColor Red = 0
startingPosByColor Yellow = 12
startingPosByColor Blue = 24
startingPosByColor Green = 36

-- Retorna a posição de início da área final para cada cor
getFinishAreaStart :: Color -> Int
getFinishAreaStart Red = 48
getFinishAreaStart Yellow = 54
getFinishAreaStart Blue = 60
getFinishAreaStart Green = 66

-- Retorna a posição final da área final para cada cor
getFinishAreaEnd :: Color -> Int
getFinishAreaEnd Red = 53
getFinishAreaEnd Yellow = 59
getFinishAreaEnd Blue = 65
getFinishAreaEnd Green = 71

-- Exibe o estado atual do jogo de forma formatada
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
  putStrLn $ "Foi Jogada Lucky? " ++ show (wasLuckyMove gameState)
  putStrLn $ "Vencedor: " ++ show (winnerColor gameState)
  putStrLn $ "Terminou? " ++ show (end gameState)
  putStrLn "---------------------------"

-- Exibe todas as peças no tabuleiro
printPieces :: [Piece] -> IO ()
printPieces pieces = mapM_ printPiece pieces

-- Exibe uma peça específica com cor correspondente
printPiece :: Piece -> IO ()
printPiece piece = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (playerColorToANSI (pieceColor piece))]
  putStrLn $ "Peça: " ++ show piece
  ANSI.setSGR [ANSI.Reset]

-- Exibe as informações de um jogador, incluindo se é bot e sua posição inicial
printPlayer :: Player -> IO ()
printPlayer player = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (playerColorToANSI (playerColor player))]
  putStrLn $ "Jogador: " ++ show (playerColor player)
  putStrLn $ "Bot: " ++ show (isBot player)
  putStrLn $ "Posição inicial: " ++ show (startingPos player)
  ANSI.setSGR [ANSI.Reset]

-- Exibe um movimento possível no formato "De X para Y"
printMove :: Int -> (Int, Int) -> IO ()
printMove index (from, to) = putStrLn $ show index ++ ". De " ++ show from ++ " para " ++ show to

-- Exibe um oponente disponível para captura
printOponnent :: Int -> Int -> IO ()
printOponnent index oponent = putStrLn $ show index ++ ". Oponente: " ++ show oponent

-- Converte a cor de um jogador para a cor correspondente no terminal
playerColorToANSI :: Color -> ANSI.Color
playerColorToANSI Red = ANSI.Red
playerColorToANSI Blue = ANSI.Blue
playerColorToANSI Green = ANSI.Green
playerColorToANSI Yellow = ANSI.Yellow
