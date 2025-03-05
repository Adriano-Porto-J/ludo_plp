module Game.Auxiliary where

import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified System.Console.ANSI as ANSI
import GameTypes

getPlayerPieces :: GameState -> [Piece]
getPlayerPieces gameState = filter ((== currentPlayer gameState) . pieceColor) (pieces gameState)

getPlayerByColor :: [Player] -> Color -> Player
getPlayerByColor players color = head $ filter ((== color) . playerColor) players

getPieceWithMostTilesWalked :: [Piece] -> Maybe Piece
getPieceWithMostTilesWalked pieces =
  let piecesNotFinished = filter (\piece -> not (inFinishArea piece) && not (finished piece)) pieces
   in if null piecesNotFinished
        then Nothing
        else Just (maximumBy (comparing tilesWalked) piecesNotFinished)

startingPosByColor :: Color -> Int
startingPosByColor Red = 0
startingPosByColor Yellow = 12
startingPosByColor Blue = 24
startingPosByColor Green = 36

getFinishAreaStart :: Color -> Int
getFinishAreaStart Red = 48
getFinishAreaStart Yellow = 54
getFinishAreaStart Blue = 60
getFinishAreaStart Green = 66

getFinishAreaEnd :: Color -> Int
getFinishAreaEnd Red = 53
getFinishAreaEnd Yellow = 59
getFinishAreaEnd Blue = 65
getFinishAreaEnd Green = 71

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
  putStrLn $ "Foi Jogada Lucky? " ++ show (wasLuckyMove gameState)
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

playerColorToANSI :: Color -> ANSI.Color
playerColorToANSI Red = ANSI.Red
playerColorToANSI Blue = ANSI.Blue
playerColorToANSI Green = ANSI.Green
playerColorToANSI Yellow = ANSI.Yellow
