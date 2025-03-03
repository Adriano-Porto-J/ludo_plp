module Game.Auxiliary where

import Data.List (maximumBy)
import Data.Ord (comparing)
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
getFinishAreaStart Blue = 54
getFinishAreaStart Green = 60
getFinishAreaStart Yellow = 66

getFinishAreaEnd :: Color -> Int
getFinishAreaEnd Red = 53
getFinishAreaEnd Blue = 59
getFinishAreaEnd Green = 65
getFinishAreaEnd Yellow = 71