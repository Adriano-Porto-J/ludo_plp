module Game.Auxiliary where

import Data.List (maximumBy)
import Data.Ord (comparing)
import GameTypes

getPlayerPieces :: GameState -> [Piece]
getPlayerPieces gameState = filter ((== currentPlayer gameState) . pieceColor) (pieces gameState)

getPieceWithMostTilesWalked :: [Piece] -> Maybe Piece
getPieceWithMostTilesWalked pieces = 
  let piecesNotFinished = filter (\piece -> not (inFinishArea piece) && not (finished piece)) pieces
  in if null piecesNotFinished 
       then Nothing 
       else Just (maximumBy (comparing tilesWalked) piecesNotFinished)

startingPosByColor :: Color -> Int
startingPosByColor Red    = 0
startingPosByColor Blue   = 13
startingPosByColor Green  = 26
startingPosByColor Yellow = 39
