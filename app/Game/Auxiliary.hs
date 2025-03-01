module Game.Auxiliary where

import Data.List (maximumBy)
import Data.Ord (comparing)
import GameTypes

getPlayerPieces :: GameState -> [Piece]
getPlayerPieces gameState = filter (\piece -> pieceColor piece == currentPlayer gameState) (pieces gameState)

getPieceWithMostTilesWalked :: [Piece] -> Piece
getPieceWithMostTilesWalked pieces = do
  let piecesNotFinished = filter (\piece -> inFinishArea piece == False && finished piece == False) pieces
  maximumBy (comparing tilesWalked) piecesNotFinished