module Game.ProcessMove where

import Game.Auxiliary
import GameTypes

processMove :: GameState -> (Int, Int) -> GameState
processMove gameState (pieceStart, pieceEnd) = do
  let currentPlayerColor = currentPlayer gameState

  let piece = getPieceByPositionAndColor (pieces gameState) currentPlayerColor pieceStart

  if pieceStart == -1
    then do
      let newPieces = movePieceToBoard (pieces gameState) piece (currentPlayer gameState)
      gameState {pieces = newPieces}
    else -- To Be Implemented
      gameState

getPieceByPositionAndColor :: [Piece] -> Color -> Int -> Piece
getPieceByPositionAndColor pieces color position = head $ filter (\piece -> piecePosition piece == position && pieceColor piece == color) pieces

-- Move uma peça para o tabuleiro
movePieceToBoard :: [Piece] -> Piece -> Color -> [Piece]
movePieceToBoard pieces piece color = do
  let newPiece = piece {piecePosition = startingPosByColor color, inStartingArea = False, tilesWalked = 0}
  let updatedPieces = removePieceByColorAndPos pieces (pieceColor piece) (piecePosition piece) False
  newPiece : updatedPieces

-- Remove a primeira peça por cor e posição
removePieceByColorAndPos :: [Piece] -> Color -> Int -> Bool -> [Piece]
removePieceByColorAndPos [] _ _ _ = []
removePieceByColorAndPos (p : ps) color pos wasRemoved
  | pieceColor p == color && piecePosition p == pos && not wasRemoved = removePieceByColorAndPos ps color pos True
  | otherwise = p : removePieceByColorAndPos ps color pos wasRemoved
