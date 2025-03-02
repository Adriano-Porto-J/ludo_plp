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
    else do
      let newPieces = movePieceInBoard (pieces gameState) piece pieceStart pieceEnd (currentPlayer gameState)
      gameState {pieces = newPieces}

getPieceByPositionAndColor :: [Piece] -> Color -> Int -> Piece
getPieceByPositionAndColor pieces color position = head $ filter (\piece -> piecePosition piece == position && pieceColor piece == color) pieces

-- Move uma peça para o tabuleiro
movePieceToBoard :: [Piece] -> Piece -> Color -> [Piece]
movePieceToBoard pieces piece color = do
  let newPiece = piece {piecePosition = startingPosByColor color, inStartingArea = False, tilesWalked = 0}
  let updatedPieces = removePieceByColorAndPos pieces (pieceColor piece) (piecePosition piece)
  newPiece : updatedPieces

-- Remove a primeira peça por cor e posição
removePieceByColorAndPos :: [Piece] -> Color -> Int -> [Piece]
removePieceByColorAndPos pieces color pos = removePieceByColorAndPosAUX pieces color pos False

removePieceByColorAndPosAUX :: [Piece] -> Color -> Int -> Bool -> [Piece]
removePieceByColorAndPosAUX [] _ _ _ = []
removePieceByColorAndPosAUX (p : ps) color pos wasRemoved
  | pieceColor p == color && piecePosition p == pos && not wasRemoved = removePieceByColorAndPosAUX ps color pos True
  | otherwise = p : removePieceByColorAndPosAUX ps color pos wasRemoved

-- Move uma peça capturada de volta para a base
-- Conjunto de peças, peça capturada -> conjunto de peças atualizado
movePieceCaptured :: [Piece] -> Piece -> [Piece]
movePieceCaptured pieces piece = do
  let newPiece = piece {piecePosition = -1, inStartingArea = True, tilesWalked = 0}
  let updatedPieces = removePieceByColorAndPos pieces (pieceColor piece) (piecePosition piece)
  newPiece : updatedPieces

-- Move uma peça no tabuleiro
movePieceInBoard :: [Piece] -> Piece -> Int -> Int -> Color -> [Piece]
movePieceInBoard pieces piece startPos endPos color = do
  let newPiece = piece {piecePosition = endPos, tilesWalked = tilesWalked piece + (endPos - startPos) `mod` 52}
  let updatedPieces = newPiece : (removePieceByColorAndPos pieces color startPos)
  let capturedPieces = filter (\p -> piecePosition p == endPos && pieceColor p /= color) updatedPieces
  if length capturedPieces > 0
    then do
      let capturedPiece = head capturedPieces
      movePieceCaptured updatedPieces capturedPiece
    else updatedPieces
