module Game.ProcessMove where

import Data.Map (fromListWith, toList)
import Game.Auxiliary
import GameTypes

processMove :: GameState -> (Int, Int) -> GameState
processMove gameState (pieceStart, pieceEnd) = do
  let currentPlayerColor = currentPlayer gameState
  let piece = getPieceByPositionAndColor (pieces gameState) currentPlayerColor pieceStart

  if pieceStart == -1 -- Mover peça para o tabuleiro
    then do
      let newPieces = movePieceToBoard (pieces gameState) piece (currentPlayer gameState)
      let blockades = findBlockades newPieces
      gameState {pieces = newPieces, blockades = blockades}
    else
      if pieceEnd == -1 -- Mover peça na casa Death
        then do
          let newPieces = movePieceCaptured (pieces gameState) piece

          let blockades = findBlockades newPieces
          gameState {pieces = newPieces, blockades = blockades}
        else do
          -- Mover Peça no tabuleiro
          let newPieces = movePieceInBoard (pieces gameState) piece pieceStart pieceEnd (currentPlayer gameState)
          let blockades = findBlockades newPieces
          gameState {pieces = newPieces, blockades = blockades}

 

isLuckyTile :: Int -> Bool
isLuckyTile pos = pos == 41 

getOpponentPieces :: [Piece] -> Color -> [(Color, Int)]
getOpponentPieces pieces currentColor = [(pieceColor p, piecePosition p) | p <- pieces, pieceColor p /= currentColor, piecePosition p /= -1]

chooseOpponentPieceToEliminate :: [(Color, Int)] -> Int
chooseOpponentPieceToEliminate opponentPieces = snd (head opponentPieces) 

applyDeath :: [Piece] -> Int -> [Piece]
applyDeath pieces pos = [p | p <- pieces, piecePosition p /= pos]

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

findBlockades :: [Piece] -> [(Color, Int)]
findBlockades pieces = do
  -- Cria um mapa (dicionário) que conta quantas vezes cada posição aparece na lista de peças
  let positionCounts = fromListWith (+) [(piecePosition p, 1) | p <- pieces]

  -- Filtra o mapa para encontrar apenas as posições que aparecem duas ou mais vezes
  let duplicatePositions = filter (\(pos, count) -> count >= 2 && pos /= -1) (toList positionCounts)

  -- Cria uma lista de tuplas contendo a cor e a posição das peças que estão em posições duplicadas
  map (\(pos, _) -> (getPieceColorInPosition pieces pos, pos)) duplicatePositions

-- Retorna a lista de posições duplicadas com suas respectivas cores

getPieceColorInPosition :: [Piece] -> Int -> Color
getPieceColorInPosition pieces position = pieceColor $ head $ filter (\piece -> piecePosition piece == position) pieces