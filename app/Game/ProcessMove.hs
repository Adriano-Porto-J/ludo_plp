module Game.ProcessMove where

import Data.Map (fromListWith, toList)
import Game.Auxiliary
import GameTypes

processMove :: GameState -> (Int, Int) -> GameState
processMove gameState (pieceStart, pieceEnd) = do
  case piece of
    Just piece -> do
      let walked = tilesWalked piece
      if pieceStart < 0
        then do
          -- Mover peça para o tabuleiro
          let newPieces = movePieceToBoard (pieces gameState) piece (currentPlayer gameState)
          let blockades = findBlockades newPieces
          gameState {pieces = newPieces, blockades = blockades, processingMove = False}
        else
          if pieceEnd == -1
            then do
              -- Mover peça na casa Death
              let newPieces = movePieceCaptured (pieces gameState) piece
              let blockades = findBlockades newPieces
              gameState {pieces = newPieces, blockades = blockades, processingMove = False}
            else
              if walked + (pieceEnd + piecePosition (piece)) > 46
                then do
                  if (inFinishArea piece) == True
                    then do
                      -- Move Peça na área final
                      let newPieces = movePieceInFinishArea (pieces gameState) piece pieceStart (pieceEnd + (piecePosition piece))
                      let blockades = findBlockades newPieces
                      gameState {pieces = newPieces, blockades = blockades, processingMove = False}
                    else do
                      -- Move Peça para a área final
                      let newPieces = movePieceToFinishArea (getPlayerByColor (players gameState) currentPlayerColor) (pieces gameState) piece pieceStart (pieceEnd + (piecePosition piece))
                      let blockades = findBlockades newPieces
                      gameState {pieces = newPieces, blockades = blockades, processingMove = False}
                else do
                  -- Mover Peça no tabuleiro
                  let newPieces = movePieceInBoard (pieces gameState) piece pieceStart (pieceEnd + (piecePosition piece)) (currentPlayer gameState)
                  let blockades = findBlockades newPieces
                  gameState {pieces = newPieces, blockades = blockades, processingMove = False}
    Nothing -> gameState
  where
    currentPlayerColor = currentPlayer gameState
    piece = getPieceByPositionAndColor (pieces gameState) currentPlayerColor pieceStart

getPieceByPositionAndColor :: [Piece] -> Color -> Int -> Maybe Piece
getPieceByPositionAndColor pieces color position
  | filterArr == [] = Nothing
  | otherwise = Just (head filterArr)
  where
    filterArr = filter (\piece -> piecePosition piece == position && pieceColor piece == color) pieces

getPieceByIdAndColor :: GameState -> Int -> Color -> Piece
getPieceByIdAndColor gameState id color = do
  let pieces = GameTypes.pieces (gameState)
  head (filter (\piece -> pieceId piece == id && pieceColor piece == color) pieces)

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
  let newPiece = piece {piecePosition = -1 * GameTypes.pieceId (piece), inStartingArea = True, tilesWalked = 0}
  let updatedPieces = removePieceByColorAndPos pieces (pieceColor piece) (piecePosition piece)
  newPiece : updatedPieces

-- Move uma peça no tabuleiro
movePieceInBoard :: [Piece] -> Piece -> Int -> Int -> Color -> [Piece]
movePieceInBoard pieces piece startPos endPos color = do
  let newPiece = piece {piecePosition = endPos `mod` 48, tilesWalked = piecePosition (piece) - endPos}
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
  let duplicatePositions = filter (\(pos, count) -> count >= 2 && pos >= 0) (toList positionCounts)

  -- Cria uma lista de tuplas contendo a cor e a posição das peças que estão em posições duplicadas
  map (\(pos, _) -> (getPieceColorInPosition pieces pos, pos)) duplicatePositions

-- Retorna a lista de posições duplicadas com suas respectivas cores

getPieceColorInPosition :: [Piece] -> Int -> Color
getPieceColorInPosition pieces position = pieceColor $ head $ filter (\piece -> piecePosition piece == position) pieces

movePieceToFinishArea :: Player -> [Piece] -> Piece -> Int -> Int -> [Piece]
movePieceToFinishArea player pieces piece startPos endPos = do
  let newPiece = piece {piecePosition = finishAreaStart + ((piecePosition (piece)) - finishAreaStart), inFinishArea = True}
  let updatedPieces = newPiece : (removePieceByColorAndPos pieces (pieceColor piece) startPos)
  updatedPieces
  where finishAreaStart = getFinishAreaStart (pieceColor (piece))
movePieceInFinishArea :: [Piece] -> Piece -> Int -> Int -> [Piece]
movePieceInFinishArea pieces piece startPos endPos = do
  if endPos == getFinishAreaEnd (pieceColor piece)
    then do
      let newPiece = piece {piecePosition = endPos, finished = True}
      let updatedPieces = newPiece : (removePieceByColorAndPos pieces (pieceColor piece) startPos)
      updatedPieces
    else do
      movePieceInBoard pieces piece startPos endPos (pieceColor piece)

processLuckyMove :: GameState -> Int -> GameState
processLuckyMove gameState pieceToKill = do
  let piece = head $ filter (\p -> (piecePosition p) == pieceToKill) (pieces gameState)
  let newPiece = piece {piecePosition = -1 * GameTypes.pieceId (piece), inStartingArea = True, tilesWalked = 0}
  let updatedPieces = newPiece : (removePieceByColorAndPos (pieces gameState) (pieceColor piece) (piecePosition piece))
  gameState {pieces = updatedPieces}
