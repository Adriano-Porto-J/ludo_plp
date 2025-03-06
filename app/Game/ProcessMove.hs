module Game.ProcessMove where

import Data.Map (fromListWith, toList)  -- Para manipulação de mapas/dicionários (contagem de posições de peças)
import Game.Auxiliary                    -- Módulo auxiliar para funções adicionais (não mostrado aqui)
import GameTypes                         -- Tipos do jogo, incluindo definição de Peças e Estado do Jogo

-- Função principal que processa o movimento de uma peça no jogo
processMove :: GameState -> (Int, Int) -> GameState
processMove gameState (pieceStart, pieceEnd) = do
  -- Obtém a cor do jogador atual
  let currentPlayerColor = currentPlayer gameState
  -- Obtém a peça pela posição de início e cor
  let piece = getPieceByPositionAndColor (pieces gameState) currentPlayerColor pieceStart
  case piece of
    Just piece ->  -- Caso a peça seja encontrada
      if pieceStart < 0
        then do
          -- Se a posição de início for negativa, move a peça para o tabuleiro
          let newPieces = movePieceToBoard (pieces gameState) piece (currentPlayer gameState)
          let blockades = findBlockades newPieces
          gameState {pieces = newPieces, blockades = blockades, processingMove = False}
        else
          if pieceEnd == -1
            then do
              -- Se a posição final for -1, move a peça para a área de captura (Death)
              let newPieces = movePieceCaptured (pieces gameState) piece
              let blockades = findBlockades newPieces
              gameState {pieces = newPieces, blockades = blockades, processingMove = False}
            else
              if pieceEnd >= 48 && pieceStart < 48
                then do
                  -- Se a peça está indo para a área final
                  let newPieces = movePieceToFinishArea (getPlayerByColor (players gameState) currentPlayerColor) (pieces gameState) piece pieceStart pieceEnd
                  let blockades = findBlockades newPieces
                  gameState {pieces = newPieces, blockades = blockades, processingMove = False}
                else
                  if pieceStart >= 48
                    then do
                      -- Se a peça já está na área final, apenas move dentro da área final
                      let newPieces = movePieceInFinishArea (pieces gameState) piece pieceStart pieceEnd
                      let blockades = findBlockades newPieces
                      gameState {pieces = newPieces, blockades = blockades, processingMove = False}
                    else do
                      -- Caso contrário, move a peça dentro do tabuleiro
                      let newPieces = movePieceInBoard (pieces gameState) piece pieceStart pieceEnd (currentPlayer gameState)
                      let blockades = findBlockades newPieces
                      gameState {pieces = newPieces, blockades = blockades, processingMove = False}
    Nothing -> gameState  -- Caso a peça não seja encontrada, retorna o estado do jogo sem alterações

-- Função que retorna a peça dada sua posição e cor
getPieceByPositionAndColor :: [Piece] -> Color -> Int -> Maybe Piece
getPieceByPositionAndColor pieces color position
  | filterArr == [] = Nothing  -- Se não houver peças na posição, retorna Nothing
  | otherwise = Just (head filterArr)  -- Caso contrário, retorna a primeira peça encontrada
  where
    filterArr = filter (\piece -> piecePosition piece == position && pieceColor piece == color) pieces

-- Função que retorna uma peça por seu ID e cor
getPieceByIdAndColor :: GameState -> Int -> Color -> Piece
getPieceByIdAndColor gameState id color = do
  let pieces = GameTypes.pieces (gameState)
  head (filter (\piece -> pieceId piece == id && pieceColor piece == color) pieces)

-- Função para mover uma peça para o tabuleiro
movePieceToBoard :: [Piece] -> Piece -> Color -> [Piece]
movePieceToBoard pieces piece color = do
  let newPiece = piece {piecePosition = startingPosByColor color, inStartingArea = False, tilesWalked = 0}
  let updatedPieces = removePieceByColorAndPos pieces (pieceColor piece) (piecePosition piece)
  newPiece : updatedPieces

-- Função auxiliar para remover uma peça por cor e posição
removePieceByColorAndPos :: [Piece] -> Color -> Int -> [Piece]
removePieceByColorAndPos pieces color pos = removePieceByColorAndPosAUX pieces color pos False

-- Função recursiva auxiliar para remoção da peça
removePieceByColorAndPosAUX :: [Piece] -> Color -> Int -> Bool -> [Piece]
removePieceByColorAndPosAUX [] _ _ _ = []  -- Se a lista estiver vazia, retorna uma lista vazia
removePieceByColorAndPosAUX (p : ps) color pos wasRemoved
  | pieceColor p == color && piecePosition p == pos && not wasRemoved = removePieceByColorAndPosAUX ps color pos True  -- Remove a primeira peça
  | otherwise = p : removePieceByColorAndPosAUX ps color pos wasRemoved  -- Caso contrário, mantém a peça

-- Função para mover uma peça capturada de volta para a base
movePieceCaptured :: [Piece] -> Piece -> [Piece]
movePieceCaptured pieces piece = do
  let newPiece = piece {piecePosition = -1 * GameTypes.pieceId (piece), inStartingArea = True, tilesWalked = 0}
  let updatedPieces = removePieceByColorAndPos pieces (pieceColor piece) (piecePosition piece)
  newPiece : updatedPieces

-- Função para mover uma peça dentro do tabuleiro
movePieceInBoard :: [Piece] -> Piece -> Int -> Int -> Color -> [Piece]
movePieceInBoard pieces piece startPos endPos color = do
  let positionsWalked = endPos - startPos
  if positionsWalked == -1 || positionsWalked == -2
    then do
      let newPiece = piece {piecePosition = endPos, tilesWalked = tilesWalked piece + positionsWalked}
      let updatedPieces = newPiece : (removePieceByColorAndPos pieces color startPos)
      let capturedPieces = filter (\p -> piecePosition p == endPos && pieceColor p /= color) updatedPieces
      if length capturedPieces > 0
        then do
          -- Se uma peça for capturada, move ela para a base
          let capturedPiece = head capturedPieces
          movePieceCaptured updatedPieces capturedPiece
        else updatedPieces
    else do
      let newPiece = piece {piecePosition = endPos, tilesWalked = tilesWalked piece + (positionsWalked + 48) `mod` 48}
      let updatedPieces = newPiece : (removePieceByColorAndPos pieces color startPos)
      let capturedPieces = filter (\p -> piecePosition p == endPos && pieceColor p /= color) updatedPieces
      if length capturedPieces > 0
        then do
          let capturedPiece = head capturedPieces
          movePieceCaptured updatedPieces capturedPiece
        else updatedPieces

-- Função para encontrar posições bloqueadas por peças duplicadas
findBlockades :: [Piece] -> [(Color, Int)]
findBlockades pieces = do
  -- Cria um mapa (dicionário) que conta quantas vezes cada posição aparece na lista de peças
  let positionCounts = fromListWith (+) [(piecePosition p, 1) | p <- pieces]

  -- Filtra o mapa para encontrar apenas as posições que aparecem duas ou mais vezes
  let duplicatePositions = filter (\(pos, count) -> count >= 2 && pos >= 0) (toList positionCounts)
  let filterPositions = filter (\(pos, _) -> not (pos `elem` [53, 59, 65, 71])) duplicatePositions
  -- Cria uma lista de tuplas contendo a cor e a posição das peças que estão em posições duplicadas
  map (\(pos, _) -> (getPieceColorInPosition pieces pos, pos)) filterPositions

-- Função que retorna a cor da peça na posição dada
getPieceColorInPosition :: [Piece] -> Int -> Color
getPieceColorInPosition pieces position = pieceColor $ head $ filter (\piece -> piecePosition piece == position) pieces

-- Função para mover uma peça para a área final
movePieceToFinishArea :: Player -> [Piece] -> Piece -> Int -> Int -> [Piece]
movePieceToFinishArea player pieces piece startPos endPos = do
  let newPiece = piece {piecePosition = endPos, tilesWalked = endPos - (startingPos player), inFinishArea = True}
  let updatedPieces = newPiece : (removePieceByColorAndPos pieces (pieceColor piece) startPos)
  updatedPieces

-- Função para mover uma peça dentro da área final
movePieceInFinishArea :: [Piece] -> Piece -> Int -> Int -> [Piece]
movePieceInFinishArea pieces piece startPos endPos = do
  if endPos == getFinishAreaEnd (pieceColor piece)
    then do
      -- Se a peça atingiu o fim da área final, marca como terminada
      let newPiece = piece {piecePosition = endPos, finished = True}
      let updatedPieces = newPiece : (removePieceByColorAndPos pieces (pieceColor piece) startPos)
      updatedPieces
    else do
      -- Caso contrário, move a peça no tabuleiro
      movePieceInBoard pieces piece startPos endPos (pieceColor piece)

-- Função para processar o movimento de uma peça "sortuda" (que deve ser capturada)
processLuckyMove :: GameState -> Int -> GameState
processLuckyMove gameState pieceToKill = do
  let piece = head $ filter (\p -> (piecePosition p) == pieceToKill) (pieces gameState)
  let newPiece = piece {piecePosition = -1 * (pieceId piece), inStartingArea = True, tilesWalked = 0}
  let updatedPieces = newPiece : (removePieceByColorAndPos (pieces gameState) (pieceColor piece) (piecePosition piece))
  gameState {pieces = updatedPieces}
