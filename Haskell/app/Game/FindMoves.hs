module Game.FindMoves where

import Game.Auxiliary
import GameTypes

-- Obtém os movimentos disponíveis para o jogador atual
getAvailableMoves :: GameState -> [(Int, Int)]
getAvailableMoves gameState = do
  let diceRoll = diceRolled gameState
  let playerPieces = getPlayerPieces gameState
  let piecesInStartingArea = filter inStartingArea playerPieces
  let piecesOnBoard = filter (\p -> not (inStartingArea p || finished p || inFinishArea p)) playerPieces
  let piecesInFinishArea = filter (\p -> inFinishArea p && not (finished p)) playerPieces
  let specialTilesInGame = specialTiles gameState
  let gameBlockades = blockades gameState

  -- Se há peças na base e o jogador tirou 6, ele pode sair
  let movesFromStart =
        if diceRoll == 6 && not (null piecesInStartingArea)
          then [(piecePosition p, startingPosByColor (pieceColor p)) | p <- piecesInStartingArea]
          else []

  -- Movimentos no tabuleiro
  let movesOnBoard = getMovesOnBoard piecesOnBoard diceRoll specialTilesInGame gameBlockades
  let movesInFinishArea = getMovesInFinishArea piecesInFinishArea gameBlockades diceRoll
  
  -- Filtra os movimentos para evitar capturas em casas seguras
  filterSafeMoves gameState (movesFromStart ++ movesOnBoard ++ movesInFinishArea)

-- Obtém os movimentos possíveis dentro da área de chegada
getMovesInFinishArea :: [Piece] -> [(Color, Int)] -> Int -> [(Int, Int)]
getMovesInFinishArea pieces blockades diceRoll = do
  let finishAreaMoves =
        map
          ( \p ->
              if (piecePosition p + diceRoll) > getFinishAreaEnd (pieceColor p)
                then (piecePosition p, piecePosition p) -- Peça não se move se ultrapassar o fim
                else (piecePosition p, (piecePosition p) + diceRoll)
          )
          pieces
  let finishAreaMovesNotBlocked = filter (not . isBlocked blockades) finishAreaMoves
  
  -- Remove movimentos onde a peça não sai do lugar
  filter (\(start, end) -> start /= end) finishAreaMovesNotBlocked

-- Obtém os movimentos possíveis no tabuleiro principal
getMovesOnBoard :: [Piece] -> Int -> [SpecialTile] -> [(Color, Int)] -> [(Int, Int)]
getMovesOnBoard pieces diceRoll specialTiles blockades = do
  let piecePositions =
        map (\piece -> (piecePosition piece, (piecePosition piece + diceRoll) `mod` 48)) pieces

  let movesWithoutBlockades = filter (not . isBlocked blockades) piecePositions
  let movesWithTilesApplied = map (applySpecialTile specialTiles) movesWithoutBlockades

  map (\(start, end) -> handleMovesToFinishArea (start, end) diceRoll pieces) movesWithTilesApplied

-- Trata casos onde a peça deve entrar na área de chegada
handleMovesToFinishArea :: (Int, Int) -> Int -> [Piece] -> (Int, Int)
handleMovesToFinishArea (start, end) dice pieces = do
  let piece = head $ filter (\p -> piecePosition p == start) pieces
  if (tilesWalked piece + dice) > 48
    then (start, getFinishAreaStart (pieceColor piece) + (tilesWalked piece + dice - 1) - 48)
    else (start, end)

-- Verifica se um movimento é bloqueado
isBlocked :: [(Color, Int)] -> (Int, Int) -> Bool
isBlocked blockades (startPos, endPos) =
  any (\(_, blockade) -> isBetween startPos endPos blockade) blockades
  where
    isBetween start end blockade
      | start == blockade = False
      | start < end = blockade > start && blockade < end || blockade == end
      | start > end = blockade > start || blockade < end || blockade == end
      | otherwise = blockade == end

-- Aplica efeitos das casas especiais ao movimento
applySpecialTile :: [SpecialTile] -> (Int, Int) -> (Int, Int)
applySpecialTile specialTiles (start, end) =
  case lookup end (map (\t -> (tilePosition t, tileType t)) specialTiles) of
    Just Boost -> (start, (end + 3) `mod` 48) -- Pula 3 casas
    Just Decline -> (start, max 0 (end - 3)) -- Volta 3 casas
    Just Death -> (start, -1) -- Volta para o início
    _ -> (start, end)

-- Encontra peças vulneráveis a captura em jogadas de sorte
findLuckyMoves :: GameState -> [Int]
findLuckyMoves gameState = do
  let gamePieces = pieces gameState
  let currentPlayerColor = currentPlayer gameState

  let availablePieces = filter (\p -> pieceColor p /= currentPlayerColor && not (inStartingArea p || inFinishArea p || finished p)) gamePieces
  let safeTiles = map (\tile -> tilePosition tile) (filter (\tile -> (tileType tile) == Safe) (specialTiles gameState))
  let unsafePieces = filter (\p -> not (piecePosition p `elem` safeTiles)) availablePieces
  map piecePosition unsafePieces

-- Filtra movimentos para evitar capturas em casas seguras
filterSafeMoves :: GameState -> [(Int, Int)] -> [(Int, Int)]
filterSafeMoves gameState moves =
  filter (not . isCapturingOnSafeTile gameState) moves

-- Verifica se um movimento captura uma peça em uma casa segura
isCapturingOnSafeTile :: GameState -> (Int, Int) -> Bool
isCapturingOnSafeTile gameState (from, to) =
  let piecesAtDestination = filter ((== to) . piecePosition) (pieces gameState)
      safeTiles = map tilePosition (filter ((== Safe) . tileType) (specialTiles gameState))
   in any ((`elem` safeTiles) . piecePosition) piecesAtDestination
