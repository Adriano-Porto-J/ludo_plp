module Game.FindMoves where

import Game.Auxiliary
import GameTypes

getAvailableMoves :: GameState -> [(Int, Int)]
getAvailableMoves gameState = do
  let diceRoll = diceRolled gameState
  let playerPieces = getPlayerPieces gameState
  let piecesInStartingArea = filter inStartingArea playerPieces
  let piecesOnBoard = filter (\p -> not (inStartingArea p || finished p || inFinishArea p)) playerPieces
  let piecesInFinishArea = filter (\p -> inFinishArea p && not (finished p)) playerPieces
  let specialTilesInGame = specialTiles gameState
  let gameBlockades = blockades gameState

  -- Se há peças na base e o jogador tirou 5, ele pode sair
  let movesFromStart =
        if diceRoll == 5 && not (null piecesInStartingArea)
          then [(piecePosition p, startingPosByColor (pieceColor p)) | p <- piecesInStartingArea]
          else []

  -- Movimentos no tabuleiro
  let movesOnBoard = getMovesOnBoard piecesOnBoard diceRoll specialTilesInGame gameBlockades
  let movesInFinishArea = getMovesInFinishArea piecesInFinishArea gameBlockades diceRoll
  movesFromStart ++ movesOnBoard ++ movesInFinishArea

getMovesInFinishArea :: [Piece] -> [(Color, Int)] -> Int -> [(Int, Int)]
getMovesInFinishArea pieces blockades diceRoll = do
  let finishAreaMoves =
        map
          ( \p ->
              if (piecePosition p + diceRoll) > getFinishAreaEnd (pieceColor p)
                then (piecePosition p, piecePosition p)
                else (piecePosition p, (piecePosition p) + diceRoll)
          )
          pieces
  let finishAreaMovesNotBlocked = filter (not . isBlocked blockades) finishAreaMoves
  filter (\(start, end) -> start /= end) finishAreaMovesNotBlocked

getMovesOnBoard :: [Piece] -> Int -> [SpecialTile] -> [(Color, Int)] -> [(Int, Int)]
getMovesOnBoard pieces diceRoll specialTiles blockades = do
  let piecePositions =
        map (\piece -> (piecePosition piece, (piecePosition piece + diceRoll) `mod` 52)) pieces

  let movesWithoutBlockades = filter (not . isBlocked blockades) piecePositions
  let movesWithTilesApplied = map (applySpecialTile specialTiles) movesWithoutBlockades

  map (\(start, end) -> handleMovesToFinishArea (start, end) diceRoll pieces) movesWithTilesApplied

handleMovesToFinishArea :: (Int, Int) -> Int -> [Piece] -> (Int, Int)
handleMovesToFinishArea (start, end) dice pieces = do
  let piece = head $ filter (\p -> piecePosition p == start) pieces
  if (tilesWalked piece + dice) > 52
    then (start, getFinishAreaStart (pieceColor piece) + (tilesWalked piece + dice - 1) - 52)
    else (start, end)

isBlocked :: [(Color, Int)] -> (Int, Int) -> Bool
isBlocked blockades (startPos, endPos) =
  any (\(_, blockade) -> (startPos < blockade && endPos > blockade) || (startPos > blockade && endPos < blockade)) blockades

applySpecialTile :: [SpecialTile] -> (Int, Int) -> (Int, Int)
applySpecialTile specialTiles (start, end) =
  case lookup end (map (\t -> (tilePosition t, tileType t)) specialTiles) of
    Just Boost -> (start, (end + 3) `mod` 52) -- Pula 3 casas
    Just Decline -> (start, max 0 (end - 3)) -- Volta 3 casas
    Just Death -> (start, -1) -- Volta para o início
    _ -> (start, end)
