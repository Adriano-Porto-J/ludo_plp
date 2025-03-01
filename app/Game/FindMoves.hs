module Game.FindMoves where

import Game.Auxiliary
import GameTypes

-- getAvailableMoves :: GameState -> Int -> [(Int, Int)]
-- getAvailableMoves gameState diceRoll = do
--   let currentColor = currentPlayer gameState

getAvailableMoves :: GameState -> [(Int, Int)]
getAvailableMoves gameState = do
  let diceRoll = diceRolled gameState
  let playerPieces = getPlayerPieces gameState
  let piecesInStartingArea = filter (\piece -> inStartingArea piece == True) playerPieces

  if length piecesInStartingArea > 0 && diceRoll == 5
    then (-1, 0) : getMovesInBoard gameState playerPieces diceRoll
    else getMovesInBoard gameState playerPieces diceRoll

getMovesInBoard :: GameState -> [Piece] -> Int -> [(Int, Int)]
getMovesInBoard gameState playerPieces diceRoll = do
  let playerPieces = filter (\piece -> inStartingArea piece == False && finished piece == False && inFinishArea piece == False) playerPieces
  let piecePositions = map (\piece -> (piecePosition piece, (piecePosition piece + diceRoll) `mod` 52)) playerPieces

  let gameBlockades = blockades gameState

  checkBlockades gameBlockades piecePositions

checkBlockades :: [(Color, Int)] -> [(Int, Int)] -> [(Int, Int)]
checkBlockades blockades moves = do
  let isBetweenBlockades (startPos, endPos) =
        any
          ( \(_, blockade) ->
              (startPos < blockade && endPos > blockade)
                || (startPos > blockade && endPos < blockade)
          )
          blockades

  filter (not . isBetweenBlockades) moves