module Game.Index where

import Game.Auxiliary
import qualified Game.CreateGame as CreateGame (createGameState)
import qualified Game.FindMoves as FindMoves (findLuckyMoves, getAvailableMoves)
import qualified Game.ProcessMove as ProcessMove (processLuckyMove, processMove)
import GameTypes

createGameState :: Int -> Int -> GameState
createGameState jogadores bots = CreateGame.createGameState jogadores bots

getAvailableMoves :: GameState -> [(Int, Int)]
getAvailableMoves gameState = FindMoves.getAvailableMoves gameState

getLuckyMoves :: GameState -> [Int]
getLuckyMoves gameState = FindMoves.findLuckyMoves gameState

handleSixesInRow :: GameState -> GameState
handleSixesInRow gameState
  | diceRoll == 6 =
      if sixesInRow gameState == 2
        then case getPieceWithMostTilesWalked (getPlayerPieces gameState) of
          Just pieceWithMostTiles ->
            let newPiece = pieceWithMostTiles {tilesWalked = 0, inStartingArea = True, piecePosition = -1}
                newPieces = map (\piece -> if piece == pieceWithMostTiles then newPiece else piece) (pieces gameState)
             in gameState {pieces = newPieces, sixesInRow = 0}
          Nothing -> gameState {sixesInRow = 0} -- Nenhuma peça para remover, apenas reseta
        else gameState {sixesInRow = sixesInRow gameState + 1}
  | otherwise = gameState {sixesInRow = 0}
  where
    diceRoll = diceRolled gameState

nextPlayer :: GameState -> GameState
nextPlayer gameState =
  let colors = map playerColor (players gameState)
      current = currentPlayer gameState
      next = getNextPlayer colors current
   in gameState {currentPlayer = next, sixesInRow = 0} -- Reseta contador de seis seguidos

getNextPlayer :: [Color] -> Color -> Color
getNextPlayer colors current =
  case lookup current (zip colors (tail colors ++ [head colors])) of
    Just next -> next
    Nothing -> head colors -- Caso inesperado

processMove :: GameState -> (Int, Int) -> GameState
processMove gameState jogada =
  ProcessMove.processMove gameState jogada

processLuckyMove :: GameState -> Int -> GameState
processLuckyMove gameState jogada =
  ProcessMove.processLuckyMove gameState jogada

checkGameOver :: GameState -> Bool
checkGameOver gameState = any (\player -> all finished (filter ((== player) . pieceColor) (pieces gameState))) (map playerColor (players gameState))

filterSafeMoves :: GameState -> [(Int, Int)] -> [(Int, Int)]
filterSafeMoves gameState moves =
  filter (not . isCapturingOnSafeTile gameState) moves

isCapturingOnSafeTile :: GameState -> (Int, Int) -> Bool
isCapturingOnSafeTile gameState (from, to) =
  let piecesAtDestination = filter ((== to) . piecePosition) (pieces gameState)
      safeTiles = map tilePosition (filter ((== Safe) . tileType) (specialTiles gameState))
  in any ((`elem` safeTiles) . piecePosition) piecesAtDestination