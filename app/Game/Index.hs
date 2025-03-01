module Game.Index where

import Game.Auxiliary
import qualified Game.CreateGame as CreateGame (createGameState)
import qualified Game.FindMoves as FindMoves (getAvailableMoves)
import GameTypes

createGameState :: Int -> Int -> GameState
createGameState jogadores bots =
  CreateGame.createGameState jogadores bots

getAvailableMoves :: GameState -> [(Int, Int)]
getAvailableMoves gameState =
  FindMoves.getAvailableMoves gameState

handleSixesInRow :: GameState -> GameState
handleSixesInRow gameState = do
  let diceRoll = diceRolled gameState
  if diceRoll == 6
    then do
      if sixesInRow gameState == 2
        then do
          let playerPieces = getPlayerPieces gameState
          let pieceWithMostTilesWalked = getPieceWithMostTilesWalked playerPieces
          let newPiece = pieceWithMostTilesWalked {tilesWalked = 0, inStartingArea = True, piecePosition = -1}
          let newPieces = map (\piece -> if piece == pieceWithMostTilesWalked then newPiece else piece) (pieces gameState)
          gameState {pieces = newPieces, sixesInRow = 0}
        else gameState {sixesInRow = sixesInRow gameState + 1}
    else gameState {sixesInRow = 0}