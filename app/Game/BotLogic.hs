module Game.BotLogic where
import GameTypes
import Game.Auxiliary (getFinishAreaEnd)
import Data.List (find, maximumBy)
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)

getBestMove :: GameState -> [(Int, Int)] -> (Int, Int)
getBestMove gameState availableMoves =
  case findWinningMove gameState availableMoves of
    Just move -> move
    Nothing -> case findCaptureMove gameState availableMoves of
      Just move -> move
      Nothing -> case findMostAdvancedMove availableMoves of
        Just move -> move
        Nothing -> pickRandomMove availableMoves

findWinningMove :: GameState -> [(Int, Int)] -> Maybe (Int, Int)
findWinningMove gameState moves =
  let winPos = getFinishAreaEnd (currentPlayer gameState)
  in find (\(_, to) -> to == winPos) moves

findCaptureMove :: GameState -> [(Int, Int)] -> Maybe (Int, Int)
findCaptureMove gameState = find (\(_, to) -> capturesOpponent gameState to && not (isSafePosition to))

findMostAdvancedMove :: [(Int, Int)] -> Maybe (Int, Int)
findMostAdvancedMove [] = Nothing
findMostAdvancedMove moves = Just (maximumBy (compare `on` snd) moves)

pickRandomMove :: [(Int, Int)] -> (Int, Int)
pickRandomMove moves = moves !! unsafePerformIO (randomRIO (0, length moves - 1))

capturesOpponent :: GameState -> Int -> Bool
capturesOpponent gameState to =
  any (\p -> piecePosition p == to && pieceColor p /= currentColor) opponentPieces
  where
    currentColor = currentPlayer gameState
    opponentPieces = filter (\p -> pieceColor p /= currentColor) (pieces gameState)

isSafePosition :: Int -> Bool
isSafePosition pos = pos `elem` [0, 7, 12, 20, 24, 28, 33, 37, 46]

getBotLuckyMove :: [Int] -> Int
getBotLuckyMove luckyMoves = head luckyMoves