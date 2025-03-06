module Game.BotLogic where
import GameTypes
import Game.Auxiliary (getFinishAreaEnd, startingPosByColor)
import Data.List (find, maximumBy)
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)

-- Escolhe o melhor movimento para o bot com base em prioridades: ganhar, capturar ou avançar mais
getBestMove :: GameState -> [(Int, Int)] -> (Int, Int)
getBestMove gameState availableMoves =
  case findWinningMove gameState availableMoves of
    Just move -> move
    Nothing -> case findCaptureMove gameState availableMoves of
      Just move -> move
      Nothing -> case findMostAdvancedMove gameState availableMoves of
        Just move -> move
        Nothing -> pickRandomMove availableMoves

-- Verifica se algum movimento leva diretamente à vitória
findWinningMove :: GameState -> [(Int, Int)] -> Maybe (Int, Int)
findWinningMove gameState moves =
  let winPos = getFinishAreaEnd (currentPlayer gameState)
  in find (\(_, to) -> to == winPos) moves

-- Encontra um movimento que captura uma peça adversária
findCaptureMove :: GameState -> [(Int, Int)] -> Maybe (Int, Int)
findCaptureMove gameState = find (\(_, to) -> capturesOpponent gameState to && not (isSafePosition to))

<<<<<<< HEAD
findMostAdvancedMove :: GameState ->  [(Int, Int)] -> Maybe (Int, Int)
findMostAdvancedMove _ [] = Nothing
findMostAdvancedMove gameState moves =
  let startingPos = startingPosByColor (currentPlayer gameState)
  in Just (maximumBy (compare `on` (\(_, to) -> (to + startingPos) `mod` 48)) moves)
=======
-- Retorna o movimento que leva a peça para a posição mais avançada no tabuleiro
findMostAdvancedMove :: [(Int, Int)] -> Maybe (Int, Int)
findMostAdvancedMove [] = Nothing
findMostAdvancedMove moves = Just (maximumBy (compare `on` snd) moves)
>>>>>>> 6067844680db13f25e1a45856045777b95e2f590

-- Escolhe um movimento aleatório da lista de opções
pickRandomMove :: [(Int, Int)] -> (Int, Int)
pickRandomMove moves = moves !! unsafePerformIO (randomRIO (0, length moves - 1))

-- Verifica se uma posição captura uma peça adversária
capturesOpponent :: GameState -> Int -> Bool
capturesOpponent gameState to =
  any (\p -> piecePosition p == to && pieceColor p /= currentColor) opponentPieces
  where
    currentColor = currentPlayer gameState
    opponentPieces = filter (\p -> pieceColor p /= currentColor) (pieces gameState)

-- Verifica se uma posição é segura (onde peças não podem ser capturadas)
isSafePosition :: Int -> Bool
isSafePosition pos = pos `elem` [0, 7, 12, 20, 24, 28, 33, 37, 46]

-- Escolhe um número da lista de movimentos de sorte do bot
getBotLuckyMove :: [Int] -> Int
getBotLuckyMove luckyMoves = head luckyMoves
