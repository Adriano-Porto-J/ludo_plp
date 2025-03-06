module Game.Index where

import Game.Auxiliary
import qualified Game.CreateGame as CreateGame (createGameState)
import qualified Game.FindMoves as FindMoves (findLuckyMoves, getAvailableMoves)
import qualified Game.ProcessMove as ProcessMove (processLuckyMove, processMove)
import Game.LoadSaveState
import GameTypes

-- Cria o estado inicial do jogo com base no número de jogadores e bots
createGameState :: Int -> Int -> GameState
createGameState jogadores bots = CreateGame.createGameState jogadores bots

-- Obtém os movimentos disponíveis para o jogador atual
getAvailableMoves :: GameState -> [(Int, Int)]
getAvailableMoves gameState = FindMoves.getAvailableMoves gameState

-- Obtém as jogadas "de sorte", que podem capturar peças vulneráveis no tabuleiro
getLuckyMoves :: GameState -> [Int]
getLuckyMoves gameState = FindMoves.findLuckyMoves gameState

-- Passa a vez para o próximo jogador
nextPlayer :: GameState -> GameState
nextPlayer gameState =
  let colors = map playerColor (players gameState) -- Lista de cores dos jogadores
      current = currentPlayer gameState -- Jogador atual
      next = getNextPlayer colors current -- Determina o próximo jogador
      save = saveGameState gameState -- Salva o estado do jogo
      winner = if (checkGameOver gameState == True) && (winnerColor gameState == Black) then current else (winnerColor gameState)
   in gameState {currentPlayer = next, sixesInRow = 0, diceRolled = -1, winnerColor = winner} -- Reseta contador de seis seguidos e dado

-- Obtém a cor do próximo jogador na sequência
getNextPlayer :: [Color] -> Color -> Color
getNextPlayer colors current =
  case lookup current (zip colors (tail colors ++ [head colors])) of
    Just next -> next
    Nothing -> head colors -- Caso inesperado, retorna a primeira cor

-- Processa um movimento normal de uma peça
processMove :: GameState -> (Int, Int) -> GameState
processMove gameState jogada =
  ProcessMove.processMove gameState jogada

-- Processa uma jogada de sorte
processLuckyMove :: GameState -> Int -> GameState
processLuckyMove gameState jogada =
  ProcessMove.processLuckyMove gameState jogada

-- Verifica se o jogo acabou, ou seja, se algum jogador já moveu todas as suas peças para a área final
checkGameOver :: GameState -> Bool
checkGameOver gameState = any (\player -> all finished (filter ((== player) . pieceColor) (pieces gameState))) (map playerColor (players gameState))

-- Verifica se é a vez de um bot jogar
isBotTurn :: GameState -> Bool
isBotTurn gameState =
  let current = currentPlayer gameState -- Obtém o jogador atual
   in any ((== current) . playerColor) (filter isBot (players gameState)) -- Verifica se ele é um bot
