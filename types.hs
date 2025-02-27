module Ludo where

import System.Random (randomRIO)
import Data.List (find, elemIndex, maximumBy)
import Data.Maybe (fromMaybe, isJust)
import System.IO (writeFile, readFile)
import Control.Monad (when)
import Control.Exception (try, IOException)

-- Representação das cores dos jogadores
data Cor = Vermelho | Verde | Amarelo | Azul
  deriving (Show, Eq, Read)

-- Tipos de casas no tabuleiro
data TipoCasa = Normal       -- Casa comum
              | Safe         -- Casa segura (não pode ser capturada)
              | Boost Int    -- Casa de impulso (avança X casas)
              | Decline Int  -- Casa de declínio (recua X casas)
              | Death       -- Casa da morte (peça volta para a base)
              | Lucky        -- Casa da sorte (escolhe uma peça adversária para remover)
  deriving (Show, Eq, Read)

-- Representação de uma casa no tabuleiro
data Casa = Casa
  { tipoCasa :: TipoCasa  -- Tipo da casa
  , ocupadoPor :: [Peca]  -- Peças que ocupam a casa
  } deriving (Show, Read)

-- Representação de uma peça
data Peca = Peca
  { corPeca :: Cor  -- Cor da peça
  , posicao :: Int  -- Posição atual da peça
  } deriving (Show, Eq, Read)

-- Representação de um jogador
data Jogador = Jogador
  { corJogador :: Cor  -- Cor do jogador
  , pecas :: [Peca]    -- Lista de peças do jogador
  , isBot :: Bool      -- Indica se o jogador é um bot
  } deriving (Show, Read)

-- Estado atual do jogo
data GameState = GameState
  { jogadores :: [Jogador]  -- Lista de jogadores
  , tabuleiro :: [Casa]     -- Tabuleiro do jogo
  , currentTurn :: Cor      -- Cor do jogador atual
  , rollDado :: Int         -- Valor do último lançamento do dado
  , gameOver :: Bool        -- Indica se o jogo terminou
  } deriving (Show, Read)
