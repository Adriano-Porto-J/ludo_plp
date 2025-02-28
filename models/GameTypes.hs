module models.GameTypes where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.List (elemIndex, find, maximumBy)
import Data.Maybe (fromMaybe, isJust)
import System.IO (readFile, writeFile)
import System.Random (randomRIO)

-- Representação das cores dos jogadores
data Color = Red | Green | Blue | Yellow
  deriving (Show, Eq, Read)

-- Tipos de casas no tabuleiro
data TileType
  = Normal -- Casa comum
  | Safe -- Casa segura (não pode ser capturada)
  | Boost  -- Casa de impulso (avança X casas)
  | Decline  -- Casa de declínio (recua X casas)
  | Death -- Casa da morte (peça volta para a base)
  | Lucky -- Casa da sorte (escolhe uma peça adversária para remover)
  deriving (Show, Eq, Read)

-- Representação de uma casa no tabuleiro
data SpecialTile = SpecialTile
  { tileType :: TileType, -- Tipo da casa
    position :: Int
  }
  deriving (Show, Read)

-- Representação de uma peça
data Piece = Piece
  { corPiece :: Cor, -- Cor da peça
    position :: Int -- Posição atual da peça
  }
  deriving (Show, Eq, Read)

-- Representação de um jogador
data Jogador = Jogador
  { color :: Cor, -- Cor do jogador
    pieces :: [Piece], -- Lista de peças do jogador
    isBot :: Bool -- Indica se o jogador é um bot
  }
  deriving (Show, Read)

-- Estado atual do jogo
data GameState = GameState
  { players :: [Jogador], -- Lista de jogadores
    specialTiles :: [SpecialTile], -- Tabuleiro do jogo
    jogadorAtual :: Cor, -- Cor do jogador atual
    rollDado :: Int, -- Valor do último lançamento do dado
    fim :: Bool, -- Indica se o jogo acabou
    sixesInRow :: [(Cor, Int)] -- Contador de seis seguidos}
  deriving (Show, Read)
