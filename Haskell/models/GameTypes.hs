{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module GameTypes where
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Representação das cores dos jogadores
data Color = Red | Green | Blue | Yellow | Black
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- Tipos de casas no tabuleiro
data TileType
  = Normal -- Casa comum
  | Safe -- Casa segura (não pode ser capturada)
  | Boost -- Casa de impulso (avança X casas)
  | Decline -- Casa de declínio (recua X casas)
  | Death -- Casa da morte (peça volta para a base)
  | Lucky -- Casa da sorte (escolhe uma peça adversária para remover)
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- Representação de uma casa no tabuleiro
data SpecialTile = SpecialTile
  { tileType :: TileType, -- Tipo da casa
    tilePosition :: Int
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- Representação de uma peça
data Piece = Piece
  { 
    pieceId :: Int, --Identificador entre 0 e 3 peças de determinado jogador
    pieceColor :: Color, -- Cor da peça
    piecePosition :: Int, -- Posição atual da peça
    tilesWalked :: Int, -- Número de casas andadas
    inStartingArea :: Bool, -- Indica se a peça está na área de início
    inFinishArea :: Bool, -- Indica se a peça está na área de fim
    finished :: Bool -- Indica se a peça já terminou o jogo
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- Representação de um jogador
data Player = Player
  { playerColor :: Color, -- Cor do jogador
    isBot :: Bool, -- Indica se o jogador é um bot
    startingPos :: Int -- Posição inicial do jogador
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- Definindo um tipo para representar os estados da tela
data ScreenState = MenuInicial
                 | MenuPlayers
                 | MenuBotsUmPlayer
                 | MenuBotsDoisPlayer
                 | MenuBotsTresPlayer
                 | JogoEmAndamento
                 deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- Estado atual do jogo
data GameState = GameState
  { players :: [Player], -- Lista de jogadores
    specialTiles :: [SpecialTile], -- Tabuleiro do jogo
    pieces :: [Piece], -- Lista de peças
    blockades :: [(Color, Int)], -- Lista de bloqueios
    currentPlayer :: Color, -- Cor do jogador atual
    diceRolled :: Int, -- Valor do último lançamento do dado
    processingMove :: Bool, -- Indica se um dado foi lançado e espera ser aplicado a uma peça
    end :: Bool, -- Indica se o jogo acabou
    sixesInRow :: Int, -- Contador de seis seguidos}
    screenState :: ScreenState,  -- Estado atual da tela
    wasLuckyMove :: Bool,
    winnerColor :: Color
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
