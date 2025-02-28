module GameTypes where

-- Representação das cores dos jogadores
data Color = Red | Green | Blue | Yellow
  deriving (Show, Eq, Read)

-- Tipos de casas no tabuleiro
data TileType
  = Normal -- Casa comum
  | Safe -- Casa segura (não pode ser capturada)
  | Boost -- Casa de impulso (avança X casas)
  | Decline -- Casa de declínio (recua X casas)
  | Death -- Casa da morte (peça volta para a base)
  | Lucky -- Casa da sorte (escolhe uma peça adversária para remover)
  deriving (Show, Eq, Read)

-- Representação de uma casa no tabuleiro
data SpecialTile = SpecialTile
  { tileType :: TileType, -- Tipo da casa
    tilePosition :: Int
  }
  deriving (Show, Read)

-- Representação de uma peça
data Piece = Piece
  { pieceColor :: Color, -- Cor da peça
    piecePosition :: Int, -- Posição atual da peça
    tilesWalked :: Int, -- Número de casas andadas
    inStartingArea :: Bool, -- Indica se a peça está na área de início
    inFinishArea :: Bool, -- Indica se a peça está na área de fim
    finished :: Bool -- Indica se a peça já terminou o jogo
  }
  deriving (Show, Eq, Read)

-- Representação de um jogador
data Player = Player
  { playerColor :: Color, -- Cor do jogador
    pieces :: [Piece], -- Lista de peças do jogador
    isBot :: Bool, -- Indica se o jogador é um bot
    startingPos :: Int -- Posição inicial do jogador
  }
  deriving (Show, Read)

-- Estado atual do jogo
data GameState = GameState
  { players :: [Player], -- Lista de jogadores
    specialTiles :: [SpecialTile], -- Tabuleiro do jogo
    currentPlayer :: Color, -- Cor do jogador atual
    rollDice :: Int, -- Valor do último lançamento do dado
    end :: Bool, -- Indica se o jogo acabou
    sixesInRow :: Int -- Contador de seis seguidos}
  }
  deriving (Show, Read)
