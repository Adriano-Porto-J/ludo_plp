module Game.CreateGame where

import Game.Auxiliary
import GameTypes

createGameState :: Int -> Int -> GameState
createGameState players bots =
  let colors = if players == 2 then [Red, Blue] else if players == 3 then [Red, Yellow, Blue] else [Red, Yellow, Blue, Green]
      playerOrBot = definePlayers colors (players - bots) bots
      allPlayers = map createPlayer playerOrBot
      allPieces = concatMap createPieces colors
      specialTiles = createSpecialTiles
   in GameState
        { players = allPlayers,
          pieces = allPieces,
          blockades = [],
          specialTiles = specialTiles,
          currentPlayer = Red,
          diceRolled = -1,
          processingMove = False,
          end = False,
          sixesInRow = 0,
          screenState = MenuInicial,  -- Inicializa o estado da tela como MenuInicial
          wasLuckyMove = False
        }

definePlayers :: [Color] -> Int -> Int -> [(Color, Bool)]
definePlayers colors numPlayers bots
  | numPlayers + bots > length colors = error "Número de jogadores e bots excede o número de cores disponíveis"
  | otherwise =
      let (playerColors, botColors) = splitAt numPlayers colors
          playerTuples = map (\color -> (color, False)) playerColors
          botTuples = map (\color -> (color, True)) botColors
       in playerTuples ++ botTuples

createPlayer :: (Color, Bool) -> Player
createPlayer (color, isB) = Player {playerColor = color, isBot = isB, startingPos = startingPosByColor color}

createPieces :: Color -> [Piece]
createPieces color =
  [ Piece {pieceId = 1, pieceColor = color, piecePosition = -1, tilesWalked = 0, inStartingArea = True, inFinishArea = False, finished = False},
    Piece {pieceId = 2, pieceColor = color, piecePosition = -2, tilesWalked = 0, inStartingArea = True, inFinishArea = False, finished = False},
    Piece {pieceId = 3, pieceColor = color, piecePosition = -3, tilesWalked = 0, inStartingArea = True, inFinishArea = False, finished = False},
    Piece {pieceId = 4, pieceColor = color, piecePosition = -4, tilesWalked = 0, inStartingArea = True, inFinishArea = False, finished = False}
  ]

createSpecialTiles :: [SpecialTile]
createSpecialTiles =
  [ SpecialTile Safe 0,
    SpecialTile Safe 7,
    SpecialTile Decline 11,
    SpecialTile Safe 12,
    SpecialTile Safe 24,
    SpecialTile Lucky 25,
    SpecialTile Safe 28,
    SpecialTile Safe 33,
    SpecialTile Safe 37,
    SpecialTile Boost 40,
    SpecialTile Death 41,
    SpecialTile Safe 46
  ]
