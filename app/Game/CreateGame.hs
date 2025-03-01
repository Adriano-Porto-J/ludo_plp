module Game.CreateGame where

import GameTypes

createGameState :: Int -> Int -> GameState
createGameState players bots = do
  if players == 2
    then do
      let colors = [Red, Blue]
      let playerOrBot = definePlayers colors (players - bots) bots
      let players = map createPlayer playerOrBot
      let pieces = concatMap createPieces colors
      let specialTiles = createSpecialTiles

      GameState
        { players = players,
          pieces = pieces,
          blockades = [],
          specialTiles = specialTiles,
          currentPlayer = Red,
          diceRolled = 0,
          end = False,
          sixesInRow = 0
        }
    else do
      let colors = [Red, Green, Blue, Yellow]
      let playerOrBot = definePlayers colors (players - bots) bots
      let players = map createPlayer playerOrBot
      let pieces = concatMap createPieces colors
      let specialTiles = createSpecialTiles

      GameState
        { players = players,
          pieces = pieces,
          blockades = [],
          specialTiles = specialTiles,
          currentPlayer = Red,
          diceRolled = 0,
          end = False,
          sixesInRow = 0
        }

definePlayers :: [Color] -> Int -> Int -> [(Color, Bool)]
definePlayers colors numPlayers bots = do
  let playerColors = take numPlayers colors
  let botColors = take bots (drop numPlayers colors)
  let playerTuples = map (\color -> (color, False)) playerColors
  let botTuples = map (\color -> (color, True)) botColors
  playerTuples ++ botTuples

createPlayer :: (Color, Bool) -> Player
createPlayer (color, isB) = Player {playerColor = color, isBot = isB, startingPos = (startingPosbyColor color)}

startingPosbyColor :: Color -> Int
startingPosbyColor Red = 1
startingPosbyColor Green = 14
startingPosbyColor Blue = 27
startingPosbyColor Yellow = 40

createPieces :: Color -> [Piece]
createPieces color =
  [ Piece {pieceColor = color, piecePosition = -1, tilesWalked = 0, inStartingArea = True, inFinishArea = False, finished = False},
    Piece {pieceColor = color, piecePosition = -1, tilesWalked = 0, inStartingArea = True, inFinishArea = False, finished = False},
    Piece {pieceColor = color, piecePosition = -1, tilesWalked = 0, inStartingArea = True, inFinishArea = False, finished = False},
    Piece {pieceColor = color, piecePosition = -1, tilesWalked = 0, inStartingArea = True, inFinishArea = False, finished = False}
  ]

createSpecialTiles :: [SpecialTile]
createSpecialTiles =
  [ SpecialTile {tileType = Safe, tilePosition = 1},
    SpecialTile {tileType = Safe, tilePosition = 6},
    SpecialTile {tileType = Decline, tilePosition = 10},
    SpecialTile {tileType = Safe, tilePosition = 14},
    SpecialTile {tileType = Safe, tilePosition = 19},
    SpecialTile {tileType = Lucky, tilePosition = 23},
    SpecialTile {tileType = Safe, tilePosition = 27},
    SpecialTile {tileType = Safe, tilePosition = 32},
    SpecialTile {tileType = Boost, tilePosition = 36},
    SpecialTile {tileType = Safe, tilePosition = 40},
    SpecialTile {tileType = Safe, tilePosition = 45},
    SpecialTile {tileType = Death, tilePosition = 49}
  ]
