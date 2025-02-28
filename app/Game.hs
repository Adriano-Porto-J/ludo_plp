module Game where

import GameTypes

createGameState :: Int -> Int -> GameState
createGameState players bots = do
  if players == 2
    then do
      let colors = [Red, Blue]
      let players = createPlayers colors (players - bots) bots
      let specialTiles = createSpecialTiles
      let sixes = [(Red, 0), (Blue, 0)]

      GameState
        { players = players,
          specialTiles = specialTiles,
          currentPlayer = Red,
          rollDice = 0,
          end = False,
          sixesInRow = sixes
        }
    else do
      let colors = [Red, Green, Blue, Yellow]
      let players = createPlayers colors (players - bots) bots
      let specialTiles = createSpecialTiles
      let sixes = [(Red, 0), (Green, 0), (Blue, 0), (Yellow, 0)]

      GameState
        { players = players,
          specialTiles = specialTiles,
          currentPlayer = Red,
          rollDice = 0,
          end = False,
          sixesInRow = sixes
        }

createPlayers :: [Color] -> Int -> Int -> [Player]

definePlayers colors players bots = do
  let players = take players colors
  let players = map (\color -> (color, False)) players
  let bots = take bots (drop players colors)
  let bots = map (\color -> (color, True)) bots
  let players = players ++ bots
  let players = map createPlayer players
  players

createPlayer :: (Color, Boolean) -> Player
createPlayer color = Player {color = color, pieces = createPieces color, isBot = Boolean}

createPieces :: Color -> [Piece]
createPieces color =
  [ Piece {color = color, position = 0},
    Piece {color = color, position = 0},
    Piece {color = color, position = 0},
    Piece {color = color, position = 0}
  ]

createSpecialTiles :: [Tile]
createSpecialTiles =
  [ SpecialTile {tileType = Safe, position = 1},
    SpecialTile {tileType = Safe, position = 6},
    SpecialTile {tileType = Decline, position = 10},
    SpecialTile {tileType = Safe, position = 14},
    SpecialTile {tileType = Safe, position = 19},
    SpecialTile {tileType = Lucky, position = 23},
    SpecialTile {tileType = Safe, position = 27},
    SpecialTile {tileType = Safe, position = 32},
    SpecialTile {tileType = Boost, position = 36},
    SpecialTile {tileType = Safe, position = 40},
    SpecialTile {tileType = Safe, position = 45},
    SpecialTile {tileType = Death, position = 49}
  ]
