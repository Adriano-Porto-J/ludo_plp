module Game where

import GameTypes

createGameState :: Int -> Int -> GameState
createGameState players bots = do
  if players == 2
    then do
      let colors = [Red, Blue]
      let playerOrBot = definePlayers colors (players - bots) bots
      let players = map createPlayer playerOrBot
      let specialTiles = createSpecialTiles

      GameState
        { players = players,
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

      let specialTiles = createSpecialTiles

      GameState
        { players = players,
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
createPlayer (color, isB) = Player {playerColor = color, pieces = (createPieces color), isBot = isB, startingPos = (startingPosbyColor color)}

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



  getTileType :: GameState -> Int -> IO TileType
getTileType gs position = do
  let casas = specialTiles gs
  if position < 0 || position > 75 then do
      putStrLn $ "Posição inválida! Deve estar entre 0 e 75, mas foi " ++ show position
      return Normal 
    else
      case find (\casa -> tilePosition casa == position) casas of
        Just casa -> return $ tileType casa
        Nothing   -> return Normal
nextPlayer :: GameState -> GameState
nextPlayer gs = do
  let corAtual = currentPlayer gs
  let jogadores = players gs
  let cores = map((Player color   _) -> color) jogadores
  let currentIndex = case elemIndex corAtual cores of
        Just index -> index
        Nothing -> 0
  let nextIndex = (currentIndex + 1) mod length cores
  let nextCor = cores !! nextIndex
  return nextCor
'vidaal — Hoje às 10:23
getAvailableMoves :: GameState -> Color -> Int -> [Piece]
getAvailableMoves gs color diceRoll = do
  let currentPlayerState = head (filter (\p -> playerColor p == color) (players gs))
  let playerPieces = pieces currentPlayerState
  filter (canMovePiece diceRoll) playerPieces
canMovePiece :: Int -> Piece -> Bool
canMovePiece diceRoll piece
  | inStartingArea piece = diceRoll == 6 
  | otherwise = True 
checkWin :: GameState -> Color -> Bool
checkWin gs color = do
  let currentPlayerState = head (filter (\p -> playerColor p == color) (players gs))
  all (\p -> inFinishArea p && finished p) (pieces currentPlayerState)
'vidaal — Hoje às 10:59
applyEffect :: GameState -> Piece -> Tile -> IO GameState
applyEffect gs piece tile = 
  case tileType tile of

    Normal -> return gs
    Safe   -> return gs

    Boost -> do
        putStrLn "Você caiu em uma Casa de Boost! Avançando 3 casas."
        return $ movePiece gs piece 3

    Decline -> do
        putStrLn "Casa de Decline! Voltando 2 casas."
        return $ movePiece gs piece (-2)

    Death -> do
        putStrLn "Casa da Morte! Sua peça volta para a base."
        return $ sendPieceToBase gs piece

    Lucky -> do
        let opponents = filter (\p -> playerColor p /= pieceColor piece) (players gs)
        if null opponents 
            then putStrLn "Nenhum adversário disponível para remover." >> return gs
            else chooseOpponentToRemove gs opponents

movePiece :: GameState -> Piece -> Int -> GameState
movePiece gs piece pos = 
    let newPos = (piecePosition piece + pos) mod 52
        updatedPiece = piece { piecePosition = newPos }
    in updatePieceGameState gs updatedPiece

sendPieceToBase :: GameState -> Piece -> GameState
sendPieceToBase gs piece = 
    let updatedPiece = piece { piecePosition = -1, inStartingArea = True }
    in updatePieceGameState gs updatedPiece

  --falta a funcao chooseopponetToRemove
