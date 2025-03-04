module Interface.Rendering where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified GameTypes
import Game.CreateGame
import Game.ProcessMove
import Game.Index
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
import Graphics.Gloss (Picture, rectanglePath)

-- Janela
window :: Display
window = InWindow "Ludo" (800, 800) (200, 200)

-- Tamanho dos estados
boardSize :: Float
boardSize = 455


-- Desenhar Menu inicial 
drawMenuStart :: Picture
drawMenuStart = pictures
    [color black (rectangleSolid boardSize boardSize)   -- Fundo do menu
    , drawButtonNewGame
    , drawButtonLoadSavedGame
    , drawTituloLudo
    ]

drawButtonNewGame :: Picture
drawButtonNewGame = pictures
    [translate 0 (2 * cellSize) $ color white (rectangleSolid (6 * cellSize) (2 * cellSize)) -- Fundo do botão
    , translate 0 (2 * cellSize) $ scale 0.20 0.20 $ color black (text "Start New Game")  -- Texto centralizado
    ]

drawButtonLoadSavedGame :: Picture
drawButtonLoadSavedGame = pictures
    [translate 0 (-2 * cellSize) $ color white (rectangleSolid (6 * cellSize) (2 * cellSize)) -- Fundo do botão
    , translate 0 (-2 * cellSize) $ scale 0.20 0.20 $ color black (text "Load Saved Game")  -- Texto centralizado
    ]

drawTituloLudo :: Picture
drawTituloLudo  = pictures
    [translate 0 (5 * cellSize) $ scale 0.30 0.30 $ color black (text "Ludo Game")  -- Título centralizado
    ]

{-
-- Função para verificar clique nos botões do menu inicial
handleMenuStart :: Event -> Maybe MenuStartAction
handleMenuStart (EventKey (MouseButton LeftButton) Down _ (x, y)) =
    -- Verifica se o clique foi no botão "Start New Game"
    if x >= (-3 * cellSize) && x <= (3 * cellSize) && y >= (1.5 * cellSize) && y <= (2.5 * cellSize)
        then Just StartNewGame
    -- Verifica se o clique foi no botão "Load Saved Game"
    else if x >= (-3 * cellSize) && x <= (3 * cellSize) && y >= (-2.5 * cellSize) && y <= (-1.5 * cellSize)
        then Just LoadSavedGame
    -- Caso contrário, nenhum botão foi clicado
    else Nothing
handleMenuStart _ = Nothing
-}

-- Desenhar Menu players
drawMenuSelectionPlayers :: Picture
drawMenuSelectionPlayers = pictures
    [color black (rectangleSolid boardSize boardSize)   -- Fundo do menu
    , drawButtonsPlayers
    , drawTituloSelecionePlayers
    ]

drawButtonsPlayers :: Picture
drawButtonsPlayers = pictures
    [translate (-4 * cellSize) 0 $ color white (rectangleSolid (1.5 * cellSize) (1.5 * cellSize)) -- Botão 1
    , translate (4 * cellSize) 0 $ color white (rectangleSolid (1.5 * cellSize) (1.5 * cellSize)) -- Botão 2
    , translate 0 (-3 * cellSize) $ color white (rectangleSolid (1.5 * cellSize) (1.5 * cellSize)) -- Botão 3
    , translate (-4 * cellSize) 0 $ scale 0.20 0.20 $ color black (text "1")  -- Texto do botão 1
    , translate (4 * cellSize) 0 $ scale 0.20 0.20 $ color black (text "2")  -- Texto do botão 2
    , translate 0 (-3 * cellSize) $ scale 0.20 0.20 $ color black (text "3")  -- Texto do botão 3
    ]

drawTituloSelecionePlayers :: Picture
drawTituloSelecionePlayers = pictures
    [translate 0 (5 * cellSize) $ scale 0.20 0.20 $ color black (text "Selecione a quantidade")  -- Título 1
    , translate 0 (4 * cellSize) $ scale 0.20 0.20 $ color black (text "de bots")  -- Título 2
    ]

-- Função para verificar clique nos botões do menu de seleção de jogadores
handleMenuPlayers :: Event -> Maybe Int
handleMenuPlayers (EventKey (MouseButton LeftButton) Down _ (x, y)) =
    -- Verifica se o clique foi no botão "1"
    if x >= (-4.75 * cellSize) && x <= (-3.25 * cellSize) && y >= (-0.75 * cellSize) && y <= (0.75 * cellSize)
        then Just 1
    -- Verifica se o clique foi no botão "2"
    else if x >= (3.25 * cellSize) && x <= (4.75 * cellSize) && y >= (-0.75 * cellSize) && y <= (0.75 * cellSize)
        then Just 2
    -- Verifica se o clique foi no botão "3"
    else if x >= (-0.75 * cellSize) && x <= (0.75 * cellSize) && y >= (-3.75 * cellSize) && y <= (-2.25 * cellSize)
        then Just 3
    -- Caso contrário, nenhum botão foi clicado
    else Nothing
handleMenuPlayers _ = Nothing

-- Tamanho das casas do caminho
cellSize :: Float
cellSize = 35

-- Tamanho dos quadrados dos jogadores (5x5 células)
squareSize :: Float
squareSize = 140

-- Fundo branco
background :: Color
background = white

-- Desenhar tabuleiro
drawBoard :: Picture
drawBoard = pictures
    [ color (black) (rectangleSolid boardSize boardSize)  -- Fundo do tabuleiro
    , drawBaseAreas
    , drawPaths
    , drawSpecialTiles
    , drawGrid
    ]

-- Desenhar áreas iniciais dos jogadores
drawBaseAreas :: Picture
drawBaseAreas = pictures
    [ translate (-158) (158) $ color red (rectangleSolid squareSize squareSize)    -- Área vermelha
    , translate (158) (158) $ color yellow (rectangleSolid squareSize squareSize)       -- Área azul
    , translate (-158) (-158) $ color green (rectangleSolid squareSize squareSize)    -- Área verde
    , translate (158) (-158) $ color blue (rectangleSolid squareSize squareSize)    -- Área amarela
    ]

-- Desenhar caminhos das peças no estilo do tabuleiro original do Ludo
drawPaths :: Picture
drawPaths = pictures
    [ color white $ rectangleSolid (cellSize * 6) cellSize  -- Caminho horizontal
    , color white $ rectangleSolid cellSize (cellSize * 6)  -- Caminho vertical

    -- Cercando caminhos com quadrados brancos
    , pictures [translate (-6 * cellSize + (i * cellSize)) cellSize $ color white (rectangleSolid cellSize cellSize) | i <- [0..6]]
    , pictures [translate (-6 * cellSize + (i * cellSize)) (-cellSize) $ color white (rectangleSolid cellSize cellSize) | i <- [0..6]]
    , pictures [translate (6 * cellSize - (i * cellSize)) cellSize $ color white (rectangleSolid cellSize cellSize) | i <- [0..6]]
    , pictures [translate (6 * cellSize - (i * cellSize)) (-cellSize) $ color white (rectangleSolid cellSize cellSize) | i <- [0..6]]
    , pictures [translate cellSize (-6 * cellSize + (i * cellSize)) $ color white (rectangleSolid cellSize cellSize) | i <- [0..6]]
    , pictures [translate (-cellSize) (-6 * cellSize + (i * cellSize)) $ color white (rectangleSolid cellSize cellSize) | i <- [0..6]]
    , pictures [translate cellSize (6 * cellSize - (i * cellSize)) $ color white (rectangleSolid cellSize cellSize) | i <- [0..6]]
    , pictures [translate (-cellSize) (6 * cellSize - (i * cellSize)) $ color white (rectangleSolid cellSize cellSize) | i <- [0..6]]
    -- Caminhos coloridos das peças
    , translate (-2 * cellSize) 0 $ color red (rectangleSolid cellSize cellSize)
    , translate (2 * cellSize) 0 $ color blue (rectangleSolid cellSize cellSize)
    , translate 0 (-2 * cellSize) $ color green (rectangleSolid cellSize cellSize)
    , translate 0 (2 * cellSize) $ color yellow (rectangleSolid cellSize cellSize)

    -- Caminhos coloridos até o centro
    , pictures [translate (-5 * cellSize + (i * cellSize)) 0 $ color red (rectangleSolid cellSize cellSize) | i <- [0..4]]
    , pictures [translate (5 * cellSize - (i * cellSize)) 0 $ color blue (rectangleSolid cellSize cellSize) | i <- [0..4]]
    , pictures [translate 0 (-5 * cellSize + (i * cellSize)) $ color green (rectangleSolid cellSize cellSize) | i <- [0..4]]
    , pictures [translate 0 (5 * cellSize - (i * cellSize)) $ color yellow (rectangleSolid cellSize cellSize) | i <- [0..4]]

    , color black $ rectangleSolid cellSize cellSize  -- Centro


    -- Quadrados brancos nas pontas dos caminhos coloridos
    , translate (-6 * cellSize) 0 $ color white (rectangleSolid cellSize cellSize)
    , translate (6 * cellSize) 0 $ color white (rectangleSolid cellSize cellSize)
    , translate 0 (-6 * cellSize) $ color white (rectangleSolid cellSize cellSize)
    , translate 0 (6 * cellSize) $ color white (rectangleSolid cellSize cellSize)

    --Quadrados coloridos de inicio das pecas
    , translate (-5 * cellSize) (1 * cellSize) $ color red (rectangleSolid cellSize cellSize)
    , translate (5 * cellSize) (-1 * cellSize) $ color blue (rectangleSolid cellSize cellSize)
    , translate (1 * cellSize) (5 * cellSize) $ color yellow (rectangleSolid cellSize cellSize)
    , translate (-1 * cellSize) (-5 * cellSize) $ color green (rectangleSolid cellSize cellSize)

    ]

drawSpecialTiles::Picture
drawSpecialTiles = pictures (map drawSpecialTile createSpecialTiles)

getSpecialTileSprite::GameTypes.SpecialTile->Picture
getSpecialTileSprite special | GameTypes.tileType (special) == GameTypes.Safe = safeTile
                             | GameTypes.tileType (special) == GameTypes.Boost = boostTile
                             | GameTypes.tileType (special) == GameTypes.Decline = declineTile
                             | GameTypes.tileType (special) == GameTypes.Death = deathTile
                             | GameTypes.tileType (special) == GameTypes.Lucky = luckyTile
                             | otherwise = Blank
--
drawSpecialTile::GameTypes.SpecialTile->Picture
drawSpecialTile special = drawOnRegular (getSpecialTileSprite special) (fromIntegral(GameTypes.tilePosition special))

boostTile::Picture
boostTile = pictures [ rotate 0.0 $ rectangleSolid side 7.0
                    , rotate (90.0) $ rectangleSolid side 7.0]
    where side = min cellSize cellSize * 0.75

declineTile::Picture
declineTile = pictures [ rotate 0.0 $ rectangleSolid side 7.0]
    where side = min cellSize cellSize * 0.75

safeTile::Picture
safeTile = thickCircle radius 6.0
    where radius = min cellSize cellSize * 0.3

deathTile::Picture
deathTile = pictures [ rotate 45.0 $ rectangleSolid side 7.0
                    , rotate (-45.0) $ rectangleSolid side 7.0]
    where side = min cellSize cellSize * 0.75

luckyTile::Picture
luckyTile = pictures
    [ translate (0.125 * cellSize) 0 $ rotate (-60.0) $ rectangleSolid side 5.0
    , translate 0 (0.3 * cellSize) $ rectangleSolid side 5.0
    , translate 0.4 0 $ scale 0.8 0.8 $ rectangleSolid side 5]
    where side = min cellSize cellSize * 0.75

-- Desenhar a grade do tabuleiro
drawGrid :: Picture
drawGrid = color black $ pictures
    [line [(x,-boardSize/2), (x, boardSize/2)] | x <- [-boardSize/2, -boardSize/2 + cellSize .. boardSize/2]]
    <> pictures [line [(-boardSize/2, y), (boardSize/2, y)] | y <- [-boardSize/2, -boardSize/2 + cellSize .. boardSize/2]]

-- Lógica

initialGameState = (Game.CreateGame.createGameState 4 2)

transformGameIO :: Event -> GameTypes.GameState -> IO GameTypes.GameState
transformGameIO (EventKey (MouseButton LeftButton) Up _ (x, y)) gameState
    | x > xMin && x < xMax && y > yMin && y < yMax = rolarDadoIO gameState  -- Rola o dado
    | x > boardXMin && x < boardXMax && y > boardYMin && y < boardYMax = return (selectPiece gameState (selectPosition gameState (x / cellSize) (y / cellSize))) -- Rola o dado
    | otherwise = return (walkOneEachPiece  gameState)  -- Não faz nada se o clique não for no botão
  where
    -- Coordenadas do botão
    xMin = -7 * cellSize
    xMax = -1 * cellSize
    yMin = -9 * cellSize
    yMax = -7 * cellSize
    boardXMin = -6.5 * cellSize
    boardXMax =  6.5 * cellSize
    boardYMin = -6.5 * cellSize
    boardYMax =  6.5 * cellSize
transformGameIO _ gameState = return gameState  -- Não faz nada para outros eventos

selectPiece::GameTypes.GameState -> Int -> GameTypes.GameState --Seleciona a peça de acordo com a localização do tabuleiro e realiza a jogada
selectPiece gameState piecePos = case piece of
                                    Nothing -> gameState
                                    Just piece -> 
                                        if GameTypes.processingMove (gameState) == True && piecePos > -5
                                            then
                                            if GameTypes.diceRolled gameState == 6 && GameTypes.sixesInRow gameState < 3 then
                                                (Game.ProcessMove.processMove gameState (piecePos,GameTypes.diceRolled (gameState))) {GameTypes.diceRolled = -1}
                                            else do
                                                let newState = Game.ProcessMove.processMove gameState (piecePos,GameTypes.diceRolled (gameState))
                                                nextPlayer newState
                                        else gameState
                                
    where piece = getPieceByPositionAndColor (GameTypes.pieces gameState) (GameTypes.currentPlayer gameState) piecePos 
--
selectPosition::GameTypes.GameState -> Float -> Float -> Int
selectPosition gameState x y | basePos!!0 < x && basePos!!1 > x && basePos!!2 < y && basePos!!3 > y = -1
                             | (basePos!!0 + 1) < x && (basePos!!1 + 1) > x && basePos!!2 < y && basePos!!3 > y = -2
                             | basePos!!0 < x && basePos!!1 > x && (basePos!!2 - 1) < y && (basePos!!3 - 1) > y = -3
                             | (basePos!!0 + 1) < x && (basePos!!1 + 1) > x && (basePos!!2 - 1) < y && (basePos!!3 - 1) > y = -4
                             | x < -4.5 && x > -5.5 && y < 1.5 && y > 0.5 = 0
                             | x < -3.5 && x > -4.5 && y < 1.5 && y > 0.5 = 1
                             | x < -2.5 && x > -3.5 && y < 1.5 && y > 0.5 = 2
                             | x < -1.5 && x > -2.5 && y < 1.5 && y > 0.5 = 3
                             | x < -0.5 && x > -1.5 && y < 1.5 && y > 0.5 = 4
                             | x < -0.5 && x > -1.5 && y < 2.5 && y > 1.5 = 5
                             | x < -0.5 && x > -1.5 && y < 3.5 && y > 2.5 = 6
                             | x < -0.5 && x > -1.5 && y < 4.5 && y > 3.5 = 7
                             | x < -0.5 && x > -1.5 && y < 5.5 && y > 4.5 = 8
                             | x < -0.5 && x > -1.5 && y < 6.5 && y > 5.5 = 9
                             | x < 0.5 && x > -0.5 && y < 6.5 && y > 5.5 = 10
                             | x < 1.5 && x > 0.5 && y < 6.5 && y > 5.5 = 11
                             | x < 1.5 && x > 0.5 && y < 5.5 && y > 4.5 = 12
                             | x < 1.5 && x > 0.5 && y < 4.5 && y > 3.5 = 13
                             | x < 1.5 && x > 0.5 && y < 3.5 && y > 2.5 = 14
                             | x < 1.5 && x > 0.5 && y < 2.5 && y > 1.5 = 15
                             | x < 1.5 && x > 0.5 && y < 1.5 && y > 0.5 = 16
                             | x < 2.5 && x > 1.5 && y < 1.5 && y > 0.5 = 17
                             | x < 3.5 && x > 2.5 && y < 1.5 && y > 0.5 = 18
                             | x < 4.5 && x > 3.5 && y < 1.5 && y > 0.5 = 19
                             | x < 5.5 && x > 4.5 && y < 1.5 && y > 0.5 = 20
                             | x < 6.5 && x > 5.5 && y < 1.5 && y > 0.5 = 21
                             | x < 6.5 && x > 5.5 && y < 0.5 && y > -0.5 = 22
                             | x < 6.5 && x > 5.5 && y < -0.5 && y > -1.5 = 23
                             | x < 5.5 && x > 4.5 && y < -0.5 && y > -1.5 = 24
                             | x < 4.5 && x > 3.5 && y < -0.5 && y > -1.5 = 25
                             | x < 3.5 && x > 2.5 && y < -0.5 && y > -1.5 = 26
                             | x < 2.5 && x > 1.5 && y < -0.5 && y > -1.5 = 27
                             | x < 1.5 && x > 0.5 && y < -0.5 && y > -1.5 = 28
                             | x < 1.5 && x > 0.5 && y < -1.5 && y > -2.5 = 29
                             | x < 1.5 && x > 0.5 && y < -2.5 && y > -3.5 = 30
                             | x < 1.5 && x > 0.5 && y < -3.5 && y > -4.5 = 31
                             | x < 1.5 && x > 0.5 && y < -4.5 && y > -5.5 = 32
                             | x < 1.5 && x > 0.5 && y < -5.5 && y > -6.5 = 33
                             | x < 0.5 && x > -0.5 && y < -5.5 && y > -6.5 = 34
                             | x < -0.5 && x > -1.5 && y < -5.5 && y > -6.5 = 35
                             | x < -0.5 && x > -1.5 && y < -4.5 && y > -5.5 = 36
                             | x < -0.5 && x > -1.5 && y < -3.5 && y > -4.5 = 37
                             | x < -0.5 && x > -1.5 && y < -2.5 && y > -3.5 = 38
                             | x < -0.5 && x > -1.5 && y < -1.5 && y > -2.5 = 39
                             | x < -0.5 && x > -1.5 && y < -0.5 && y > -1.5 = 40
                             | x < -1.5 && x > -2.5 && y < -0.5 && y > -1.5 = 41
                             | x < -2.5 && x > -3.5 && y < -0.5 && y > -1.5 = 42
                             | x < -3.5 && x > -4.5 && y < -0.5 && y > -1.5 = 43
                             | x < -4.5 && x > -5.5 && y < -0.5 && y > -1.5 = 44
                             | x < -5.5 && x > -6.5 && y < -0.5 && y > -1.5 = 45
                             | x < -5.5 && x > -6.5 && y < 0.5 && y > -0.5 = 46
                             | x < -5.5 && x > -6.5 && y < 1.5 && y > 0.5 = 47
                             | x < -4.5 && x > -5.5 && y < 0.5 && y > -0.5 = 48 --Zona final do vermelho
                             | x < -3.5 && x > -4.5 && y < 0.5 && y > -0.5 = 49
                             | x < -2.5 && x > -3.5 && y < 0.5 && y > -0.5 = 50
                             | x < -1.5 && x > -2.5 && y < 0.5 && y > -0.5 = 51
                             | x < -0.5 && x > -1.5 && y < 0.5 && y > -0.5 = 52
                             | x < 0.5 && x > -0.5 && y < 5.5 && y > 4.5 = 54 --Zona final do amarelo
                             | x < 0.5 && x > -0.5 && y < 4.5 && y > 3.5 = 55
                             | x < 0.5 && x > -0.5 && y < 3.5 && y > 2.5 = 56
                             | x < 0.5 && x > -0.5 && y < 2.5 && y > 1.5 = 57
                             | x < 0.5 && x > -0.5 && y < 1.5 && y > 0.5 = 58
                             | x < 1.5 && x > 0.5 && y < 0.5 && y > -0.5 = 60 --Zona final do azul
                             | x < 2.5 && x > 1.5 && y < 0.5 && y > -0.5 = 61
                             | x < 3.5 && x > 2.5 && y < 0.5 && y > -0.5 = 62
                             | x < 4.5 && x > 3.5 && y < 0.5 && y > -0.5 = 63
                             | x < 5.5 && x > 4.5 && y < 0.5 && y > -0.5 = 64
                             | x < 0.5 && x > -0.5 && y < -0.5 && y > -1.5 = 66 --Zona final do Amarelo
                             | x < 0.5 && x > -0.5 && y < -1.5 && y > -2.5 = 67
                             | x < 0.5 && x > -0.5 && y < -2.5 && y > -3.5 = 68
                             | x < 0.5 && x > -0.5 && y < -3.5 && y > -4.5 = 69
                             | x < 0.5 && x > -0.5 && y < -4.5 && y > -5.5 = 70
                             | otherwise = -5
    where basePos = baseQuadByColorMousePos(baseQuadByColor (GameTypes.currentPlayer (gameState)))

baseQuadByColorMousePos::(Int,Int) -> [Float]
baseQuadByColorMousePos (x,y)           | (x,y) == (-5,5) = [-5.5,-4.5,4.5,5.5] --vermelho
                                        | (x,y) == (4,5) = [3.5,4.5,4.5,5.5] --amarelo
                                        | (x,y) == (-5,-4) = [-5.5,-4.5,-4.5,-3.5] --verde
                                        | otherwise = [3.5,4.5,-4.5,-3.5] --azul

pieceSprite::GameTypes.Piece -> Picture
pieceSprite piece = pictures [color black (rectangleSolid (side+5) (side+5)), color cor (rectangleSolid (side-1) (side-1))]
    where cor = getPieceColor piece
          side = min cellSize cellSize * 0.75

getPieceColor::GameTypes.Piece -> Color
getPieceColor piece | pieceColor == GameTypes.Red = red
               | pieceColor == GameTypes.Blue = blue
               | pieceColor == GameTypes.Yellow = yellow
               | otherwise = green
    where pieceColor = GameTypes.pieceColor (piece)

baseQuadByColor::GameTypes.Color -> (Int,Int)
baseQuadByColor color | color == GameTypes.Red = (-5,5)
                     | color == GameTypes.Blue = (4,-4)
                     | color == GameTypes.Yellow = (4,5)
                     | otherwise = (-5,-4)

basePosPiece::GameTypes.Piece -> Picture
basePosPiece piece | id <= 2 = translate ((x + id - 1) * cellSize) (y * cellSize) $ (pieceSprite piece)
              | otherwise = translate ((x + (id - 3)) * cellSize) ((y - 1) * cellSize) $ (pieceSprite piece)
    where id = fromIntegral (GameTypes.pieceId piece)
          quad = baseQuadByColor (GameTypes.pieceColor piece)
          x = fromIntegral (fst quad)
          y = fromIntegral (snd quad)

drawPlayerText::GameTypes.GameState -> Picture
drawPlayerText gameState | currentPlayer == GameTypes.Red = translate (-3 * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color red (text "Jogador Vermelho")
                         | currentPlayer == GameTypes.Yellow = translate (-3 * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color yellow (text "Jogador Amarelo")
                         | currentPlayer == GameTypes.Green = translate (-3 * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color green (text "Jogador Verde")
                         | otherwise = translate (-3 * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color blue (text "Jogador Azul")
    where currentPlayer = GameTypes.currentPlayer gameState

drawButton :: Picture
drawButton = pictures
    [translate  (-3.5 * cellSize) (-8 * cellSize) $ color black (rectangleSolid (6 * cellSize) (2 * cellSize))-- Fundo do botão
    , translate (-5.5 * cellSize) (-8 * cellSize) $ scale 0.20 0.20 $ color white (text "Rolar Dado")  -- Texto
    ]

-- Tamanho do dado
diceSize :: Float
diceSize = 2*cellSize

-- Tamanho dos circulos do dado
pipRadius :: Float
pipRadius = 5

-- Cor do dado
diceColor :: Color
diceColor = black

-- Cor dos circulos do dado
pipColor :: Color
pipColor = white

-- Desenha um ponto (pipa) na posição (x, y)
drawPip :: Float -> Float -> Picture
drawPip x y = translate x y $ color pipColor (circleSolid pipRadius)

-- Desenha a face do dado com os pontos correspondentes ao valor
drawDiceFace :: Int -> Picture
drawDiceFace value = pictures [diceSquare, pips]
  where
    -- Quadrado do dado
    diceSquare = color black (rectangleSolid diceSize diceSize)

    -- Pontos (pipas) dependendo do valor do dado
    pips = case value of
      1 -> pictures [drawPip 0 0]  -- Ponto central
      2 -> pictures [drawPip (-offset) offset, drawPip offset (-offset)]
      3 -> pictures [drawPip (-offset) offset, drawPip 0 0, drawPip offset (-offset)]
      4 -> pictures [drawPip (-offset) offset, drawPip offset offset,
                     drawPip (-offset) (-offset), drawPip offset (-offset)]
      5 -> pictures [drawPip (-offset) offset, drawPip offset offset,
                     drawPip 0 0,
                     drawPip (-offset) (-offset), drawPip offset (-offset)]
      6 -> pictures [drawPip (-offset) offset, drawPip offset offset,
                     drawPip (-offset) 0, drawPip offset 0,
                     drawPip (-offset) (-offset), drawPip offset (-offset)]
      _ -> blank  -- Caso inválido (não desenha nada)

    -- Offset para posicionar os pontos
    offset = diceSize / 3

drawDice :: GameTypes.GameState -> Picture
drawDice gameState = translate (2 * cellSize) (-8 * cellSize) $ drawDiceFace (GameTypes.diceRolled gameState)

rolarDadoIO :: GameTypes.GameState -> IO GameTypes.GameState
rolarDadoIO gameState = do
    if ((GameTypes.diceRolled gameState) == -1 || ((GameTypes.diceRolled gameState) == -6 && GameTypes.sixesInRow gameState < 3))then do 
        dado <- randomRIO (1, 6)  -- Gera um número aleatório entre 1 e 6
        putStrLn ("Dado rolado: " ++ show dado)  -- Apenas para debug
        return gameState { GameTypes.diceRolled = dado, GameTypes.processingMove = True} -- Atualiza o estado do jogo
    else return gameState

-- Desenha a tela do jogo
drawScreen :: GameTypes.GameState -> IO Picture
drawScreen gameState = return $ pictures
    [ drawBoard                -- Desenha o tabuleiro
    , drawAllPieces gameState  -- Desenha todas as peças
    , drawButton               -- Desenha o botão
    , drawDice gameState       -- Desenha o dado com o valor atual
    , drawPlayerText gameState -- Desenha o texto indicando a vez do jogador  
    ]

drawAllPieces::GameTypes.GameState -> Picture
drawAllPieces gameState = pictures (map drawPiece (GameTypes.pieces (gameState)))

drawPiece::GameTypes.Piece -> Picture
drawPiece piece | position == -1 = basePosPiece piece 
                | otherwise = if GameTypes.inFinishArea (piece) == True then 
                    (drawOnFinalArea sprite position) 
                else 
                    (drawOnRegular sprite position)
    where position = (fromIntegral(GameTypes.piecePosition piece))
          sprite = pieceSprite piece

drawOnRegular::Picture -> Float -> Picture
drawOnRegular sprite position
                       | position >= 0 && position < 5 = translate ((position - 5) * cellSize) (1 * cellSize) $ sprite
                       | position >= 5 && position < 9 = translate ((-1) * cellSize) ((position - 3) * cellSize) $ sprite
                       | position >= 9 && position < 12 = translate ((position - 10) * cellSize) (6 * cellSize) $ sprite
                       | position >= 12 && position < 16 = translate (1 * cellSize) ((17 - position) * cellSize) $ sprite
                       | position >= 16 && position < 21 = translate ((position - 15) * cellSize) (1 * cellSize) $ sprite
                       | position >= 21 && position < 24 = translate (6 * cellSize) ((22 - position) * cellSize) $ sprite
                       | position >= 24 && position < 29 = translate ((29 - position)* cellSize) ((-1) * cellSize) $ sprite
                       | position >= 29 && position < 33 = translate (1 * cellSize) ((27 - position) * cellSize) $ sprite
                       | position >= 33 && position < 36 = translate ((34 - position) * cellSize) ((-6) * cellSize) $ sprite
                       | position >= 36 && position < 40 = translate ((-1) * cellSize) ((position - 41) * cellSize) $ sprite
                       | position >= 40 && position < 45 = translate ((39 - position) * cellSize) ((-1) * cellSize) $ sprite
                       | position >= 45 && position <= 47 = translate ((-6) * cellSize) ((position - 46) * cellSize) $ sprite
                       | otherwise = drawOnFinalArea sprite position

drawOnFinalArea::Picture -> Float -> Picture
drawOnFinalArea sprite position| position >= 48 && position < 53 = translate ((position - 53) * cellSize) (0 * cellSize) $ sprite
                         | position >= 54 && position < 59 = translate (0 * cellSize) ((59 - position) * cellSize) $ sprite
                         | position >= 60 && position < 65 = translate ((65 - position) * cellSize) (0 * cellSize) $ sprite
                         | position >= 66 && position < 71 = translate (0 * cellSize) ((position - 71) * cellSize) $ sprite
                         | otherwise = Blank

walkOne::GameTypes.Piece->GameTypes.Piece
walkOne piece = GameTypes.Piece {
    GameTypes.pieceId = GameTypes.pieceId piece,
    GameTypes.pieceColor = GameTypes.pieceColor piece,
    GameTypes.piecePosition = ((GameTypes.piecePosition piece) + 1),
    GameTypes.tilesWalked = GameTypes.tilesWalked piece,
    GameTypes.inStartingArea = GameTypes.inStartingArea piece,
    GameTypes.inFinishArea = False,
    GameTypes.finished = GameTypes.finished piece
  }

walkOneEachPiece::GameTypes.GameState->GameTypes.GameState
walkOneEachPiece gameState = do
    let newPieces = map walkOne (GameTypes.pieces gameState)
    gameState {GameTypes.pieces = newPieces}

render :: IO ()
render = playIO window background 30 initialGameState drawScreen transformGameIO (const (return . id))
-- Função principal

--render = display window background drawBoard

--Para um ambiente interativo, devemos utilizar a função play ao invés de render
--Mais sobre em: https://hackage.haskell.org/package/gloss-1.13.2.2/docs/Graphics-Gloss.html#v:play

--render = play background 30 gameState drawBoard inputHandling (const id)
--gameState sendo um tipo de dado que representa o estado do jogo
--drawBoard como sendo uma função que recebe esse tipo de dado e o transforma para Picture (gameState -> Picture)
--inputHandling sendo a função (Event -> gameState -> gameState) que atualiza o estado do jogo de acordo c a entrada
--(const id) sendo uma função que atualiza o gameState a cada segundo que se passa,t id) não faz nada