module Interface.Rendering where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified GameTypes
import Game.CreateGame
import Game.ProcessMove
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)

-- Tamanho do tabuleiro e das áreas
boardSize :: Float
boardSize = 455

cellSize :: Float
cellSize = 35  -- Tamanho das casas do caminho

-- Tamanho dos quadrados dos jogadores (3x3 células)
squareSize :: Float
squareSize = 140

-- Janela
window :: Display
window = InWindow "Ludo" (800, 800) (200, 200)

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

drawSpecialTile::GameTypes.SpecialTile->Picture
drawSpecialTile special = drawOnRegular (getSpecialTileSprite special) (fromIntegral(GameTypes.tilePosition special))

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
    dado <- randomRIO (1, 6)  -- Gera um número aleatório entre 1 e 6
    putStrLn ("Dado rolado: " ++ show dado)  -- Apenas para debug
    return gameState { GameTypes.diceRolled = dado } -- Atualiza o estado do jogo

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
    [line [(x, -boardSize/2), (x, boardSize/2)] | x <- [-boardSize/2, -boardSize/2 + cellSize .. boardSize/2]]
    <> pictures [line [(-boardSize/2, y), (boardSize/2, y)] | y <- [-boardSize/2, -boardSize/2 + cellSize .. boardSize/2]]

-- Lógica

initialGameState = (createGameState 4 2)

transformGameIO :: Event -> GameTypes.GameState -> IO GameTypes.GameState
transformGameIO (EventKey (MouseButton LeftButton) Up _ (x, y)) gameState
    | x > xMin && x < xMax && y > yMin && y < yMax = rolarDadoIO gameState  -- Rola o dado
    | otherwise = return (walkOneEachPiece  gameState)  -- Não faz nada se o clique não for no botão
  where
    -- Coordenadas do botão
    xMin = -7 * cellSize
    xMax = -1 * cellSize
    yMin = -9 * cellSize
    yMax = -7 * cellSize
transformGameIO _ gameState = return gameState  -- Não faz nada para outros eventos

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

baseQuadByColor::GameTypes.Piece -> (Int,Int)
baseQuadByColor piece | pieceColor == GameTypes.Red = (-5,5)
                     | pieceColor == GameTypes.Blue = (4,-4)
                     | pieceColor == GameTypes.Yellow = (4,5)
                     | otherwise = (-5,-4)
    where pieceColor = GameTypes.pieceColor (piece)

basePos::GameTypes.Piece -> Picture
basePos piece | id <= 1 = translate ((x + id) * cellSize) (y * cellSize) $ (pieceSprite piece)
              | otherwise = translate ((x + (id - 2)) * cellSize) ((y - 1) * cellSize) $ (pieceSprite piece)
    where id = fromIntegral (GameTypes.pieceId piece)
          quad = baseQuadByColor piece
          x = fromIntegral (fst quad)
          y = fromIntegral (snd quad)

-- Desenha a tela do jogo
drawScreen :: GameTypes.GameState -> IO Picture
drawScreen gameState = return $ pictures 
    [ drawBoard                -- Desenha o tabuleiro
    , drawAllPieces gameState  -- Desenha todas as peças
    , drawButton               -- Desenha o botão
    , drawDice gameState       -- Desenha o dado com o valor atual
    ]

drawAllPieces::GameTypes.GameState -> Picture
drawAllPieces gameState = pictures (map drawPiece (GameTypes.pieces (gameState)))

drawPiece::GameTypes.Piece -> Picture
drawPiece piece | position == -1 = basePos piece 
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
--(const id) sendo uma função que atualiza o gameState a cada segundo que se passa,
--como não iremos necessitar de animações no nosso escopo, a função identidade (const id) não faz nada