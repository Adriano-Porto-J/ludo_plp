module Interface.Rendering where

import Graphics.Gloss

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
window = InWindow "Ludo" (600, 600) (100, 100)

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
drawSpecialTiles= pictures 
    [ translate (-5 * cellSize) (1 * cellSize) $ color black safeTile
    , translate (-3 * cellSize) (-1 * cellSize) $ color black safeTile
    , translate (-1 * cellSize) (-5 * cellSize) $ color black safeTile
    , translate (1 * cellSize) (-2 * cellSize) $ color black safeTile
    , translate (5 * cellSize) (-1 * cellSize) $ color black safeTile
    , translate (1 * cellSize) (1 * cellSize) $ color black safeTile
    , translate (1 * cellSize) (5 * cellSize) $ color black safeTile
    , translate (-1 * cellSize) (6 * cellSize) $ color black boostTile
    , translate (-1 * cellSize) (-3 * cellSize) $ color black declineTile
    , translate (-1 * cellSize) (5 * cellSize) $ color black deathTile
    , translate (4 * cellSize) (-1 * cellSize) $ color black luckyTile
    ]


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
-- Função principal
render :: IO ()
render = display window background drawBoard

--Para um ambiente interativo, devemos utilizar a função play ao invés de render
--Mais sobre em: https://hackage.haskell.org/package/gloss-1.13.2.2/docs/Graphics-Gloss.html#v:play

--render = play background 30 gameState drawBoard inputHandling (const id)
--gameState sendo um tipo de dado que representa o estado do jogo
--drawBoard como sendo uma função que recebe esse tipo de dado e o transforma para Picture (gameState -> Picture)
--inputHandling sendo a função (Event -> gameState -> gameState) que atualiza o estado do jogo de acordo c a entrada
--(const id) sendo uma função que atualiza o gameState a cada segundo que se passa,
--como não iremos necessitar de animações no nosso escopo, a função identidade (const id) não faz nada