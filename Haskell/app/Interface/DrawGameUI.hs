module Interface.DrawGameUI where 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Interface.Index
import Game.Index
import GameTypes
import Graphics.Gloss (Picture, greyN, Color, yellow)
import GHC.Real (RealFrac(ceiling))
import Data.String

-- Desenha o texto que indica qual o jogador da atual rodada
drawPlayerText::GameTypes.GameState -> Picture
drawPlayerText gameState | currentPlayer == GameTypes.Red = translate ((-3 + (botX gameState)) * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color redColor (text ("Jogador Vermelho" ++ (botText gameState)))
                         | currentPlayer == GameTypes.Yellow = translate ((-3 + (botX gameState)) * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color yellowColor (text ("Jogador Amarelo" ++ (botText gameState)))
                         | currentPlayer == GameTypes.Green = translate ((-3 + (botX gameState)) * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color greenColor (text ("Jogador Verde" ++ (botText gameState)))
                         | otherwise = translate ((-3 + (botX gameState)) * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color blueColor (text ("Jogador Azul" ++ (botText gameState)))
    where currentPlayer = GameTypes.currentPlayer gameState

drawLuckyText::GameTypes.GameState -> Picture
drawLuckyText gameState = if Game.Index.isBotTurn gameState == False 
                            then if GameTypes.wasLuckyMove gameState == True
                              then translate ((-9 + (botX gameState)) * cellSize) (-10 * cellSize) 
                                   $ scale 0.20 0.20 $ color black (text "Escolha uma peca adversaria para ser capturada")
                              else Blank
                          else Blank

botText::GameState -> [Char]
botText gameState = if (isBotTurn gameState) == True then " (Bot) Rode o dado e clique no tabuleiro para prosseguir" else ""

botX::GameState -> Float
botX gameState = if (isBotTurn gameState) == True then -9 else 0

-- Desenha o botão para rolar o dado
drawButton :: Picture
drawButton = pictures
    [translate  (-3.5 * cellSize) (-8 * cellSize) $ color black (rectangleSolid (6 * cellSize) (2 * cellSize))-- Fundo do botão
    , translate (-5.5 * cellSize) (-8 * cellSize) $ scale 0.20 0.20 $ color white (text "Rolar Dado")  -- Texto
    ]


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

drawButtonSaveTheGame :: Picture
drawButtonSaveTheGame = pictures 
  [ translate (5 * cellSize) (-8 * cellSize) $ color yellow (rectangleSolid (3 * cellSize) (2 * cellSize))-- Fundo do botão
    , translate (4 * cellSize) (-8 * cellSize) $ scale 0.20 0.20 $ color black (text "Save")  -- Texto
  ]

-- Desenha uma picture com o vencedor 
drawWinnerPlayer :: GameTypes.GameState -> Picture
drawWinnerPlayer gameState = case winnerColor of
                                    GameTypes.Red -> translate (-4 * cellSize) (10 * cellSize) $ scale 0.20 0.20 $ color black (text "Jogador Vermelho Ganhou")
                                    GameTypes.Yellow -> translate (-4 * cellSize) (10 * cellSize) $ scale 0.20 0.20 $ color black (text "Jogador Amarelo Ganhou")
                                    GameTypes.Green -> translate (-4 * cellSize) (10 * cellSize) $ scale 0.20 0.20 $ color black (text "Jogador Verde Ganhou")
                                    GameTypes.Blue -> translate (-4 * cellSize) (10 * cellSize) $ scale 0.20 0.20 $ color black (text "Jogador azul Ganhou")
                                    _ -> blank  -- Caso inválido (não desenha nada)
    where winnerColor = GameTypes.winnerColor gameState
  
  