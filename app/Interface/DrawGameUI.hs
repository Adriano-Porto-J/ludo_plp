module Interface.DrawGameUI where 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Interface.Index
import GameTypes

-- Desenha o texto que indica qual o jogador da atual rodada
drawPlayerText::GameTypes.GameState -> Picture
drawPlayerText gameState | currentPlayer == GameTypes.Red = translate (-3 * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color red (text "Jogador Vermelho")
                         | currentPlayer == GameTypes.Yellow = translate (-3 * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color yellow (text "Jogador Amarelo")
                         | currentPlayer == GameTypes.Green = translate (-3 * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color green (text "Jogador Verde")
                         | otherwise = translate (-3 * cellSize) (8 * cellSize) $ scale 0.20 0.20 $ color blue (text "Jogador Azul")
    where currentPlayer = GameTypes.currentPlayer gameState

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