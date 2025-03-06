module Interface.DrawOnBoard where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified GameTypes
import Interface.Index

drawAllPieces::GameTypes.GameState -> Picture
drawAllPieces gameState = pictures (map drawPiece (GameTypes.pieces (gameState)))

drawPiece::GameTypes.Piece -> Picture
drawPiece piece | position < 0 = basePosPiece piece 
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

pieceSprite::GameTypes.Piece -> Picture
pieceSprite piece = pictures [color black (rectangleSolid (side+5) (side+5)), color cor (rectangleSolid (side-1) (side-1))]
    where cor = getPieceColor piece
          side = min cellSize cellSize * 0.75

getPieceColor::GameTypes.Piece -> Color
getPieceColor piece | pieceColor == GameTypes.Red = redColor
               | pieceColor == GameTypes.Blue = blueColor
               | pieceColor == GameTypes.Yellow = yellowColor
               | otherwise = greenColor
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