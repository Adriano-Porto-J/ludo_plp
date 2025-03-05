module Game.LoadSaveState where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import GameTypes

saveGameState :: GameState -> IO ()
saveGameState gameState = do
  B.writeFile "saved_game.json" (encode gameState)
  putStrLn "Jogo salvo com sucesso!"

loadGameState :: IO (Maybe GameState)
loadGameState = do
  exists <- doesFileExist "saved_game.json"
  if exists
    then do
      contents <- B.readFile "saved_game.json"
      return (decode contents)
    else return Nothing