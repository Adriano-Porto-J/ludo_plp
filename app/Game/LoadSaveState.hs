module Game.LoadSaveState where

-- Importação das bibliotecas necessárias
import Data.Aeson (decode, encode) -- Funções para codificar e decodificar JSON
import qualified Data.ByteString.Lazy as B -- Biblioteca para manipulação de ByteStrings de forma preguiçosa
import GHC.Generics (Generic) -- Para utilizar a classe 'Generic', necessário para a serialização de dados com Aeson
import System.Directory (doesFileExist) -- Função para verificar se um arquivo existe no sistema
import GameTypes -- Importação dos tipos de dados do jogo (não mostrado, mas inclui o tipo GameState)

-- Função para salvar o estado do jogo em um arquivo
saveGameState :: GameState -> IO ()
saveGameState gameState = do
  -- Codifica o estado do jogo como JSON e grava no arquivo "saved_game.json"
  B.writeFile "saved_game.json" (encode gameState)
  -- Imprime uma mensagem informando que o jogo foi salvo com sucesso
  putStrLn "Jogo salvo com sucesso!"

-- Função para carregar o estado do jogo a partir de um arquivo
loadGameState :: IO (Maybe GameState)
loadGameState = do
  -- Verifica se o arquivo "saved_game.json" existe
  exists <- doesFileExist "saved_game.json"
  if exists
    then do
      -- Se o arquivo existe, lê o conteúdo e tenta decodificar o JSON para um valor do tipo GameState
      contents <- B.readFile "saved_game.json"
      return (decode contents) -- Retorna o estado do jogo carregado (pode ser 'Just gameState' ou 'Nothing' se a decodificação falhar)
    else 
      -- Se o arquivo não existe, retorna 'Nothing', indicando que não há um estado salvo
      return Nothing
