-- Salva o jogo num arquivo
salvaJogo :: Jogo -> IO ()
salvaJogo jogo = writeFile "meu_ludo.txt" (show jogo)

-- Carrega o jogo salvo
carregaJogo :: IO Jogo
carregaJogo = do
  conteudo <- readFile "meu_ludo.txt"
  return (read conteudo :: Jogo)
