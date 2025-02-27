-- Monta o tabuleiro básico, 52 casas + finais
montaTabuleiro :: [Casa]
montaTabuleiro = do
  let casaVazia = Casa Normal []
  let principal = take 52 (repeat casaVazia) -- Casas normais
  let finais = take 24 (repeat casaVazia) -- 4 caminhos de 6
  principal ++ finais

-- Adiciona casas especiais ao tabuleiro
adicionaCasasEspeciais :: [Casa] -> [Casa]
adicionaCasasEspeciais tabuleiro =
  let posicoesBoost = [5, 15, 25, 35]  -- Posições com Boost
      posicoesDecline = [10, 20, 30, 40]  -- Posições com Decline
      posicoesDeath = [12, 24, 36, 48]  -- Posições com Death
      posicoesLucky = [8, 16, 32, 40]  -- Posições com Lucky
      posicoesSafe = [0, 13, 26, 39]  -- Posições seguras
      updateCasa pos casa
        | pos `elem` boostPositions = casa { tipoCasa = Boost 3 }
        | pos `elem` declinePositions = casa { tipoCasa = Decline 2 }
        | pos `elem` deathPositions = casa { tipoCasa = Death }
        | pos `elem` luckyPositions = casa { tipoCasa = Lucky }
        | pos `elem` safePositions = casa { tipoCasa = Safe }
        | otherwise = casa
  in zipWith updateCasa [0..] tabuleiro
  
-- Cria um jogador com 4 peças na base
novoJogador :: Cor -> Bool -> Jogador
novoJogador cor ehBot = do
  let pecas = [Peca cor 0, Peca cor 0, Peca cor 0, Peca cor 0] -- Todas na base
  Jogador cor pecas ehBot
  
-- Começa o jogo com jogadores e bots
iniciaJogo :: Int -> Int -> Jogo
iniciaJogo numJogadores numBots = do
  let cores = [Vermelho, Verde, Amarelo, Azul]
  let ehBot = take numBots (repeat True) ++ take (numJogadores - numBots) (repeat False)
  let listaJogadores = zipWith novoJogador cores ehBot -- Mistura cores e bots
  Jogo listaJogadores (casasEspeciais montaTabuleiro) Vermelho 0 False 0 -- Começa com Vermelho
  
-- Rola um dado de 1 a 6
rolaDado :: IO Int
rolaDado = randomRIO (1, 6)
