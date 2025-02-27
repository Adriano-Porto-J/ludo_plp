-----Alterar esse main dps

main :: IO ()
main = do
  putStrLn "Vamos jogar Ludo? Quantos jogadores (2-4)?"
  numJogadores <- getLine
  let totalJogadores = read numJogadores :: Int
  putStrLn "Quantos bots você quer? (0-2)"
  numBots <- getLine
  let totalBots = read numBots :: Int
  putStrLn "Beleza, o jogo vai comecar!"
  let jogoInicial = iniciaJogo totalJogadores totalBots
  jogaLudo jogoInicial


jogaLudo :: Jogo -> IO ()
jogaLudo jogo
  | (\(Jogo _ _ _ _ fim _) -> fim) jogo = do
      putStrLn "EITA, ALGUÉM GANHOU!!! Fim de jogo!"
  | otherwise = do
      let (Jogo jogadores tab vez dado _ seis) = jogo
      putStrLn $ "Agora é a vez do " ++ show vez ++ ". Último dado: " ++ show dado
      novoDado <- rolaDado
      putStrLn $ "Dado rolou: " ++ show novoDado
      let jogoComDado = Jogo jogadores tab vez novoDado False seis
      let contaSeis = if novoDado == 6 then seis + 1 else 0
      let jogoAtualizado = jogoComDado {sixesInRow = contaSeis}
      if contaSeis >= 3
        then do
          putStrLn "TRÊS SEIS SEGUIDOS!!! Perdeu a vez, coitado!"
          jogaLudo $ jogoAtualizado {turn = proximoJogador jogoAtualizado, sixesInRow = 0}
        else do
          let quemJoga = head (filter (\(Jogador c _ _) -> c == vez) jogadores)
          let (Jogador _ pecasJogador ehBot) = quemJoga
          if ehBot
            then do
              putStrLn "Bot jogando... vamos ver o que ele faz!"
              jogoDepoisBot <- jogadaBot jogoAtualizado
              let jogoFinal = veSeGanhou $ if novoDado == 6 then jogoDepoisBot else jogoDepoisBot {turn = proximoJogador jogoDepoisBot}
              jogaLudo jogoFinal
            else do
              putStrLn "Suas peças estão em: " ++ show (map (\(Peca _ p) -> p) pecasJogador)
              putStrLn "Escolha: 0-3 (mover peça), 's' (salvar), 'c' (carregar)"
              escolha <- getLine
              if escolha == "s"
                then do
                  putStrLn "Salvando o jogo agora..."
                  salvaJogo jogoAtualizado
                  jogaLudo jogoAtualizado
                else if escolha == "c"
                  then do
                    putStrLn "Carregando o jogo salvo..."
                    jogoCarregado <- carregaJogo
                    jogaLudo jogoCarregado
                  else do
                    let numeroPeca = read escolha :: Int
                    if numeroPeca >= 0 && numeroPeca <= 3 -- Checa se é válido
                      then do
                        let pecaEscolhida = pecasJogador !! numeroPeca
                        let jogoDepoisMover = moverPeca jogoAtualizado pecaEscolhida ((\(Peca _ p) -> p) pecaEscolhida + novoDado)
                        let jogoFinal = veSeGanhou $ if novoDado == 6 then jogoDepoisMover else jogoDepoisMover {turn = proximoJogador jogoDepoisMover}
                        jogaLudo jogoFinal
                      else do
                        putStrLn "Número errado, cara! Tenta de novo."
                        jogaLudo jogoAtualizado


board :: Jogo -> [Casa]
board (Jogo _ b _ _ _ _) = b
