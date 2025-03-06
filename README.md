 LUDO MODIFICADO 🎲
Especificação do Projeto: https://docs.google.com/document/d/1l2ZcZusR_4niFJZ7k7ZHK85utCCWVtktkYYqTv4uPMQ/edit?tab=t.0


Desenvolvedores:
- Adriano Porto
- André Tharssys
- Arthur Vidal
- Leonardo Mota

Como rodar:
> $ git clone https://github.com/Adriano-Porto-J/ludo_plp_haskell.git
> $ cd ludo_plp_haskell/
> $ cabal clean
> $ cabal build
> $ cabal run   



O que é o Ludo Modificado?

Nosso Ludo Modificado conta com algumas novidades em relação ao jogo original: Existem algumas casas no tabuleiro que são especiais, ou seja, podem oferecer vantagens ou desvantagens aos jogadores!
Casa Lucky: O jogador que tiver uma peça nessa casa poderá mandar uma peça inimiga diretamente para a base
Casa Safe: Essa casa faz com que as peças sejam intocáveis (não podem ser capturadas nem sofrer o efeito da casa lucky)
Casa Boost: O player avançará um número extra de casas no tabuleiro (3 casas)
Casa Decline: O player retrocederá um número de casas no tabuleiro (3 casas)
Casa Death: O player terá a sua peça retornada à base

O jogo pode ser jogado tanto pelo terminal quanto pela interface gráfica (basta acessar o main e trocar (debug = True) para jogar pelo terminal e (debug = False) para jogar pela interface gráfica

O jogo contém bots (até 3). Eles foram programados para: 
1- Priorizar vencer
2- Capturar peças inimigas
3- Andar casas 

Também é possível salvar e carregar o jogo.
