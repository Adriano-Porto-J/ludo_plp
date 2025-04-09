module Interface.DrawMenu where 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Interface.Index
import GameTypes

-- Desenho Menu Inicial
drawMenuInicial :: Picture
drawMenuInicial = pictures 
    [color (white) (rectangleSolid boardSize boardSize)
    , drawTituloLudo
    , drawButtonNovoJogo
    , drawButtonCarregarJogoSalvo
    ]

-- Placeholder para o título do Menu Inicial
drawTituloLudo :: Picture
drawTituloLudo = translate (-180) 180 $ scale 0.5 0.5 (text "Ludo Game")

-- Placeholder para o botão de Novo Jogo
drawButtonNovoJogo :: Picture
drawButtonNovoJogo = pictures 
    [translate 0 50 $ color blueColor (rectangleSolid 200 50)
     ,translate (-70) 40 (scale 0.2 0.2 (text "Novo Jogo"))
    ]

-- Placeholder para o botão de Carregar Jogo Salvo
drawButtonCarregarJogoSalvo :: Picture
drawButtonCarregarJogoSalvo = pictures 
    [translate 0 (-50) $ color greenColor (rectangleSolid 300 50)
    , translate (-130) (-60) (scale 0.2 0.2 (text "Carregar Jogo Salvo"))
    ]

--Desenho Menu Players
drawMenuPlayers :: Picture
drawMenuPlayers = pictures 
    [color (white) (rectangleSolid boardSize boardSize)
    , drawTituloSelecioneQuantPlayers
    , drawButtonsQuantPlayers
    ]

-- Título do Menu de Seleção de Quantidade de Players
drawTituloSelecioneQuantPlayers :: Picture
drawTituloSelecioneQuantPlayers = translate (-280) 180 $ scale 0.25 0.25 (text "Selecione a Quantidade de Jogadores")

-- Botões de Seleção de Quantidade de Players (1 a 4 jogadores)
drawButtonsQuantPlayers :: Picture
drawButtonsQuantPlayers = pictures 
    [translate (-100) 80 $ color blueColor (rectangleSolid 50 50),translate (-110) 70 (scale 0.2 0.2 (text "1")),
     translate 100 80 $ color blueColor (rectangleSolid 50 50),translate 90 70 (scale 0.2 0.2 (text "2")),
     translate (-100) (-40) $ color blueColor (rectangleSolid 50 50),translate (-110) (-50) (scale 0.2 0.2 (text "3")),
     translate 100 (-40) $ color blueColor (rectangleSolid 50 50), translate 90 (-50) (scale 0.2 0.2 (text "4"))]

--Desenho Menu bots
drawMenuBotsUmPlayer :: Picture
drawMenuBotsUmPlayer = pictures
    [color (white) (rectangleSolid boardSize boardSize)
    , drawButtonsBotsUmPlayer
    , drawTituloSelecioneQuantBots
    ]

drawMenuBotsDoisPlayer :: Picture
drawMenuBotsDoisPlayer = pictures
    [color (white) (rectangleSolid boardSize boardSize)
    , drawButtonsBotsDoisPlayer
    , drawTituloSelecioneQuantBots
    ]

drawMenuBotsTresPlayer :: Picture
drawMenuBotsTresPlayer = pictures
    [color (white) (rectangleSolid boardSize boardSize)
    , drawButtonsBotsTresPlayer
    , drawTituloSelecioneQuantBots
    ]

-- Título do Menu de Seleção de Quantidade de Bots
drawTituloSelecioneQuantBots :: Picture
drawTituloSelecioneQuantBots = translate (-280) 180 $ scale 0.25 0.25 (text "Selecione a Quantidade de Bots")

drawButtonsBotsUmPlayer :: Picture
drawButtonsBotsUmPlayer = pictures 
    [translate (-100) 80 $ color blueColor (rectangleSolid 50 50),translate (-110) 70 (scale 0.2 0.2 (text "1"))
     ,translate 100 80 $ color blueColor (rectangleSolid 50 50),translate 90 70 (scale 0.2 0.2 (text "2"))
     ,translate (-100) (-40) $ color blueColor (rectangleSolid 50 50),translate (-110) (-50) (scale 0.2 0.2 (text "3"))
    ]

drawButtonsBotsDoisPlayer :: Picture
drawButtonsBotsDoisPlayer = pictures 
    [translate (-100) 80 $ color blueColor (rectangleSolid 50 50),translate (-110) 70 (scale 0.2 0.2 (text "0"))
     ,translate 100 80 $ color blueColor (rectangleSolid 50 50),translate 90 70 (scale 0.2 0.2 (text "1"))
     ,translate (-100) (-40) $ color blueColor (rectangleSolid 50 50),translate (-110) (-50) (scale 0.2 0.2 (text "2"))
    ]

drawButtonsBotsTresPlayer :: Picture
drawButtonsBotsTresPlayer = pictures 
    [translate (-100) 80 $ color blueColor (rectangleSolid 50 50),translate (-110) 70 (scale 0.2 0.2 (text "0"))
     ,translate 100 80 $ color blueColor (rectangleSolid 50 50),translate 90 70 (scale 0.2 0.2 (text "1"))
     ]
    
