module Fixtures where

import LI12122

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m2 :: [(Peca, Coordenadas)] -- Mapa 2 das FAQ - Blackboard
m2 =
  [ (Bloco, (0,0)),
    (Bloco, (0,1)),
    (Bloco, (0,3)),
    (Bloco, (0,4)),
    (Bloco, (0,5)),
    (Bloco, (0,6)),
    (Bloco, (1,6)),
    (Bloco, (2,6)),
    (Bloco, (3,6)),
    (Bloco, (4,6)),
    (Bloco, (5,6)),
    (Bloco, (6,6)),
    (Bloco, (7,6)),
    (Bloco, (8,6)),
    (Bloco, (9,6)),
    (Bloco, (10,6)),
    (Bloco, (11,6)),
    (Bloco, (12,6)),
    (Bloco, (13,6)),
    (Bloco, (14,6)),
    (Bloco, (15,6)),
    (Bloco, (16,6)),
    (Bloco, (17,6)),
    (Bloco, (18,6)),
    (Bloco, (19,6)),
    (Bloco, (20,6)),
    (Porta, (1,5)),
    (Bloco, (4,5)),
    (Bloco, (4,4)),
    (Bloco, (8,5)),
    (Caixa, (10,5)),
    (Bloco, (12,5)),
    (Bloco, (12,4)),
    (Caixa, (14,5)),
    (Bloco, (20,5)),
    (Bloco, (20,4)),
    (Bloco, (20,3)),
    (Bloco, (20,2)),
    (Bloco, (20,1)),
    (Bloco, (20,0))
  ]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)
