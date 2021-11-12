{- |
Module      : Tarefa1_2021li1g033
Description : Validação de um potencial mapa
Copyright   : Rui Lopes Martins <a100643@alunos.uminho.pt>;
            : Diogo Ribeiro Vassalo de Abreu <a100646@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g033 where

import LI12122

-- | Verifica se o 'Mapa' é válido.
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa l@(h:t) = pecaUnica l

-- | Verifica se as peças têm todas coordenadas diferentes.
pecaUnica :: [(Peca, Coordenadas)] -> Bool
pecaUnica [] = True
pecaUnica ((_,s):t) = pertenceCauda s t && pecaUnica t

-- | Auxiliar de pecaUnica - Verifica se uma coordenada pertence à cauda (tail).
pertenceCauda :: Coordenadas -> [(Peca,Coordenadas)] -> Bool
pertenceCauda _ [] = True
pertenceCauda c ((_,s):t) = c /= s && pertenceCauda c t

