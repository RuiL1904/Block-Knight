{- |
Module      : Tarefa1_2021li1g033
Description : Validação de um potencial mapa
Copyright   : Rui Lopes Martins <a100643@alunos.uminho.pt>;
            : Diogo Ribeiro Vassalo de Abreu <a100646@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g033 where

import LI12122

-- | Verifica se o mapa é válido.
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa l = coordenadaDiferente l  
                        && coordenadaPositiva l
                        && portaUnica l 
                        && caixasAFlutuar l 
                        && peloMenosUmVazio l

-- TAREFA 1.1

-- | Verifica se as peças têm todas coordenadas diferentes.
coordenadaDiferente :: [(Peca, Coordenadas)] -> Bool
coordenadaDiferente [] = True
coordenadaDiferente l@((_,s):t) = pertenceCauda s t && coordenadaDiferente t

-- | Auxiliar de coordenadaDiferente - Verifica se uma coordenada pertence à cauda (tail).
pertenceCauda :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
pertenceCauda _ [] = True
pertenceCauda c ((_,s):t) = c /= s && pertenceCauda c t

-- | Verifica se as coordenadas são todas positivas, visto que o mapa começa nas coordenadas (0,0).
coordenadaPositiva :: [(Peca, Coordenadas)] -> Bool
coordenadaPositiva [] = True
coordenadaPositiva ((_,(x,y)):t) = x >= 0 && y >= 0 && coordenadaPositiva t

-- TAREFA 1.2

-- | Verifica se existe uma e uma só porta.
portaUnica :: [(Peca, Coordenadas)] -> Bool
portaUnica l = contaPortas l == 1

-- | Auxiliar de portaUnica - Conta quantas portas existem no mapa.
contaPortas :: [(Peca, Coordenadas)] -> Int
contaPortas [] = 0
contaPortas ((f,_):t) =
    case f of
        Porta -> 1 + contaPortas t
        _ -> contaPortas t

-- TAREFA 1.3

-- | Verifica se não existem caixas a flutuar.
caixasAFlutuar :: [(Peca, Coordenadas)] -> Bool
caixasAFlutuar [] = True
caixasAFlutuar l@((f,(x,y)):t) =
    case f of
        Caixa -> encontraPeca (x, y + 1) l && caixasAFlutuar t
        _ -> caixasAFlutuar t

-- | Auxiliar de caixasAFlutuar - Verifica se existe alguma peça na posição dada (neste caso: y + 1).
encontraPeca :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
encontraPeca _ [] = False
encontraPeca c ((f,c'@(x',y')):t)
    | c == c' && (f == Caixa || f == Bloco) = True -- ^ Verifica também se a peça em questão é uma caixa ou um bloco.
    | otherwise = encontraPeca c t
