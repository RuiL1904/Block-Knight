module Utils where

import LI12122
import Data.List (sort)

-- | Calcula a altura de um mapa.
alturaMapa :: [(Peca, Coordenadas)] -> Int
alturaMapa [] = 0
alturaMapa [(f,(_,y))] = y + 1 -- ^ Caso exista apenas uma peça declarada.
alturaMapa (p@(f,(x,y)):p'@(f',(x',y')):t)
    | y >= y' = alturaMapa (p:t)
    | otherwise = alturaMapa (p':t)

-- | Calcula a largura de um mapa.
larguraMapa :: [(Peca, Coordenadas)] -> Int
larguraMapa [] = 0
larguraMapa [(f,(x,_))] = x + 1 -- ^ Caso exista apenas uma peça declarada.
larguraMapa (p@(f,(x,y)):p'@(f',(x',y')):t)
    | x >= x' = larguraMapa (p:t)
    | otherwise = larguraMapa (p':t)

-- | Encontra todas as peças declaradas numa linha.
pecasNaLinha :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
pecasNaLinha _ [] = []
pecasNaLinha x (p@(f,(_,y)):t)
    | x == y = p : pecasNaLinha x t
    | otherwise = pecasNaLinha x t

-- | Cria uma lista com todas as peças de um mapa.
listaPecas :: [(Peca, Coordenadas)] -> [Peca]
listaPecas l = map fst l
-- | Cria uma lista de pares com todas as coordenadas de um mapa.

sortedCoordenadas :: [(Peca, Coordenadas)] -> [Coordenadas]
sortedCoordenadas l = sort (map snd l)

-- | Junta duas listas num par - uma de peças e outra de coordenadas.
juntaListas :: [Peca] -> [Coordenadas] -> [(Peca, Coordenadas)]
juntaListas [] _ = []
juntaListas _ [] = []
juntaListas (h:t) (h':t') = (h,h') : juntaListas t t'
