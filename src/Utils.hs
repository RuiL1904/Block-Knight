module Utils where

import LI12122
import Data.List
import Data.Function

-- | Calcula a altura de um mapa.
alturaMapa :: [(Peca, Coordenadas)] -> Int
alturaMapa [] = 0
alturaMapa [(f,(_,y))] = y + 1 -- Caso exista apenas uma peça declarada.
alturaMapa (p@(f,(x,y)):p'@(f',(x',y')):t)
    | y >= y' = alturaMapa (p:t)
    | otherwise = alturaMapa (p':t)

-- | Calcula a largura de um mapa.
larguraMapa :: [(Peca, Coordenadas)] -> Int
larguraMapa [] = 0
larguraMapa [(f,(x,_))] = x + 1 -- Caso exista apenas uma peça declarada.
larguraMapa (p@(f,(x,y)):p'@(f',(x',y')):t)
    | x >= x' = larguraMapa (p:t)
    | otherwise = larguraMapa (p':t)

-- | Encontra todas as peças declaradas numa linha.
pecasNaLinha :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
pecasNaLinha _ [] = []
pecasNaLinha x (p@(f,(_,y)):t)
    | x == y = p : pecasNaLinha x t
    | otherwise = pecasNaLinha x t

-- | Ordena um mapa por ordem crescente das suas coordenadas.
ordenaMapa :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaMapa [] = []
ordenaMapa l = sortBy (compare `on` snd) l

-- | Transforma a lista dada, numa lista apenas com as segundas componentes (neste caso coordenadas).
listaCoordenadas :: [(Peca, Coordenadas)] -> [Coordenadas]
listaCoordenadas [] = []
listaCoordenadas l = map snd l
