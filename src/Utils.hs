module Utils where

import LI12122
import Data.List
import Data.Function

-- | Calcula a altura de um 'Mapa'.
alturaMapa :: [(Peca, Coordenadas)] -> Int
alturaMapa [] = 0
alturaMapa [(_,(_,y))] = y
alturaMapa (p@(f,(x,y)):p'@(f',(x',y')):t)
    | y >= y' = alturaMapa (p:t)
    | otherwise = alturaMapa (p':t)

-- | Calcula a largura de um 'Mapa'.
larguraMapa :: [(Peca, Coordenadas)] -> Int
larguraMapa [] = 0
larguraMapa [(_,(x,_))] = x
larguraMapa (p@(f,(x,y)):p'@(f',(x',y')):t)
    | x >= x' = larguraMapa (p:t)
    | otherwise = larguraMapa (p':t)

-- | Encontra todas as peças declaradas numa linha.
pecasNaLinha :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
pecasNaLinha _ [] = []
pecasNaLinha x (p@(f,(_,y)):t)
    | x == y = p : pecasNaLinha x t
    | otherwise = pecasNaLinha x t

-- | Ordena um mapa por ordem crescente das suas 'Coordenadas'.
ordenaMapa :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaMapa [] = []
ordenaMapa l = sortBy (compare `on` snd) l

-- | Transforma a lista dada numa lista apenas com as segundas componentes (neste caso 'Coordenadas').
listaCoordenadas :: [(Peca, Coordenadas)] -> [Coordenadas]
listaCoordenadas [] = []
listaCoordenadas l = map snd l

menorOrdenadaWrapper l = menorOrdenada (listaCoordenadas l)

-- | Calcula a menor ordenada de um 'Mapa'.
menorOrdenada :: [Coordenadas] -> Int
menorOrdenada [] = 0
menorOrdenada [(_,y)] = y
menorOrdenada (c@(x,y):c'@(x',y'):t)
    | y <= y' = menorOrdenada (c:t)
    | otherwise = menorOrdenada (c':t)

-- | Retira a 'Peca' que se encontra nas 'Coordenadas' dadas.
retiraPeca :: Coordenadas -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
retiraPeca _ [] = []
retiraPeca (x,y) (p@(f,(x',y')):t)
    | x == x' && y == y' = t
    | otherwise = p : retiraPeca (x,y) t

-- | Encontra todas as peças declaradas numa coluna.
pecasNaColuna :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
pecasNaColuna _ [] = []
pecasNaColuna x (p@(f,(x',_)):t)
    | x == x' = p : pecasNaColuna x t
    | otherwise = pecasNaColuna x t

-- | Extrai apenas os 'Bloco' que existem na lista.
extraiBlocos :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
extraiBlocos [] = []
extraiBlocos (p@(f,_):t) =
    case f of
        Bloco -> p : extraiBlocos t
        _ -> extraiBlocos t

-- | Retira a coluna dada à lista.
retiraColuna :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
retiraColuna _ [] = []
retiraColuna x (p@(f,(x',y)):t)
    | x == x' = retiraColuna x t
    | otherwise = p : retiraColuna x t

-- | Verifica se um par de 'Coordenadas' pertence à lista dada.
coordenadaPertence :: Coordenadas -> [Coordenadas] -> Bool
coordenadaPertence _ [] = False
coordenadaPertence c (c':t)
    | c == c' = True
    | otherwise = coordenadaPertence c t