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

-- | Calcula a altura de um 'Mapa'.

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

-- | Encontra a 'Peca' que se encontra numas determinadas 'Coordenadas'.
encontraPeca :: Coordenadas -> Mapa -> Maybe Peca
encontraPeca _ [] = Nothing
encontraPeca (x,0) (h:t) = encontraNaLinha x h
encontraPeca (x,y) (h:t) = encontraPeca (x,y - 1) t

-- | Auxiliar de 'encontraNaLinha' - Procura pela 'Peca' na linha.
encontraNaLinha :: Int -> [Peca] -> Maybe Peca
encontraNaLinha _ [] = Nothing
encontraNaLinha 0 (h:t) = Just h
encontraNaLinha x (h:t) = encontraNaLinha (x - 1) t

pecaEsperada :: Peca -> Coordenadas -> Mapa -> Bool
pecaEsperada _ _ [] = False
pecaEsperada p c@(x,y) m
    | x < 0 || y < 0 || x > (length (head m)) || y > length m = False
    | encontraPeca c m == Just p = True
    | otherwise = False

pecaImpossivel :: Coordenadas -> Mapa -> Bool
pecaImpossivel c m
    | encontraPeca c m == Nothing = True
    | otherwise = False

encontraBaixo :: Coordenadas -> Mapa -> Coordenadas
encontraBaixo c@(x,y) m
    | pecaEsperada Bloco (x,y + 1) m || pecaEsperada Caixa (x,y + 1) m = c
    | otherwise = encontraBaixo (x,y + 1) m -- Caso não seja nem 'Bloco' nem 'Caixa'.

substituiPeca c p m = substituiPecaWrapper c p m []

substituiPecaWrapper :: Coordenadas -> Peca -> Mapa -> Mapa -> Mapa
substituiPecaWrapper (x,0) p (h:t) m = m ++ [substituiPecaNaLinha x p h] ++ t
substituiPecaWrapper (x,y) p (h:t) m = substituiPecaWrapper (x,y - 1) p t (m ++ [h])

substituiPecaNaLinha x p l = substituiPecaNaLinhaWrapper x p l []

substituiPecaNaLinhaWrapper :: Int -> Peca -> [Peca] -> [Peca] -> [Peca]
substituiPecaNaLinhaWrapper 0 p (h:t) l = l ++ [p] ++ t
substituiPecaNaLinhaWrapper x p (h:t) l = substituiPecaNaLinhaWrapper (x - 1) p t (l ++ [h])
