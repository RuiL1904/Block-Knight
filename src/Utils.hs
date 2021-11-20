module Utils where

import LI12122

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
