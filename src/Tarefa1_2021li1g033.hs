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
                        && caixasAFlutuarWrapper l
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
caixasAFlutuarWrapper l = caixasAFlutuar l l -- ^ Transforma a função que recebe dois argumentos (exatamente iguais) numa que recebe apenas um.
caixasAFlutuar :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Bool
caixasAFlutuar _ [] = True
caixasAFlutuar m l@((f,(x,y)):t) =
    case f of
        Caixa -> encontraPeca (x, y + 1) m && caixasAFlutuar m t
        _ -> caixasAFlutuar m t

-- | Auxiliar de caixasAFlutuar - Verifica se existe alguma peça na posição dada (neste caso: y + 1).
encontraPeca :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
encontraPeca _ [] = False
encontraPeca c ((f,c'@(x',y')):t)
    | c == c' && (f == Caixa || f == Bloco) = True -- ^ Verifica também se a peça em questão é uma caixa ou um bloco.
    | otherwise = encontraPeca c t

-- TAREFA 1.4

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

-- | Verifica se existe pelo menos um espaço vazio (declarado ou não) no mapa.
peloMenosUmVazio :: [(Peca, Coordenadas)] -> Bool
peloMenosUmVazio l = existemDeclarados l || existemNaoDeclarados ((alturaMapa l) - 1) l 

-- | Auxiliar de peloMenosUmVazio - Conta quantos vazios declarados existem.
existemDeclarados :: [(Peca, Coordenadas)] -> Bool
existemDeclarados [] = False
existemDeclarados ((f,_):t) =
    case f of
        Vazio -> True
        _ -> existemDeclarados t

-- | Auxiliar de peloMenosUmVazio - Conta quantos vazios não declarados existem.
existemNaoDeclarados :: Int -> [(Peca, Coordenadas)] -> Bool
existemNaoDeclarados (-1) _ = False -- ^ (-1), pois a posição inicial é 0.
existemNaoDeclarados x l = length (pecasNaLinha x l) /= larguraMapa l || existemNaoDeclarados (x - 1) l

-- | Auxiliar de existemNaoDeclarados - Encontra todas as peças declaradas numa linha.
pecasNaLinha :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
pecasNaLinha _ [] = []
pecasNaLinha l (p@(f,(x,y)):t)
    | l == y = p : pecasNaLinha l t
    | otherwise = pecasNaLinha l t
