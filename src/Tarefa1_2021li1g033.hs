{- |
Module      : Tarefa1_2021li1g033
Description : Validação de um potencial mapa.
Copyright   : (c) Rui Lopes Martins <a100643@alunos.uminho.pt>
              Diogo Ribeiro Vassalo de Abreu <a100646@alunos.uminho.pt>

O objetivo desta tarefa é implementar uma função que dada uma lista de peças e respetivas coordenadas testa se estas __definem corretamente um mapa__. Para tal, a lista deverá __satisfazer os seguintes critérios__ (1.1 - 1.5).
-}
module Tarefa1_2021li1g033 (
    -- **
    validaPotencialMapa,
    -- ** 1.1 - Não haver mais do que uma declaração de peça para a mesma posição.
    coordenadaDiferente, pertenceCauda, coordenadaPositiva,
    -- ** 1.2 - Declarar exatamente uma porta.
    portaUnica, contaPortas,
    -- ** 1.3 - Todas as caixas devem estar posicionadas em cima de outra caixa ou bloco, i.e. não podem haver caixas a "flutuar".
    caixasAFlutuar, procuraPeca,
    -- ** 1.4 - Devem existir espaços vazios (no mínimo um), i.e. o mapa não pode estar totalmente preenchido por caixas, blocos e porta.
    peloMenosUmVazio, existemDeclarados, existemNaoDeclarados
    ) where

import LI12122
import Utils

{- | Verifica se o mapa é válido.

== Exemplos de utilização:

>>> validaPotencialMapa [(Porta,(0,3)),(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4)),(Caixa,(4,3)),(Bloco,(5,4)),(Bloco,(6,4)),(Bloco,(6,3)),(Bloco,(6,2)),(Bloco,(6,1))]
True
>>> validaPotencialMapa []
False
-}
validaPotencialMapa :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- ^ Resultado.
validaPotencialMapa l = coordenadaDiferente l  
                        && coordenadaPositiva l
                        && portaUnica l 
                        && caixasAFlutuarWrapper l
                        && peloMenosUmVazio l

{- | Verifica se as peças têm todas coordenadas diferentes.

== Exemplos de utilização:

>>> coordenadaDiferente [(Bloco, (0,2)),(Bloco,(0,3))]
True
>>> coordenadaDiferente [(Caixa,(0,1)),(Bloco,(0,1))]
False
-}
coordenadaDiferente :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- ^ Resultado.
coordenadaDiferente [] = True
coordenadaDiferente l@((_,s):t) = pertenceCauda s t && coordenadaDiferente t

{- | Auxiliar de 'coordenadaDiferente' - Verifica se uma coordenada não pertence à cauda (tail).

== Exemplos de utilização:

>>> pertenceCauda (1,2) [(Bloco,(0,3)),(Caixa,(1,2))]
True
>>> pertenceCauda (0,0) [(Bloco,(3,2)),(Caixa,(1,1))]
False
-}
pertenceCauda :: 
    Coordenadas -- ^ Uma lista de pares de 'Int'.
    -> [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'. 
    -> Bool -- ^ Resultado.
pertenceCauda _ [] = True
pertenceCauda c ((_,s):t) = c /= s && pertenceCauda c t

{- | Verifica se as coordenadas são todas positivas, visto que o mapa tem de começar nas coordenadas (0,0).

== Exemplos de utilização.

>>> coordenadaPositiva [(Bloco,(0,2)),(Bloco,(0,4))]
True
>>> coordenadaPositiva [(Caixa,(-1,2)),(Bloco,(2,-1))]
False
>>> coordenadaPositiva [(Porta,(-1,-2)),(Bloco,(2,1))]
False
-}
coordenadaPositiva :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'. 
    -> Bool -- ^ Resultado.
coordenadaPositiva [] = True
coordenadaPositiva ((_,(x,y)):t) = x >= 0 && y >= 0 && coordenadaPositiva t

{- | Verifica se existe uma e uma só porta.

== Exemplos de utilização:

>>> portaUnica [(Porta,(1,2)),(Bloco,(1,3)),(Bloco,(1,4))]
True
>>> portaUnica [(Porta,(1,2)),(Porta,(1,3)),(Caixa,(2,4))]
False
-}
portaUnica :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'. 
    -> Bool -- ^ Resultado.
portaUnica l = contaPortas l == 1

-- | Auxiliar de 'portaUnica' - Conta quantas portas existem no mapa.
contaPortas :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Int -- ^ Resultado.
contaPortas [] = 0
contaPortas ((f,_):t) =
    case f of
        Porta -> 1 + contaPortas t
        _ -> contaPortas t

caixasAFlutuarWrapper l = caixasAFlutuar l l -- > Transforma a função que recebe dois argumentos (exatamente iguais) numa que recebe apenas um.

{- | Verifica se não existem caixas a flutuar.

== Exemplos de utilização:

>>> caixasAFlutuarWrapper [(Bloco,(0,1)),(Caixa,(0,0))]
True
>>> caixasAFlutuarWrapper [(Bloco,(0,0)),(Caixa,(1,2))]
False

__Observação:__ 

@ 
caixasAFlutuarWrapper l = caixasAFlutuar l l -- Onde l é uma lista de de pares de 'Peca' e de 'Coordenadas'.
@
-}
caixasAFlutuar :: 
    [(Peca, Coordenadas)] -- Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> [(Peca, Coordenadas)] -- Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- Resultado.
caixasAFlutuar _ [] = True
caixasAFlutuar m l@((f,(x,y)):t) =
    case f of
        Caixa -> procuraPeca (x, y + 1) m && caixasAFlutuar m t
        _ -> caixasAFlutuar m t

-- | Auxiliar de 'caixasAFlutuar' - Verifica se existe alguma peça na posição dada (neste caso: y + 1).
procuraPeca :: 
    Coordenadas -- ^ Um par de 'Int'.
    -> [(Peca, Coordenadas)]  -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- ^ Resultado.
procuraPeca _ [] = False
procuraPeca c ((f,c'@(x',y')):t)
    | c == c' && (f == Caixa || f == Bloco) = True -- > Verifica também se a peça em questão é uma caixa ou um bloco.
    | otherwise = procuraPeca c t

{- | Verifica se existe pelo menos um espaço vazio (declarado ou não) no mapa.

== Exemplos de utilização:

>>> peloMenosUmVazio [(Vazio,(1,2)),(Bloco,(3,3))]
True
>>> peloMenosUmVazio [(Bloco,(0,2)),(Bloco,(0,3))] -- Vazio definido por omissão.
True
>>> peloMenosUmVazio [(Bloco,(0,0)),(Bloco(1,0))]
False
-}
peloMenosUmVazio :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- ^ Resultado.
peloMenosUmVazio l = existemDeclarados l || existemNaoDeclarados ((alturaMapa l) - 1) l 

-- | Auxiliar de 'peloMenosUmVazio' - Conta quantos vazios declarados existem.
existemDeclarados :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- ^ Resultado.
existemDeclarados [] = False
existemDeclarados ((f,_):t) =
    case f of
        Vazio -> True
        _ -> existemDeclarados t

-- | Auxiliar de 'peloMenosUmVazio' - Conta quantos vazios não declarados existem.
existemNaoDeclarados :: 
    Int -- ^ Um 'Int'.
    -> [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- ^ Resultado.
existemNaoDeclarados (-1) _ = False -- > (-1), pois a posição inicial é 0.
existemNaoDeclarados x l = length (pecasNaLinha x l) /= larguraMapa l || existemNaoDeclarados (x - 1) l
