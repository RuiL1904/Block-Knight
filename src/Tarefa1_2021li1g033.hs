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
    caixasAFlutuar, caixasAFlutuarWrapper, procuraPeca,
    -- ** 1.4 - Devem existir espaços vazios (no mínimo um), i.e. o mapa não pode estar totalmente preenchido por caixas, blocos e porta.
    peloMenosUmVazio, existemDeclarados, existemNaoDeclarados,
    -- ** 1.5 - A base do mapa deve ser composta por blocos, i.e. deve existir um chão ao longo do mapa.
    validaChao, validaChaoWrapper
    ) where

import LI12122
import Utils

{- | Verifica se o mapa é válido.

=== Exemplos de utilização:

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
                        && caixasAFlutuar l
                        && peloMenosUmVazio l
                        && validaChao l

{- | Verifica se as peças têm todas coordenadas diferentes.

=== Exemplos de utilização:

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

=== Exemplos de utilização:

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

=== Exemplos de utilização.

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

=== Exemplos de utilização:

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

-- | Função principal - ver 'caixasAFlutuarWrapper'.
caixasAFlutuar l = caixasAFlutuarWrapper l l -- > Transforma a função que recebe dois argumentos (exatamente iguais) numa que recebe apenas um.

{- | Verifica se não existem caixas a flutuar.

=== Exemplos de utilização:

>>> caixasAFlutuar [(Bloco,(0,1)),(Caixa,(0,0))]
True
>>> caixasAFlutuar [(Bloco,(0,0)),(Caixa,(1,2))]
False

-}
caixasAFlutuarWrapper :: 
    [(Peca, Coordenadas)] -- Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> [(Peca, Coordenadas)] -- Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- Resultado.
caixasAFlutuarWrapper _ [] = True
caixasAFlutuarWrapper m l@((f,(x,y)):t) =
    case f of
        Caixa -> procuraPeca (x, y + 1) m && caixasAFlutuarWrapper m t
        _ -> caixasAFlutuarWrapper m t

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

=== Exemplos de utilização:

>>> peloMenosUmVazio [(Vazio,(1,2)),(Bloco,(3,3))]
True
>>> peloMenosUmVazio [(Bloco,(0,0)),(Bloco(1,0))]
False
-}
peloMenosUmVazio :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- ^ Resultado.
peloMenosUmVazio l = existemDeclarados l || existemNaoDeclarados l

-- | Auxiliar de 'peloMenosUmVazio' - Verifica se existe pelo menos um 'Vazio' declarado.
existemDeclarados :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- ^ Resultado.
existemDeclarados [] = False
existemDeclarados ((f,_):t) =
    case f of
        Vazio -> True
        _ -> existemDeclarados t

-- | Auxiliar de 'peloMenosUmVazio' - Verifica se existem 'Vazio' não declarados.
existemNaoDeclarados :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- ^ Resultado.
existemNaoDeclarados [] = False
existemNaoDeclarados l = length l < alturaMapa l * larguraMapa l -- > Compara o tamanho da lista com as dimensões do mapa.

-- | Função principal - ver 'validaChaoWrapper'.
validaChao l = validaChaoWrapper 0 (extraiBlocos l)

validaChaoWrapper :: 
    Int -- ^ Um 'Int' - neste caso serve como acumulador.
    -> [(Peca, Coordenadas)]  -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Bool -- ^ Resultado.

-- | Verifica se o chão do mapa é contínuo e se composto por 'Bloco'.
validaChaoWrapper x l
    | x == ((larguraMapa l) - 1) = True
    {- Referente às 3 primeiras condições abaixo:
    Verifica se as 'Coordenadas' que estamos a testar estão dentro da lista das 'Coordenadas', se sim é porque o 'Bloco' existe.
    -} 
    | coordenadaPertence (x + 1, alturaMapa (pecasNaColuna x l)) (listaCoordenadas l) = validaChaoWrapper (x + 1) (retiraColuna x l)
    | coordenadaPertence (x + 1, alturaMapa (pecasNaColuna x l) + 1) (listaCoordenadas l) = validaChaoWrapper (x + 1) (retiraColuna x l)
    | coordenadaPertence (x + 1, alturaMapa (pecasNaColuna x l) - 1) (listaCoordenadas l) = validaChaoWrapper (x + 1) (retiraColuna x l)
    {- Referente às 2 primeiras condições abaixo:
    Verifica se a menor ordenada (y) da "primeira" coluna está mais abaixo do que a menor ordenada (y) da coluna seguinte, 
    caso isso aconteça e exista um 'Bloco' acima do 'Bloco' com menor ordenada, então o 'Bloco' com menor ordenada (y) da "primeira coluna"
    é eliminado e chama-se novamente a função (sem o 'Bloco').
    -}
    | alturaMapa (pecasNaColuna x l) > alturaMapa (pecasNaColuna (x + 1) l) && coordenadaPertence (x,(alturaMapa l) - 1) (listaCoordenadas l) = validaChaoWrapper x (retiraPeca (x,(alturaMapa (pecasNaColuna x l))) l)
    | menorOrdenadaWrapper (pecasNaColuna x l) < menorOrdenadaWrapper (pecasNaColuna (x + 1) l) && coordenadaPertence (x,(alturaMapa l) + 1) (listaCoordenadas l) = validaChaoWrapper x (retiraPeca (x,(menorOrdenadaWrapper (pecasNaColuna x l))) l)
    | otherwise = False
