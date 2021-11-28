{- |
Module      : Tarefa2_2021li1g033
Description : Construção/Desconstrução do mapa
Copyright   : Rui Lopes Martins <a100643@alunos.uminho.pt>;
            : Diogo Ribeiro Vassalo de Abreu <a100646@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g033 (
    -- ** Construção do 'Mapa'.
    constroiMapa, constroiMapaVazio, inserePeca,
    -- ** Desconstrução do 'Mapa'.
    desconstroiMapa, desconstroiPeca, desconstroiLinha
    ) where

import LI12122
import Tarefa1_2021li1g033
import Utils

{- | Transforma uma lista de pares de 'Peca' e de 'Coordenadas' num 'Mapa'.

=== Exemplos de utilização:
>>> constroiMapa [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
[(Porta, (0, 3)),(Bloco, (0, 4)),(Bloco, (1, 4)),(Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4, 3)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))]
-}
constroiMapa :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Mapa -- ^ Um 'Mapa'.
constroiMapa [] = []
constroiMapa l = constroiMapaWrapper l (constroiMapaVazio l)

constroiMapaWrapper :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Mapa -- ^ Um 'Mapa'.
    -> Mapa -- ^ Um 'Mapa'.
constroiMapaWrapper [] m = m
constroiMapaWrapper (h:t) m = constroiMapaWrapper t (inserePeca h m)

-- | Constrói uma lista com um número de listas vazias igual à altura do mapa (y), cujo cada lista contém 'Vazio' (até à largura (x) máxima).
constroiMapaVazio :: 
    [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
    -> Mapa -- ^ Um 'Mapa'.
constroiMapaVazio l = [listaComVazios | _ <- [0..altura]] -- > Constrói a lista por compreensão.
    where
        largura = larguraMapa l
        altura = alturaMapa l
        listaComVazios = [Vazio | _ <- [0..largura]] -- > Constrói a lista por compreensão.

-- | Insere cada uma das peças no espaço correspondente.
inserePeca :: 
    (Peca, Coordenadas) -- ^ Um par de 'Peca' e de 'Coordenadas'.
    -> Mapa -- ^ Um 'Mapa'.
    -> Mapa -- ^ Um 'Mapa'.
inserePeca _ [] = []
-- Insere em y.
inserePeca (f,(x,y)) (h:t)
    | y == 0 = (inserePeca' f x h) : t
    | otherwise = h : inserePeca (f,(x,y - 1)) t
    where -- Insere em x.
        inserePeca' f x (h:t)
            | x == 0 = f : t
            | otherwise = h : inserePeca' f (x - 1) t

-- | Função inversa de 'constroiMapa'.
desconstroiMapa :: 
    Mapa -- ^ Um 'Mapa'.
    -> [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
desconstroiMapa l = desconstroiPeca l 0

-- | Desconstrói as peças.
desconstroiPeca :: 
    Mapa -- ^ Um 'Mapa'.
    -> Int -- ^ Um 'Int' - neste caso serve como acumulador.
    -> [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
desconstroiPeca [] _ = []
desconstroiPeca (h:t) y = (desconstroiLinha h 0 y) ++ (desconstroiPeca t (y + 1))

-- | Auxiliar de 'descontroiPeca' - Desconstrói as peças por linha.
desconstroiLinha :: 
    [Peca] -- ^ Uma lista de 'Peca' - lista a desconstruir.
    -> Int -- ^ Um 'Int' - abcissa da 'Peca'.
    -> Int -- ^ Um 'Int' - ordenada da 'Peca'.
    -> [(Peca, Coordenadas)] -- ^ Uma lista de pares de 'Peca' e de 'Coordenadas'.
desconstroiLinha [] _ _ = []
desconstroiLinha (Vazio:t) x y = desconstroiLinha t (x + 1) y
desconstroiLinha (h:t) x y = (h,(x,y)) : desconstroiLinha t (x + 1) y
