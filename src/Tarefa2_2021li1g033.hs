{- |
Module      : Tarefa2_2021li1g033
Description : Construção/Desconstrução do mapa
Copyright   : Rui Lopes Martins <a100643@alunos.uminho.pt>;
            : Diogo Ribeiro Vassalo de Abreu <a100646@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g033 where

import LI12122
import Tarefa1_2021li1g033
import Utils

-- | Transforma uma lista de pares de 'Peca' e de 'Coordenadas' num 'Mapa'.
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = constroiMapaWrapper l (constroiMapaVazio l)

constroiMapaWrapper :: [(Peca, Coordenadas)] -> Mapa -> Mapa
constroiMapaWrapper [] m = m
constroiMapaWrapper (h:t) m = constroiMapaWrapper t (inserePeca h m)

-- | Constrói uma lista com um número de listas vazias igual à altura do mapa (y), cujo cada lista contém 'Vazio' (até à largura (x) máxima).
constroiMapaVazio :: [(Peca, Coordenadas)] -> Mapa
constroiMapaVazio l = [listaComVazios | _ <- [0..altura]] -- > Constrói a lista por compreensão.
    where
        largura = larguraMapa l
        altura = alturaMapa l
        listaComVazios = [Vazio | _ <- [0..largura]] -- > Constrói a lista por compreensão.

-- | Insere cada uma das peças no espaço correspondente.
inserePeca :: (Peca, Coordenadas) -> Mapa -> Mapa
inserePeca _ [] = []
-- | Insere em y.
inserePeca (f,(x,y)) (h:t)
    | y == 0 = (inserePeca' f x h) : t
    | otherwise = h : inserePeca (f,(x,y - 1)) t
    where -- | Insere em x.
        inserePeca' f x (h:t)
            | x == 0 = f : t
            | otherwise = h : inserePeca' f (x - 1) t

-- | Função inversa de 'constroiMapa'.
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa l = desconstroiPeca l 0

-- | Desconstrói as peças.
desconstroiPeca :: Mapa -> Int -> [(Peca, Coordenadas)]
desconstroiPeca [] _ = []
desconstroiPeca (h:t) y = (desconstroiLinha h 0 y) ++ (desconstroiPeca t (y + 1))

-- | Auxiliar de 'descontroiPeca' - Desconstrói as peças por linha.
desconstroiLinha :: [Peca] -> Int -> Int -> [(Peca, Coordenadas)]
desconstroiLinha [] _ _ = []
desconstroiLinha (Vazio:t) x y = desconstroiLinha t (x + 1) y
desconstroiLinha (h:t) x y = (h,(x,y)) : desconstroiLinha t (x + 1) y
