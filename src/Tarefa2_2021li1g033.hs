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

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = constroiMapa' l (constroiMapaVazio l)
    where
        constroiMapa' [] m = m
        constroiMapa' (h:t) m = constroiMapa' t (inserePeca h m)

-- | Constrói uma lista com um número de listas vazias igual à altura do mapa (y), cujo cada lista contém Vazios.
constroiMapaVazio :: [(Peca, Coordenadas)] -> Mapa
constroiMapaVazio l = [listaComVazios | _ <- [0..altura]] -- ^ Constrói a lista por compreensão.
    where
        largura = (larguraMapa l) - 1
        altura = (alturaMapa l) - 1
        listaComVazios = [Vazio | _ <- [0..largura]] -- ^ Constrói a lista por compreensão.

-- | Insere cada uma das peças no espaço correspondente.
inserePeca :: (Peca, Coordenadas) -> Mapa -> Mapa
-- | Insere em y.
inserePeca (f,(x,y)) (h:t)
    | y == 0 = (inserePeca' f x h) : t
    | otherwise = h : inserePeca (f,(x,y - 1)) t
    where -- | Insere em x.
        inserePeca' f x (h:t)
            | x == 0 = f : t
            | otherwise = h : inserePeca' f (x - 1) t

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa l = desconstroiPeca l 0

-- | Desconstrói as peças.
desconstroiPeca :: Mapa -> Int -> [(Peca, Coordenadas)]
desconstroiPeca [] _ = []
desconstroiPeca (h:t) y = (desconstroiPLinha h 0 y) ++ (desconstroiPeca t (y + 1))

-- | Auxiliar de descontroiPeca - Desconstrói as peças por linha.
desconstroiPLinha :: [Peca] -> Int -> Int -> [(Peca, Coordenadas)]
desconstroiPLinha [] _ _ = []
desconstroiPLinha (Vazio:t) x y = desconstroiPLinha t (x + 1) y
desconstroiPLinha (h:t) x y = (h,(x,y)) : desconstroiPLinha t (x + 1) y
