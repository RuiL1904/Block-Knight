{- |
Module      : Tarefa6_2021li1g033
Description : Resolução de um puzzle

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Main where

import LI12122
import Tarefa1_2021li1g033
import Tarefa2_2021li1g033
import Tarefa3_2021li1g033
import Tarefa4_2021li1g033
import Utils

-- | Uma Rose Tree.
data Tree a = Node a [Tree a]

-- Mapas para teste.
j1 :: Jogo
j1 = Jogo m1 (Jogador (6, 0) Oeste False)

m1 :: Mapa
m1 =
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
      [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    ]
  
main :: IO ()
main = print $ resolveJogo 20 j1

resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo x jogo = resolveJogoTree x (Node jogo [])
    
resolveJogoTree :: Int -> Tree Jogo -> Maybe [Movimento]
resolveJogoTree x tree
    | x < 0 = Nothing 
    | eJust caminho = Just (converteListaJogos $ retiraDoJust caminho)
    | otherwise = resolveJogoTree (x - 1) (expandeTree tree)
    where caminho = encontraCaminho tree

expandeTree :: Tree Jogo -> Tree Jogo
expandeTree (Node a []) = Node a (adicionaJogos a [Trepar, AndarEsquerda, AndarDireita, InterageCaixa])
expandeTree (Node a l) = Node a (map expandeTree l)

adicionaJogos :: Jogo -> [Movimento] -> [Tree Jogo]
adicionaJogos j l = map (\mov -> Node (moveJogador j mov) []) l

encontraCaminho :: Tree Jogo -> Maybe [Jogo]
encontraCaminho (Node j@(Jogo _ (Jogador c _ _)) [])
    | encontraPorta j == c = Just [j]
    | otherwise = Nothing

encontraCaminho (Node j (h:t))
    | eJust $ encontraCaminho h = Just (j : retiraDoJust (encontraCaminho h))
    | otherwise = encontraCaminho (Node j t)

converteListaJogos :: [Jogo] -> [Movimento]
converteListaJogos [] = []
converteListaJogos [_] = []
converteListaJogos (x:y:t) = converteJogos x y : converteListaJogos (y:t)

converteJogos :: Jogo -> Jogo -> Movimento
converteJogos (Jogo m1 (Jogador (x,y) dir eval)) (Jogo m2 (Jogador (x',y') dir' eval'))
    | y > y' = Trepar
    | eval /= eval' = InterageCaixa
    | dir' == Este = AndarDireita
    | otherwise = AndarEsquerda

-- | Retorna as 'Coordenadas' onde se encontra a 'Porta'.
encontraPorta ::Jogo -> Coordenadas
encontraPorta (Jogo m j) =
    case p of
        Porta -> c
        _ -> encontraPorta (Jogo (constroiMapa t) j)
    where ((p,c@(x,y)):t) = desconstroiMapa m
