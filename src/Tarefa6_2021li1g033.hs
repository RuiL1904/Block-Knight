{- |
Module      : Tarefa6_2021li1g033
Description : O objetivo desta tarefa é implementar uma função 'resolveJogo' que tenta (daí o Maybe no resultado) resolver um jogo num número máximo de movimentos.

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Main where

import LI12122
import Tarefa1_2021li1g033
import Tarefa2_2021li1g033
import Tarefa3_2021li1g033
import Tarefa4_2021li1g033

import Utils
import Niveis

-- | Uma Rose Tree.
data Tree a = Node a [Tree a] deriving Show

-- | Função principal.
main :: IO ()
main = print $ resolveJogo 10 j1

-- | Resolve um jogo - resolver um jogo consiste em encontrar a sequência de movimentos que o jogador pode realizar para chegar à porta.
resolveJogo :: 
    Int -- ^ Número máximo de movimentos dado.
    -> Jogo -- ^ 'Jogo' a ser resolvido.
    -> Maybe [Movimento] -- ^ A sequência de movimentos a executar para completar o jogo.
resolveJogo x jogo = resolveJogoTree x (Node jogo [])

-- | Função auxiliar de 'resolveJogo' - Implementa (ou não) a expansão de uma Rose Tree.
resolveJogoTree :: 
    Int  -- ^ Número máximo de movimentos dado.
    -> Tree Jogo -- ^ Uma Rose Tree de 'Jogo'.
    -> Maybe [Movimento] -- ^ -- ^ A sequência de movimentos a executar para completar o jogo.
resolveJogoTree x tree
    | x < 0 = Nothing -- Caso os movimentos não sejam suficientes.
    | eJust caminho = Just (converteListaJogos $ retiraDoJust caminho) -- Caso o caminho exista.
    | otherwise = resolveJogoTree (x - 1) (expandeTree tree) -- Caso com o número de movimentos atuais não seja possível encontrar uma sequência, diminui-se um número a esse 'Int' e expande-se a Rose Tree.
    where caminho = encontraCaminho tree

-- | Expande uma Rose Tree com os 4 movimentos possíveis ('Movimento') aplicados a um 'Jogo'.
expandeTree :: Tree Jogo -> Tree Jogo
expandeTree (Node a []) = Node a (adicionaJogos a x)
    where x = filter (\mov -> a /= moveJogador a mov) [Trepar, AndarEsquerda, AndarDireita, InterageCaixa]
expandeTree (Node a l) = Node a (map expandeTree l)

-- | Função auxilar de 'expandeTree' - Adiciona um 'Jogo' depois de lhe ter sido aplicado um 'Movimento'.
adicionaJogos :: Jogo -> [Movimento] -> [Tree Jogo]
adicionaJogos j l = map (\mov -> Node (moveJogador j mov) []) l

-- | Converte uma lista de 'Jogo' numa lista de 'Movimento'.
converteListaJogos :: [Jogo] -> [Movimento]
converteListaJogos [] = []
converteListaJogos [_] = []
converteListaJogos (x:y:t) = converteJogos x y : converteListaJogos (y:t)

-- | Função auxiliar de 'converteListaJogos' - converte dois 'Jogo' seguidos num 'Movimento' de acordo com certas condições.
converteJogos :: Jogo -> Jogo -> Movimento
converteJogos (Jogo m1 (Jogador (x,y) dir eval)) (Jogo m2 (Jogador (x',y') dir' eval'))
    | y' > y = Trepar -- Caso o y do primeiro 'Jogo' seja inferior ao y do segundo 'Jogo', então o 'Jogador' trepou.
    | eval /= eval' = InterageCaixa -- Caso o 'Bool' do 'Jogador' tenha trocado, então o 'Jogador' interagiu com a caixa.
    | dir' == Este = AndarDireita -- Caso a 'Direcao' do 'Jogador' no segundo 'Jogo' seja este, então o 'Jogador' andou para a direita.
    | otherwise = AndarEsquerda -- Caso a 'Direcao' do 'Jogador' no segundo jogo seja oeste, então o 'Jogador' andou para a esquerda.

-- | Encontra o caminho que vai dar à 'Porta'.
encontraCaminho :: Tree Jogo -> Maybe [Jogo]
encontraCaminho (Node j@(Jogo _ (Jogador c _ _)) [])
    | encontraPorta j = Just [j]
    | otherwise = Nothing

encontraCaminho (Node j (h:t))
    | eJust $ encontraCaminho h = Just (j : retiraDoJust (encontraCaminho h))
    | otherwise = encontraCaminho (Node j t)

-- | Retorna as 'Coordenadas' onde se encontra a 'Porta', este função é mais eficiente do que a definida na tarefa 5.
encontraPorta ::Jogo -> Bool
encontraPorta (Jogo m (Jogador (x,y) _ _)) = ((m !! y) !! x) == Porta
