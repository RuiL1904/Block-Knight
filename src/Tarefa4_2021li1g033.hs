{- |
Module      : Tarefa4_2021li1g033
Description : Movimentação do personagem
Copyright   : Rui Lopes Martins <a100643@alunos.uminho.pt>;
            : Diogo Ribeiro Vassalo de Abreu <a100646@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g033 where

import LI12122

import Tarefa3_2021li1g033

import Fixtures

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador jogo movimento = undefined

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo movimentos = undefined

-- Função Auxiliar de moveJogador que indica qual a peça numa determinada coordenada

encontraPeca :: Mapa -> Coordenadas -> Maybe Peca
encontraPeca [] _ = Nothing
encontraPeca (h:t) (x,0) = encontraNaColuna h x
encontraPeca (h:t) (x,y) = encontraPeca t (x,y - 1)

encontraNaColuna :: [Peca] -> Int -> Maybe Peca
encontraNaColuna [] _ = Nothing
encontraNaColuna (h:t) 0 = Just h
encontraNaColuna (h:t) x = encontraNaColuna t (x-1)

-- Função auxiliar que verifica se o movimento é possível

verificaMovimento :: Jogo -> Movimento -> Jogo
verificaMovimento (Jogo l (Jogador (x,y) d b)) m 
    | d == Oeste && m == AndarDireita = Jogo l (Jogador (x,y) Este b)
    | d == Oeste && m == AndarDireita && encontraPeca l (x, y + 1) == Just Vazio = error "Jogador não consegue estar nessa posição"
    | d == Oeste && m == AndarEsquerda && encontraPeca l (x, y + 1) == Just Vazio = error "Jogador não consegue estar nessa posição"
    | d == Oeste && m == AndarEsquerda && encontraPeca l (x - 1, y - 1) == Just Vazio = Jogo l (Jogador (x,y) d b) 
    | d == Oeste && m == AndarEsquerda && encontraPeca l (x - 1,y) == Just Porta = Jogo l (Jogador (x,y) d b) 
    | d == Oeste && m == AndarEsquerda && b == True && encontraPeca l (x, y - 1) == Just Bloco || encontraPeca l (x, y - 1) == Just Caixa = error "Jogador não consegue estar nessa posição com a caixa"
    | d == Oeste && m == AndarEsquerda && b == False && encontraPeca l (x - 1, y + 1) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  && encontraPeca l (x - 1,y) == Just Vazio = Jogo l (Jogador (x - 1,y) d b) 
    | d == Oeste && m == AndarEsquerda && b == True && encontraPeca l (x - 1, y - 2) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  = Jogo l (Jogador (x,y) d b) 
    | d == Oeste && m == Trepar && b == False && encontraPeca l (x - 1,y) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  && encontraPeca l (x - 1, y - 1) == Just Vazio = Jogo l (Jogador (x - 1, y - 1) d b)
    | d == Oeste && m == Trepar && b == False && encontraPeca l (x - 1, y - 1) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  = Jogo l (Jogador (x,y) d b)
    | d == Oeste && m == Trepar && b == True && encontraPeca l (x - 1, y - 2) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  = Jogo l (Jogador (x,y) d b)  
    | d == Oeste && m == Trepar && b == True && encontraPeca l (x - 1, y) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  && encontraPeca l (x - 1, y - 1) == Just Vazio && encontraPeca l (x - 1, y - 2) == Just Vazio = Jogo l (Jogador (x - 1, y - 1) d b)
    | d == Oeste && m == InterageCaixa && b == False && encontraPeca l (x, y - 1) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  = Jogo l (Jogador (x,y) d b) 
    | d == Este && m == AndarEsquerda = Jogo l (Jogador (x,y) Oeste b)
    | d == Este && m == AndarEsquerda && encontraPeca l (x, y + 1) == Just Vazio = error "Jogador não consegue estar nessa posição"
    | d == Este && m == AndarDireita && encontraPeca l (x, y + 1) == Just Vazio = error "Jogador não consegue estar nessa posição"
    | d == Este && m == AndarDireita && encontraPeca l (x + 1, y - 1) == Just Vazio = Jogo l (Jogador (x,y) d b)
    | d == Este && m == AndarDireita && encontraPeca l (x + 1,y) == Just Porta = Jogo l (Jogador (x,y) d b)
    | d == Este && m == AndarDireita && b == True && encontraPeca l (x, y - 1) == Just Bloco || encontraPeca l (x, y - 1) == Just Caixa = error "Jogador não consegue estar nessa posição com a caixa"
    | d == Este && m == AndarDireita && b == False && encontraPeca l (x + 1, y + 1) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  && encontraPeca l (x - 1,y) == Just Vazio = Jogo l (Jogador (x + 1,y) d b) 
    | d == Este && m == AndarDireita && b == True && encontraPeca l (x + 1, y - 2) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  = Jogo l (Jogador (x,y) d b) 
    | d == Este && m == Trepar && b == False && encontraPeca l (x + 1,y) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  && encontraPeca l (x + 1, y - 1) == Just Vazio = Jogo l (Jogador (x + 1, y - 1) d b)
    | d == Este && m == Trepar && b == False && encontraPeca l (x + 1, y - 1) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  = Jogo l (Jogador (x,y) d b)
    | d == Este && m == Trepar && b == True && encontraPeca l (x + 1, y - 2) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  = Jogo l (Jogador (x,y) d b)  
    | d == Este && m == Trepar && b == True && encontraPeca l (x + 1, y) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  && encontraPeca l (x + 1, y - 1) == Just Vazio && encontraPeca l (x + 1, y - 2) == Just Vazio = Jogo l (Jogador (x + 1, y - 1) d b)
    | d == Este && m == InterageCaixa && b == False && encontraPeca l (x, y - 1) == Just Bloco || encontraPeca l (x - 1, y + 1) == Just Caixa  = Jogo l (Jogador (x,y) d b)    

-- Função auxiliar para pegar em caixas

retiraCaixa :: Jogo -> Movimento -> Jogo 
retiraCaixa (Jogo l (Jogador (x,y) d b)) m = undefined

-- Função auxiliar que introduzindo o x da coluna define uma lista com os elementos dessa coluna  

colunaapenas :: [(Peca, Coordenadas)] -> Int -> [(Peca, Coordenadas)]
colunaapenas [] n = []
colunaapenas ((h, (x,y)):t) n 
    | x == n = (h, (x,y)) : colunaapenas t n
    | otherwise = colunaapenas t n 

-- Função auxiliar que calcula o maior y de uma lista de coordenadas    

maiorYCoordenadas :: [Coordenadas] -> Int 
maiorYCoordenadas [] = error "Não existe"
maiorYCoordenadas [(_,y)] = y
maiorYCoordenadas ((x,y):(xs,ys):t) 
    | y >= ys = maiorYCoordenadas ((x,y):t)
    | otherwise = maiorYCoordenadas ((xs,ys):t)

-- Função auxiliar que passa uma lista de Peças e respetivas coordenadas para apenas uma lista dessas coordenadas

soCoordenadas :: [(Peca,Coordenadas)] -> [Coordenadas]
soCoordenadas [] = []
soCoordenadas ((p,(x,y)):t) = ((x,y): soCoordenadas t)

-- Função auxiliar que calcula o bloco com maior y numa coluna

maiorYNaColuna :: [(Peca,Coordenadas)] -> Int 
maiorYNaColuna [] = error "Não existe"
maiorYNaColuna l 


    

   