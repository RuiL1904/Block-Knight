{- |
Module      : Tarefa4_2021li1g033
Description : Movimentação do personagem
Copyright   : Rui Lopes Martins <a100643@alunos.uminho.pt>;
            : Diogo Ribeiro Vassalo de Abreu <a100646@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g033 where

import LI12122
import Utils
import Tarefa3_2021li1g033

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador j mov =
    case mov of
        AndarEsquerda -> moveEsquerda j
        AndarDireita -> moveDireita j
        Trepar -> podeTrepar j
        InterageCaixa -> interageCaixa j

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j [] = j
correrMovimentos j (h:t) = correrMovimentos (moveJogador j h) t

moveEsquerda :: Jogo -> Jogo
moveEsquerda j =
    case j of
        Jogo m (Jogador (x,y) _ True)
            | (pecaEsperada Vazio (x - 1,y) m || pecaEsperada Porta (x - 1,y) m) && (pecaEsperada Vazio (x - 1,y - 1) m || pecaImpossivel (x - 1,y - 1) m) -> Jogo m (Jogador (encontraBaixo (x - 1,y) m) Oeste True)
            | otherwise -> Jogo m (Jogador (x,y) Oeste True)
        Jogo m (Jogador (x,y) _ False)
            | pecaEsperada Vazio (x - 1,y) m || pecaEsperada Porta (x - 1,y) m -> Jogo m (Jogador (encontraBaixo (x - 1,y) m) Oeste False)
            | otherwise -> Jogo m (Jogador (x,y) Oeste False)

moveDireita :: Jogo -> Jogo
moveDireita j =
    case j of
        Jogo m (Jogador (x,y) _ True)
            | (pecaEsperada Vazio (x + 1,y) m || pecaEsperada Porta (x - 1,y) m) && (pecaEsperada Vazio (x + 1,y - 1) m || pecaImpossivel (x + 1,y - 1) m) -> Jogo m (Jogador (encontraBaixo (x + 1,y) m) Este True)
            | otherwise -> Jogo m (Jogador (x,y) Este True)
        Jogo m (Jogador (x,y) _ False)
            | pecaEsperada Vazio (x + 1,y) m || pecaEsperada Porta (x - 1,y) m -> Jogo m (Jogador (encontraBaixo (x + 1,y) m) Este False)
            | otherwise -> Jogo m (Jogador (x,y) Este False)

podeTrepar :: Jogo -> Jogo
podeTrepar j =
    case j of
        Jogo m (Jogador (x,y) Oeste True)
            | pecaEsperada Bloco (x - 1,y) m || pecaEsperada Caixa (x - 1,y) m -> 
                case (pecaEsperada Vazio (x - 1,y - 1) m || pecaEsperada Porta (x - 1,y - 1) m) && (pecaEsperada Vazio (x - 1,y - 2) m || pecaImpossivel (x - 1,y - 2) m) of
                    True -> Jogo m (Jogador (x - 1,y - 1) Este True)
                    _ -> j
            | otherwise -> j
        Jogo m (Jogador (x,y) Oeste False)
            | pecaEsperada Bloco (x - 1,y) m || pecaEsperada Caixa (x - 1,y) m ->
                case pecaEsperada Vazio (x - 1,y - 1) m || pecaEsperada Porta (x - 1,y - 1) m of
                    True -> Jogo m (Jogador (x - 1,y - 1) Este False)
                    _ -> j            
            | otherwise -> j
        Jogo m (Jogador (x,y) Este True)
            | pecaEsperada Bloco (x + 1,y) m || pecaEsperada Caixa (x + 1,y) m ->
                case (pecaEsperada Vazio (x + 1,y - 1) m || pecaEsperada Porta (x + 1,y - 1) m) && (pecaEsperada Vazio (x + 1,y - 2) m || pecaImpossivel (x + 1,y - 2) m) of
                    True -> Jogo m (Jogador (x + 1,y - 1) Oeste True)
                    _ -> j
            | otherwise -> j
        Jogo m (Jogador (x,y) Este False)
            | pecaEsperada Bloco (x + 1,y) m || pecaEsperada Caixa (x + 1,y) m ->
                case pecaEsperada Vazio (x + 1,y - 1) m || pecaEsperada Porta (x + 1,y - 1) m of
                    True -> Jogo m (Jogador (x + 1,y - 1) Oeste False)
                    _ -> j
            | otherwise -> j

interageCaixa :: Jogo -> Jogo
interageCaixa j =
    case j of
        Jogo m (Jogador (x,y) Oeste True)
            | pecaEsperada Vazio (x - 1,y - 1) m -> Jogo (substituiPeca (encontraBaixo (x - 1,y) m) Caixa m) (Jogador (x,y) Oeste False)
            | otherwise -> j
        Jogo m (Jogador (x,y) Oeste False)
            | pecaEsperada Caixa (x - 1,y) m && pecaEsperada Vazio (x - 1,y - 1) m && pecaEsperada Vazio (x,y - 1) m -> Jogo (substituiPeca (x - 1,y) Vazio m) (Jogador (x,y) Oeste True)
            | otherwise -> j
        Jogo m (Jogador (x,y) Este True)
            | pecaEsperada Vazio (x + 1,y - 1) m -> Jogo (substituiPeca (encontraBaixo (x + 1,y) m) Caixa m) (Jogador (x,y) Este False)
            | otherwise -> j
        Jogo m (Jogador (x,y) Este False)
            | pecaEsperada Caixa (x + 1,y) m && pecaEsperada Vazio (x + 1,y - 1) m && pecaEsperada Vazio (x,y - 1) m -> Jogo (substituiPeca (x + 1,y) Vazio m) (Jogador (x,y) Este True)
            | otherwise -> j
