{- |
Module      : Tarefa3_2021li1g033
Description : Representação textual do jogo
Copyright   : Rui Lopes Martins <a100643@alunos.uminho.pt>;
            : Diogo Ribeiro Vassalo de Abreu <a100646@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g033 where

import LI12122

instance Show Jogo where
  show j = printJogo j

printJogo :: Jogo -> String
printJogo (Jogo m j) = mapaParaString m

-- | Transforma um mapa numa string.
mapaParaString :: Mapa -> String
mapaParaString [] = ""
mapaParaString (h:t) = linhaParaString h ++ "\n" ++ mapaParaString t

-- | Auxiliar de mapaParaString - Transforma uma linha numa string.
linhaParaString :: [Peca] -> String
linhaParaString [] = ""
linhaParaString (h:t) = pecaParaChar h : linhaParaString t

-- | Auxiliar de linhaParaString - Transforma cada uma das peças no caractér correspondente.
pecaParaChar :: Peca -> Char
pecaParaChar p =
  case p of 
    Vazio -> ' '
    Bloco -> 'X'
    Caixa -> 'C'
    Porta -> 'P'
