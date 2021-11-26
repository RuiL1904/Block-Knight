{- |
Module      : Tarefa3_2021li1g033
Description : Representação textual do jogo
Copyright   : Rui Lopes Martins <a100643@alunos.uminho.pt>;
            : Diogo Ribeiro Vassalo de Abreu <a100646@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g033 (
  -- ** Desenhar o 'Mapa'.
  mapaParaString, linhaParaString, pecaParaChar,
  -- ** Posicionar o 'Jogador'.
  posicionaJogador, jogadorParaChar, verificaCaixa, posicionaCaixa
  ) where

import LI12122

instance Show Jogo where
  show j = printJogo j -- Cria uma nova instância da Class Show para imprimir "coisas" do tipo 'Jogo'.

-- Transforma um jogo numa 'String'.
printJogo :: 
  Jogo -- ^ Um 'Jogo'.
  -> String -- ^ Resultado.
printJogo (Jogo m j) = verificaCaixaWrapper m j

{- | Transforma um mapa numa 'String'.

=== Exemplo de utilização:
>>> mapaParaString [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
"      <\n      X\n      X\nP   C X\nXXXXXXX"
-}
mapaParaString :: 
  Mapa -- ^ Um 'Mapa'.
  -> String -- ^ Resultado.
mapaParaString [] = ""
mapaParaString (h:t)
  | null t = linhaParaString h ++ mapaParaString t -- Verifica se o 't' existe, caso não exista não adiciona "\n".
  | otherwise = linhaParaString h ++ "\n" ++ mapaParaString t

-- | Auxiliar de 'mapaParaString' - Transforma uma linha (lista de 'Peca') numa 'String'.
linhaParaString :: 
  [Peca] -- ^ Uma linha (lista de 'Peca').
  -> String -- ^ Resultado.
linhaParaString [] = ""
linhaParaString (h:t) = pecaParaChar h : linhaParaString t

{- | Auxiliar de 'linhaParaString' - Transforma cada 'Peca' no 'Char' correspondente.

=== Pedaço de código (para uma melhor compreensão):
@
pecaParaChar p = -- p é uma 'Peca' neste caso.
  case p of 
    Vazio -> ' '
    Bloco -> ''X''
    Caixa -> ''C''
    Porta -> ''P''
@
-}
pecaParaChar :: 
  Peca -- ^ Uma 'Peca'.
  -> Char -- ^ Resultado.
pecaParaChar p =
  case p of 
    Vazio -> ' '
    Bloco -> 'X'
    Caixa -> 'C'
    Porta -> 'P'

posicionaJogadorWrapper m j  = posicionaJogador 0 0 (mapaParaString m) j

{- | Posiciona o 'Jogador' no 'Mapa'.

== Exemplos de utilização:
>>> posicionaJogadorWrapper [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (Jogador (5,3) Este True)
"       \n      X\n      X\nP   C>X\nXXXXXXX"

__Observação:__ 

@ 
posicionaJogadorWrapper m j = 'posicionaJogador' 0 0 ('mapaParaString' m) j
@
-}
posicionaJogador :: 
  Int -- ^ A abcissa inicial (por defeito 0).
  -> Int -- ^ A ordenada inicial (por defeito 0).
  -> String -- ^ Um 'Mapa' transformado numa 'String'.
  -> Jogador -- ^ O 'Jogador'.
  -> String -- ^ Resultado (O 'Mapa' com o 'Jogador' já posicionado.)
posicionaJogador x y m@(h:t) j@(Jogador (x',y') _ _)
  | h == '\n' = h : posicionaJogador 0 (y + 1) t j
  | x == x' && y == y' = jogadorParaChar j : t
  | otherwise = h : posicionaJogador (x + 1) y t j

{- | Auxiliar de posicionaJogador - Transforma o 'Jogador' num 'Char'.

=== Pedaço de código (para uma melhor compreensão):

@
jogadorParaChar (Jogador _ dir _)
  | dir == Este = ''>''
  | otherwise = ''<'' -- dir == Oeste
@
-}
jogadorParaChar :: 
  Jogador -- ^ O 'Jogador'.
  -> Char -- ^ Resultado.
jogadorParaChar (Jogador _ dir _)
  | dir == Este = '>'
  | otherwise = '<' -- dir == Oeste

verificaCaixaWrapper m j = verificaCaixa (posicionaJogadorWrapper m j) j

-- | Verifica se o 'Jogador' está a carregar uma 'Caixa', se sim apresenta-a no 'Mapa'.
verificaCaixa :: 
  String -- ^ O 'Mapa' transformado numa 'String'.
  -> Jogador -- ^ O 'Jogador'.
  -> String -- ^ Resultado (O 'Mapa' com a caixa posicionada ou não).
verificaCaixa m j@(Jogador (x,y) _ eval)
  | eval = posicionaCaixa 0 0 m (x, y - 1)
  | otherwise = m

-- | Posiciona a 'Caixa' em cima do 'Jogador'.
posicionaCaixa :: 
  Int -- ^ A abcissa inicial (por defeito 0).
  -> Int -- ^ A ordenada inicial (por defeito 0).
  -> String -- ^ O 'Mapa' transformado numa 'String'.
  -> Coordenadas -- ^ As 'Coordenadas' (com y - 1, pois a caixa está em cima dele) do 'Jogador'.
  -> String -- ^ Resultado.
posicionaCaixa x' y' m@(h:t) (x,y)
  | h == '\n' = h : posicionaCaixa 0 (y' + 1) t (x,y)
  | x' == x && y' == y = pecaParaChar (Caixa) : t
  | otherwise = h : posicionaCaixa (x' + 1) y' t (x,y)
