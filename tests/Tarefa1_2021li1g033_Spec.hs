module Tarefa1_2021li1g033_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g033
import Fixtures

testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa sem portas" ~: validaPotencialMapa [(Bloco, (0,1)),(Bloco,(1,1))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com caixas a flutuar" ~: validaPotencialMapa [(Caixa,(0,0)),(Caixa,(1,0)),(Porta,(1,1)),(Bloco,(0,2)),(Bloco,(1,2))] ~=? False
    , "Tarefa 1 - Teste Valida Chao m2" ~: validaChao m2 ~=? True 
    , "Tarefa 1 - Teste Valida Chao m3" ~: validaChao m3 ~=? True
    , "Tarefa 1 - Teste Valida Chao m4" ~: validaChao m4 ~=? False
    , "Tarefa 1 - Teste Valida Chao m5" ~: validaChao m5 ~=? True
    , "Tarefa 1 - Teste Valida Chao m6" ~: validaChao m6 ~=? False 
    , "Tarefa 1 - Teste Valida Chao m7" ~: validaChao m7 ~=? False
    , "Tarefa 1 - Testa Valida Mapa m8" ~: validaPotencialMapa m8 ~=? True
    , "Tarefa 1 - Testa Valida Mapa m9" ~: validaPotencialMapa m9 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    ]