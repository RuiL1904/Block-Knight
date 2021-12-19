{- |
Module      : Tarefa5_2021li1g033
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import LI12122
import Tarefa1_2021li1g033
import Tarefa2_2021li1g033
import Tarefa3_2021li1g033
import Tarefa4_2021li1g033
import Utils

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicy)
import System.Exit

data EstadoGloss = EstadoGloss {
    menuAtual :: Menu
    -- Adicionar mais coisas aqui no futuro.
}

data Menu
    = MenuPrincipal Opcoes
    | MenuJogar

data Opcoes
    = Jogar
    | Creditos
    | Sair

estadoGlossInicial :: Picture -> EstadoGloss
estadoGlossInicial p = (EstadoGloss (MenuPrincipal Jogar))

window :: Display
window = InWindow
    "Block Knight"
    (1280, 720)
    (0,0)

fr :: Int
fr = 60

reageEventoGloss :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss (MenuPrincipal Jogar)) = return $ e{menuAtual = MenuPrincipal Creditos}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss (MenuPrincipal Creditos)) = return $ e{menuAtual = MenuPrincipal Sair}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss (MenuPrincipal Creditos)) = return $ e{menuAtual = MenuPrincipal Jogar}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss (MenuPrincipal Sair)) = return $ e{menuAtual = MenuPrincipal Creditos}
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss (MenuPrincipal Sair)) = exitSuccess
reageEventoGloss _ e = return e

reageTempoGloss :: Float -> EstadoGloss -> IO EstadoGloss
reageTempoGloss _ e = return e

desenhaEstadoGloss :: EstadoGloss -> IO Picture
desenhaEstadoGloss e = do
    Just mpJogar <- loadJuicy "assets/mp_jogar.png"
    Just mpCreditos <- loadJuicy "assets/mp_creditos.png"
    Just mpSair <- loadJuicy "assets/mp_sair.png"
    case (menuAtual e) of
        MenuPrincipal Jogar -> return mpJogar
        MenuPrincipal Creditos -> return mpCreditos
        MenuPrincipal Sair -> return mpSair

main :: IO ()
main = do
    Just startImage <- loadJuicy "assets/mp_jogar.png"
    playIO window 
        (greyN 0.25) 
        fr
        (estadoGlossInicial startImage)
        desenhaEstadoGloss
        reageEventoGloss
        reageTempoGloss
    