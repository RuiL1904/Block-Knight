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
    imagens :: Imagens,
    menuAtual :: Menu
    -- Adicionar mais coisas aqui no futuro.
}

data Imagens = Imagens {
    mpJogar :: Picture,
    mpCreditos :: Picture,
    mpSair :: Picture
}

data Menu
    = MenuPrincipal Opcoes
    | MenuJogar

data Opcoes
    = Jogar
    | Creditos
    | Sair

window :: Display
window = InWindow
    "Block Knight"
    (1280, 720)
    (0,0)

fr :: Int
fr = 50

reageEventoGloss :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuPrincipal Jogar)) = return $ e{menuAtual = MenuPrincipal Creditos}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuPrincipal Creditos)) = return $ e{menuAtual = MenuPrincipal Sair}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuPrincipal Creditos)) = return $ e{menuAtual = MenuPrincipal Jogar}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuPrincipal Sair)) = return $ e{menuAtual = MenuPrincipal Creditos}
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal Sair)) = exitSuccess
reageEventoGloss _ e = return e

reageTempoGloss :: Float -> EstadoGloss -> IO EstadoGloss
reageTempoGloss _ e = return e

carregaImagens :: IO Imagens
carregaImagens = do
    Just mpJogar <- loadJuicy "assets/mp_jogar.png"
    Just mpCreditos <- loadJuicy "assets/mp_creditos.png"
    Just mpSair <- loadJuicy "assets/mp_sair.png"
    return (Imagens mpJogar mpCreditos mpSair)

desenhaEstadoGloss :: EstadoGloss -> IO Picture
desenhaEstadoGloss e = do
    let x = (imagens e)
    case (menuAtual e) of
        MenuPrincipal Jogar -> return $ mpJogar x
        MenuPrincipal Creditos -> return $ mpCreditos x
        MenuPrincipal Sair -> return $ mpSair x

main :: IO ()
main = do
    imagens <- carregaImagens
    let estadoGlossInicial = (EstadoGloss imagens (MenuPrincipal Jogar))
    playIO window 
        (greyN 0.25) 
        fr
        estadoGlossInicial
        desenhaEstadoGloss
        reageEventoGloss
        reageTempoGloss
