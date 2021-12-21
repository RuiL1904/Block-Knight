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
import Fixtures

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicy)
import System.Exit

data EstadoGloss = EstadoGloss {
    imagens :: Imagens, -- ^ O conjunto das 'Imagens' necessárias ao programa.
    menuAtual :: Menu, -- ^ O menu em que o utilizador está atualmente.
    jogo :: Jogo -- ^ O 'Jogo' atual.
    -- Adicionar mais coisas aqui no futuro.
}

data Imagens = Imagens {
    mpJogar :: Picture,
    mpCreditos :: Picture,
    mpSair :: Picture,
    creditos :: Picture,
    bloco :: Picture,
    caixa :: Picture,
    porta :: Picture,
    knightLeft :: Picture,
    knightRight :: Picture
    -- Adicionar as imagens necessárias aqui no futuro.
}

data Menu
    = MenuPrincipal OpcoesP
    | MenuJogar OpcoesJ

data OpcoesP
    = Jogar Bool -- O 'Bool' dita se o utilizador está ou não dentro do sub-menu.
    | Creditos Bool
    | Sair

data OpcoesJ
    = Mapa1 Bool -- O 'Bool' dita se o 'Mapa' em questão está ou não a ser jogado.
    | Mapa2 Bool
    | Mapa3 Bool
    | Mapa4 Bool

window :: Display
window = InWindow
    "Block Knight"
    (1280, 720)
    (0,0)

fr :: Int
fr = 50

-- | Função necessária para a função 'playIO' - reage a um evento, por parte do utilizador, através do teclado.
reageEventoGloss :: Event -> EstadoGloss -> IO EstadoGloss
-- MenuPrincipal
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Jogar False)) _) = return $ e{menuAtual = MenuPrincipal (Creditos False)}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos False)) _) = return $ e{menuAtual = MenuPrincipal Sair}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos False)) _) = return $ e{menuAtual = MenuPrincipal (Jogar False)}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuPrincipal Sair) _) = return $ e{menuAtual = MenuPrincipal (Creditos False)}
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal Sair) _) = exitSuccess
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos False)) _) = return $ e{menuAtual = MenuPrincipal (Creditos True)}
reageEventoGloss (EventKey _ Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos True)) _) = return $ e{menuAtual = MenuPrincipal (Jogar False)}
-- MenuJogar
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Jogar False)) _) = return $ e{menuAtual = MenuPrincipal (Jogar True)}
reageEventoGloss _ e = return e

-- | Função necessária para a função 'playIO' - reage à passagem do tempo.
reageTempoGloss :: Float -> EstadoGloss -> IO EstadoGloss
reageTempoGloss _ e = return e

carregaImagens :: IO Imagens
carregaImagens = do
    Just mpJogar <- loadJuicy "assets/mp_jogar.png"
    Just mpCreditos <- loadJuicy "assets/mp_creditos.png"
    Just mpSair <- loadJuicy "assets/mp_sair.png"
    Just creditos <- loadJuicy "assets/creditos.png"
    Just bloco <- loadJuicy "assets/bloco.png"
    Just caixa <- loadJuicy "assets/caixa.png"
    Just porta <- loadJuicy "assets/porta.png"
    Just knightLeft <- loadJuicy "assets/knight_left.png"
    Just knightRight <- loadJuicy "assets/knight_right.png"
    return (Imagens mpJogar mpCreditos mpSair creditos bloco caixa porta knightLeft knightRight)

-- | Função necessária para a função 'playIO' - desenha o estado do programa, consoante algumas variáveis.
desenhaEstadoGloss :: EstadoGloss -> IO Picture
desenhaEstadoGloss e = do
    let i = (imagens e)
    let (Jogo m _) = (jogo e)
    let m' = desconstroiMapa m
    case (menuAtual e) of
        MenuPrincipal (Jogar False) -> return $ mpJogar i
        MenuPrincipal (Creditos False) -> return $ mpCreditos i
        MenuPrincipal Sair -> return $ mpSair i
        MenuPrincipal (Creditos True) -> return $ creditos i
        MenuPrincipal (Jogar True) -> return $ deslocaMapa m' (Pictures [desenhaJogador e, desenhaMapa e m'])
    
desenhaJogador :: EstadoGloss -> Picture
desenhaJogador e = Translate x' y' $ Scale 0.35 0.35 $ knightRight i -- TODO: Verificar 'dir' e 'eval'; Knight está muito pequeno (alterar no GIMP).
    where (Jogo _ (Jogador (x,y) dir eval)) = (jogo e)
          i = (imagens e)
          x' = (fromIntegral x)
          y' = (fromIntegral y)

desenhaMapa :: EstadoGloss -> [(Peca, Coordenadas)] -> Picture
desenhaMapa _ [] = Blank
desenhaMapa e ((p,(x,y)):t) =
    case p of
        Bloco -> Pictures [Translate x' y' $ Scale 0.4 0.4 $ bloco i, desenhaMapa e t]   
        Caixa -> Pictures [Translate x' y' $ Scale 0.2 0.2 $ caixa i, desenhaMapa e t] 
        Porta -> Pictures [Translate x' y' $ Scale 0.4 0.4 $ porta i, desenhaMapa e t]
        _ -> Blank
    where i = (imagens e)
          x' = (fromIntegral x) * 50
          y' = (fromIntegral (-y)) * 50

deslocaMapa :: [(Peca, Coordenadas)] -> Picture -> Picture
deslocaMapa l i = Translate x y i
    where x = (-1.0) * (fromIntegral (div ((larguraMapa l) * 50) 2))
          y = (fromIntegral (div ((alturaMapa l) * 50) 2))

main :: IO ()
main = do
    imagens <- carregaImagens
    let estadoGlossInicial = (EstadoGloss imagens (MenuPrincipal (Jogar False)) (Jogo m9r (Jogador (12 * 50, (-8) * 50) Este True))) -- Mapa de exemplo, alterar depois
    playIO window 
        (greyN 0.25) 
        fr
        estadoGlossInicial
        desenhaEstadoGloss
        reageEventoGloss
        reageTempoGloss
  