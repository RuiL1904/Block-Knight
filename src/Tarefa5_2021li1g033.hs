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
import Data
import Audio

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicy)
import Graphics.Gloss.Interface.Environment
import System.Exit

-- | O estado do programa.
data EstadoGloss = EstadoGloss {
    imagens :: Imagens, -- ^ O conjunto das 'Imagens' necessárias ao programa.
    menuAtual :: Menu, -- ^ O 'Menu' em que o utilizador está atualmente.
    jogo :: Jogo -- ^ O 'Jogo' atual.
    -- Adicionar mais coisas aqui no futuro.
}

-- | O conjunto das imagens inerentes ao programa.
data Imagens = Imagens {
    background :: Picture,
    mpJogar :: Picture,
    mpOpcoes :: Picture,
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

-- | Os menus disponíveis.
data Menu
    = MenuPrincipal OpcoesP
    | MenuJogar OpcoesJ

-- | As opções do 'MenuPrincipal' disponíveis.
data OpcoesP
    = Jogar
    | Opcoes Bool -- O 'Bool' dita se o jogador está ou não dentro do 'Menu' em questão.
    | Creditos Bool -- O 'Bool' dita se o jogador está ou não dentro do 'Menu' em questão.
    | Sair

-- | As opções do 'MenuJogar' disponíveis.
data OpcoesJ
    = EscolheMapa Mapas
    | ModoArcade Mapas

-- | O conjunto dos mapas disponíveis para jogar.
data Mapas = Mapa1 | Mapa2 | Mapa3 | Mapa4 | Mapa5 | Mapa6 | Mapa7 | Mapa8

-- | Define a dimensão da janela (neste caso FullScreen).
window :: Display
window = FullScreen

-- | Uma constante que define a taxa de atualização do programa - quantas vezes a função 'reageTempoGloss' é chamada por segundo.
fr :: Int
fr = 50

-- | Função que reage a um evento, por parte do utilizador, através do teclado ou do rato.
reageEventoGloss :: 
    Event -> -- ^ Um evento, por parte do utilizador, através do teclado ou do rato, por exemplo: clicar na tecla Enter.
    EstadoGloss -> 
    IO EstadoGloss
-- MenuPrincipal.
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuPrincipal Jogar) _) = return $ e{menuAtual = MenuPrincipal (Opcoes False)}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes False)) _) = return $ e{menuAtual = MenuPrincipal (Creditos False)}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos False)) _) = return $ e{menuAtual = MenuPrincipal Sair}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuPrincipal Sair) _) = return $ e{menuAtual = MenuPrincipal (Creditos False)}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos False)) _) = return $ e{menuAtual = MenuPrincipal (Opcoes False)}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes False)) _) = return $ e{menuAtual = MenuPrincipal Jogar}  
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal Jogar) _) = 
    do killProcess
       return $ e{menuAtual = MenuJogar (EscolheMapa Mapa1)}
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes False)) _) = return $ e{menuAtual = MenuPrincipal (Opcoes True)}
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos False)) _) = return $ e{menuAtual = MenuPrincipal (Creditos True)}
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal Sair) _) = 
    do killProcess
       exitSuccess
reageEventoGloss (EventKey _ Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos True)) _) = return $ e{menuAtual = MenuPrincipal Jogar} 
-- MenuJogar.
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar _) _) = return $ e{jogo = moveDireita (jogo e)}
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar _) _) = return $ e{jogo = moveEsquerda (jogo e)}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar _) _) = return $ e{jogo = podeTrepar (jogo e)}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar _) _) = return $ e{jogo = interageCaixa (jogo e)}
reageEventoGloss (EventKey (Char 'd') Down _ _) e@(EstadoGloss _ (MenuJogar _) (Jogo m (Jogador c Oeste eval))) = return $ e{jogo = (Jogo m (Jogador c Este eval))}
reageEventoGloss (EventKey (Char 'e') Down _ _) e@(EstadoGloss _ (MenuJogar _) (Jogo m (Jogador c Este eval))) = return $ e{jogo = (Jogo m (Jogador c Oeste eval))}
reageEventoGloss (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar _) _) = return $ e{jogo = j2}
-- Fechar o programa a qualquer altura.
reageEventoGloss (EventKey (Char 'c') Down _ _) _ =
    do killProcess
       exitSuccess
reageEventoGloss _ e = return e

-- | Função que reage à passagem do tempo.
reageTempoGloss :: 
    Float -> -- ^ Um 'Float' que contabiliza o tempo que passou desde a última chamada da função 'reageTempoGloss'.
    EstadoGloss ->
    IO EstadoGloss
reageTempoGloss _ e = return e

-- Carrega todas as imagens para o data type 'Imagens'.
carregaImagens :: IO Imagens
carregaImagens = do
    Just background <- loadJuicy "assets/background.jpg"
    -- Menu Principal.
    Just mpJogar <- loadJuicy "assets/mpJogar.jpg"
    Just mpOpcoes <- loadJuicy "assets/mpOpcoes.jpg"
    Just mpCreditos <- loadJuicy "assets/mpCreditos.jpg"
    Just mpSair <- loadJuicy "assets/mpSair.jpg"
    -- Opções do Menu Principal.
    Just creditos <- loadJuicy "assets/creditos.jpg"
    -- Outros.
    Just bloco <- loadJuicy "assets/bloco.png"
    Just caixa <- loadJuicy "assets/caixa.png"
    Just porta <- loadJuicy "assets/porta.png"
    Just knightLeft <- loadJuicy "assets/knightLeft.png"
    Just knightRight <- loadJuicy "assets/knightRight.png"
    return (Imagens background mpJogar mpOpcoes mpCreditos mpSair creditos bloco caixa porta knightLeft knightRight)

-- | Desenha o estado do programa, consoante algumas variáveis - transforma um estado numa 'Picture'.
desenhaEstadoGloss :: EstadoGloss -> IO Picture
desenhaEstadoGloss e = do
    (x,y) <- getScreenSize
    let x' = (fromIntegral x) / 1920 -- Largura da imagem.
    let y' = (fromIntegral y) / 1080 -- Altura da imagem.
    let i = (imagens e)
    let (Jogo m _) = (jogo e)
    let m' = desconstroiMapa m
    case (menuAtual e) of
        MenuPrincipal Jogar -> return $ Scale x' y' $ mpJogar i
        MenuPrincipal (Opcoes False) -> return $ Scale x' y' $ mpOpcoes i
        MenuPrincipal (Creditos False) -> return $ Scale x' y' $ mpCreditos i
        MenuPrincipal Sair -> return $ Scale x' y' $ mpSair i
        MenuPrincipal (Creditos True) -> return $ Scale x' y' $ creditos i
        MenuJogar _ -> return $ Pictures [background i, deslocaMapa m' $ Pictures [desenhaMapa e m', desenhaJogador e]]

-- Função auxiliar de 'desenhaEstadoGloss' - desenha o 'Jogador'.
desenhaJogador :: EstadoGloss -> Picture
desenhaJogador e =
    case eval of
        True
            | dir == Este -> Pictures [Translate x' y' $ Scale 0.4 0.4 $ knightRight i, Translate x' (y' + 70) $ Scale 0.17 0.17 $ caixa i]
            | otherwise -> Pictures [Translate x' y' $ Scale 0.4 0.4 $ knightLeft i, Translate x' (y' + 70) $ Scale 0.17 0.17 $ caixa i]
        _
            | dir == Este -> Translate x' y' $ Scale 0.4 0.4 $ knightRight i
            | otherwise -> Translate x' y' $ Scale 0.4 0.4 $ knightLeft i
    where (Jogo _ (Jogador (x,y) dir eval)) = (jogo e)
          i = (imagens e)
          x' = (fromIntegral x) * 50
          y' = (fromIntegral (-y)) * 50

-- Função auxiliar de 'desenhaEstadoGloss' - desenha o 'Mapa'.
desenhaMapa :: EstadoGloss -> [(Peca, Coordenadas)] -> Picture
desenhaMapa _ [] = Blank
desenhaMapa e ((p,(x,y)):t) =
    case p of
        Bloco -> Pictures [Translate x' y' $ Scale 0.17 0.17 $ bloco i, desenhaMapa e t]   
        Caixa -> Pictures [Translate x' y' $ Scale 0.17 0.17 $ caixa i, desenhaMapa e t] 
        Porta -> Pictures [Translate x' y' $ Scale 0.4 0.4 $ porta i, desenhaMapa e t] -- ENCONTRAR UMA PORTA MELHOR (IMPORTANTE)
        _ -> Blank
    where i = (imagens e)
          x' = (fromIntegral x) * 50
          y' = (fromIntegral (-y)) * 50

-- Função auxiliar de 'desenhaEstadoGloss' - centra o 'Mapa' nas coordenadas (0,0).
deslocaMapa :: [(Peca, Coordenadas)] -> Picture -> Picture
deslocaMapa l i = Translate x y i
    where x =  (-1.0) * (fromIntegral (div l' 2))
          y = (fromIntegral (div a 2))
          l' = (larguraMapa l) * 50
          a = (alturaMapa l) * 50

main :: IO ()
main = do
    playMenuPrincipal
    imagens <- carregaImagens
    let jogoInicial = j1
        estadoGlossInicial = (EstadoGloss imagens (MenuPrincipal Jogar) jogoInicial)
    playIO window 
        (greyN 0.32) 
        fr
        estadoGlossInicial
        desenhaEstadoGloss
        reageEventoGloss
        reageTempoGloss
