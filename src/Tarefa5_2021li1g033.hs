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
import Graphics.Gloss.Juicy (loadJuicyPNG, loadJuicyJPG)
import Graphics.Gloss.Interface.Environment
import System.Exit
import Data.Char

-- | O estado do programa.
data EstadoGloss = EstadoGloss {
    imagens :: Imagens, -- ^ O conjunto das 'Imagens' necessárias ao programa.
    menuAtual :: Menu, -- ^ O 'Menu' em que o utilizador está atualmente.
    jogo :: Jogo -- ^ O 'Jogo' atual.
    -- Adicionar mais coisas, inerentes ao estado, aqui no futuro.
}

-- | O conjunto das imagens inerentes ao programa.
data Imagens = Imagens {
    background :: Picture,
    mpJogar :: Picture,
    mpOpcoes :: Picture,
    mpCreditos :: Picture,
    mpSair :: Picture,
    creditos :: Picture,
    mjMapa1 :: Picture,
    mjMapa2 :: Picture,
    mjMapa3 :: Picture,
    mjMapa4 :: Picture,
    mjMapa5 :: Picture,
    mjMapa6 :: Picture,
    mjMapa7 :: Picture,
    mjMapa8 :: Picture,
    mjMapa9 :: Picture,
    mjMapa10 :: Picture,
    setaBaixo :: Picture,
    setaCima :: Picture,  
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
    = EscolheMapa Bool Mapas -- O 'Bool' dita se o jogador está ou não a jogar um 'Mapa'.
    | ModoArcade
    | CarregarJogo

data Mapas = Mapa1 | Mapa2 | Mapa3 | Mapa4 | Mapa5 | Mapa6 | Mapa7 | Mapa8 | Mapa9 | Mapa10

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
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuPrincipal Jogar) _) = 
    do playMenuChange
       return $ e{menuAtual = MenuPrincipal (Opcoes False)}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes False)) _) = 
    do playMenuChange
       return $ e{menuAtual = MenuPrincipal (Creditos False)}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos False)) _) = 
    do playMenuChange
       return $ e{menuAtual = MenuPrincipal Sair}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuPrincipal Sair) _) = 
    do playMenuChange
       return $ e{menuAtual = MenuPrincipal (Creditos False)}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos False)) _) = 
    do playMenuChange
       return $ e{menuAtual = MenuPrincipal (Opcoes False)}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes False)) _) = 
    do playMenuChange
       return $ e{menuAtual = MenuPrincipal Jogar}  
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal Jogar) _) = 
    do killProcess 
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa1)}
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes False)) _) = return $ e{menuAtual = MenuPrincipal (Opcoes True)}
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos False)) _) = return $ e{menuAtual = MenuPrincipal (Creditos True)}
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal Sair) _) = 
    do killProcess
       exitSuccess
reageEventoGloss (EventKey _ Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos True)) _) = return $ e{menuAtual = MenuPrincipal (Creditos False)} 
-- MenuJogar.
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False m)) _) = return $ e{menuAtual = MenuJogar (EscolheMapa True m)}
reageEventoGloss (EventKey (SpecialKey KeyEsc) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False m)) _) = 
    do playMenuPrincipal
       return $ e{menuAtual = MenuPrincipal Jogar}
-- Escolher o mapa.
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa1)) j1) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa2), jogo = j2}
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa2)) j2) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa1), jogo = j1}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa1)) j1) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa3), jogo = j3}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa2)) j2) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa4), jogo = j4}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa3)) j3) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa1), jogo = j1}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa4)) j4) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa2), jogo = j2}
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa3)) j3) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa4), jogo = j4}
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa4)) j4) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa3), jogo = j3}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa3)) j3) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa5), jogo = j5}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa5)) j5) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa3), jogo = j3}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa4)) j4) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa6), jogo = j6}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa6)) j6) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa4), jogo = j4}
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa5)) j5) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa6), jogo = j6}
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa6)) j6) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa5), jogo = j5}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa5)) j5) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa7), jogo = j7}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa7)) j7) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa5), jogo = j5}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa6)) j6) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa8), jogo = j8}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa8)) j8) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa6), jogo = j6}
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa7)) j7) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa8), jogo = j8}
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa8)) j8) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa7), jogo = j7}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa7)) j7) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa9), jogo = j9}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa9)) j9) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa7), jogo = j7}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa8)) j8) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa10), jogo = j10}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa10)) j10) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa8), jogo = j8}
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa9)) j9) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa10), jogo = j10}
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False Mapa10)) j10) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False Mapa9), jogo = j9}
-- Utilização da Tarefa 4.
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar _) _) = return $ e{jogo = moveDireita (jogo e)}
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar _) _) = return $ e{jogo = moveEsquerda (jogo e)}
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar _) _) = return $ e{jogo = podeTrepar (jogo e)}
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar _) _) = return $ e{jogo = interageCaixa (jogo e)}
-- Mudança apenas de direção (Este/Oeste).
reageEventoGloss (EventKey (Char '1') Down _ _) e@(EstadoGloss _ (MenuJogar _) (Jogo m (Jogador c Oeste eval))) = return $ e{jogo = (Jogo m (Jogador c Este eval))}
reageEventoGloss (EventKey (Char '2') Down _ _) e@(EstadoGloss _ (MenuJogar _) (Jogo m (Jogador c Este eval))) = return $ e{jogo = (Jogo m (Jogador c Oeste eval))}
-- Reiniciar o jogo em questão.
reageEventoGloss (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True Mapa1)) _) = return $ e{jogo = j1}
reageEventoGloss (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True Mapa2)) _) = return $ e{jogo = j2}
reageEventoGloss (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True Mapa3)) _) = return $ e{jogo = j3}
reageEventoGloss (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True Mapa4)) _) = return $ e{jogo = j4}
reageEventoGloss (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True Mapa5)) _) = return $ e{jogo = j5}
reageEventoGloss (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True Mapa6)) _) = return $ e{jogo = j6}
reageEventoGloss (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True Mapa7)) _) = return $ e{jogo = j7}
reageEventoGloss (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True Mapa8)) _) = return $ e{jogo = j8}
reageEventoGloss (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True Mapa9)) _) = return $ e{jogo = j9}
reageEventoGloss (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True Mapa10)) _) = return $ e{jogo = j10}
-- Fechar o programa a qualquer altura.
reageEventoGloss (EventKey (Char 'q') Down _ _) _ =
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
    Just background <- loadJuicyJPG "assets/background.jpg"
    -- MenuPrincipal.
    Just mpJogar <- loadJuicyJPG "assets/mpJogar.jpg"
    Just mpOpcoes <- loadJuicyJPG "assets/mpOpcoes.jpg"
    Just mpCreditos <- loadJuicyJPG "assets/mpCreditos.jpg"
    Just mpSair <- loadJuicyJPG "assets/mpSair.jpg"
    -- Opções do MenuPrincipal.
    Just creditos <- loadJuicyJPG "assets/creditos.jpg"
    -- MenuJogar.
    Just mjMapa1 <- loadJuicyJPG "assets/mjMapa1.jpg"
    Just mjMapa2 <- loadJuicyJPG "assets/mjMapa2.jpg"
    Just mjMapa3 <- loadJuicyJPG "assets/mjMapa3.jpg"
    Just mjMapa4 <- loadJuicyJPG "assets/mjMapa4.jpg"
    Just mjMapa5 <- loadJuicyJPG "assets/mjMapa5.jpg"
    Just mjMapa6 <- loadJuicyJPG "assets/mjMapa6.jpg"
    Just mjMapa7 <- loadJuicyJPG "assets/mjMapa7.jpg"
    Just mjMapa8 <- loadJuicyJPG "assets/mjMapa8.jpg"
    Just mjMapa9 <- loadJuicyJPG "assets/mjMapa9.jpg"
    Just mjMapa10 <- loadJuicyJPG "assets/mjMapa10.jpg"
    Just setaBaixo <- loadJuicyPNG "assets/setaBaixo.png"
    Just setaCima <- loadJuicyPNG "assets/setaCima.png"   
    -- Outros.
    Just bloco <- loadJuicyPNG "assets/bloco.png"
    Just caixa <- loadJuicyPNG "assets/caixa.png"
    Just porta <- loadJuicyPNG "assets/porta.png"
    Just knightLeft <- loadJuicyPNG "assets/knightLeft.png"
    Just knightRight <- loadJuicyPNG "assets/knightRight.png"
    return (Imagens background mpJogar mpOpcoes mpCreditos mpSair creditos mjMapa1 mjMapa2 mjMapa3 mjMapa4 mjMapa5 mjMapa6 mjMapa7 mjMapa8 mjMapa9 mjMapa10 setaBaixo setaCima bloco caixa porta knightLeft knightRight)

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
        -- MenuPrincipal.
        MenuPrincipal Jogar -> return $ Scale x' y' $ mpJogar i
        MenuPrincipal (Opcoes False) -> return $ Scale x' y' $ mpOpcoes i
        MenuPrincipal (Creditos False) -> return $ Scale x' y' $ mpCreditos i
        MenuPrincipal Sair -> return $ Scale x' y' $ mpSair i
        MenuPrincipal (Creditos True) -> return $ Scale x' y' $ creditos i
        -- MenuJogar.
        MenuJogar (EscolheMapa False Mapa1) -> return $ Pictures [Scale x' y' $ mjMapa1 i, setaBaixo i]
        MenuJogar (EscolheMapa False Mapa2) -> return $ Pictures [Scale x' y' $ mjMapa2 i, setaBaixo i]
        MenuJogar (EscolheMapa False Mapa3) -> return $ Pictures [Scale x' y' $ mjMapa3 i, setaCima i, setaBaixo i]
        MenuJogar (EscolheMapa False Mapa4) -> return $ Pictures [Scale x' y' $ mjMapa4 i, setaCima i, setaBaixo i]
        MenuJogar (EscolheMapa False Mapa5) -> return $ Pictures [Scale x' y' $ mjMapa5 i, setaCima i, setaBaixo i]
        MenuJogar (EscolheMapa False Mapa6) -> return $ Pictures [Scale x' y' $ mjMapa6 i, setaCima i, setaBaixo i]
        MenuJogar (EscolheMapa False Mapa7) -> return $ Pictures [Scale x' y' $ mjMapa7 i, setaCima i]
        MenuJogar (EscolheMapa False Mapa8) -> return $ Pictures [Scale x' y' $ mjMapa8 i, setaCima i]
        MenuJogar (EscolheMapa False Mapa9) -> return $ Pictures [Scale x' y' $ mjMapa9 i, setaCima i]
        MenuJogar (EscolheMapa False Mapa10) -> return $ Pictures [Scale x' y' $ mjMapa10 i, setaCima i]
        MenuJogar (EscolheMapa True _) -> return $ Pictures [background i, deslocaMapa m' $ Pictures [desenhaMapa e m', desenhaJogador e]]
        
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
        Porta -> Pictures [Translate x' y' $ Scale 0.4 0.4 $ porta i, desenhaMapa e t]
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
