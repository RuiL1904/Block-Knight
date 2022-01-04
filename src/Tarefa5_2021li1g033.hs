{- |
Module      : Tarefa5_2021li1g033
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import LI12122
import Tarefa1_2021li1g033
import Tarefa2_2021li1g033
import Tarefa4_2021li1g033

import Utils
import Assets
import Niveis
import HighScore
import SaveGame
import Audio

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

import System.Exit
import Data.Char
import Data.List

-- | O estado do programa.
data EstadoGloss = EstadoGloss {
    imagens :: Imagens, -- ^ O conjunto das 'Imagens' necessárias ao programa.
    menuAtual :: Menu, -- ^ O 'Menu' em que o utilizador está atualmente.
    jogo :: Jogo, -- ^ O 'Jogo' atual.
    tempo :: Float, -- ^ O valor em segundos desde o início do programa.
    highScore :: HighScores, -- ^ A lista da pontuação mais alta obtida em cada um dos níveis.
    scoreAtual :: Int -- ^ A pontuação atual no 'Jogo' atual.
    -- Adicionar mais coisas, inerentes ao estado, aqui no futuro.
}

-- | Os menus disponíveis.
data Menu
    = MenuPrincipal OpcoesP
    | MenuJogar OpcoesJ 
    deriving Eq

-- | As opções do 'MenuPrincipal' disponíveis.
data OpcoesP
    = Jogar
    | Opcoes Bool -- O 'Bool' dita se o jogador está ou não dentro do 'Menu' em questão.
    | Creditos Bool -- O 'Bool' dita se o jogador está ou não dentro do 'Menu' em questão.
    | Sair 
    deriving Eq

-- | As opções do 'MenuJogar' disponíveis.
data OpcoesJ
    = EscolheMapa Bool Int -- O 'Bool' dita se o jogador está ou não a jogar um 'Mapa'.
    | ModoArcade Bool Int -- O 'Bool' dita se o jogador está ou não em jogo.
    | CarregarJogo Bool
    deriving Eq

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
reageEventoGloss ev e@(EstadoGloss _ (MenuPrincipal _) _ _ _ _) = reageEventoMenuPrincipal ev e
reageEventoGloss ev e@(EstadoGloss _ (MenuJogar _) _ _ _ _) = reageEventoMenuJogar ev e

reageEventoMenuPrincipal :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoMenuPrincipal (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal Jogar) _ _ _ _) = return $ e{menuAtual = MenuJogar (EscolheMapa False 0)}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes False)) _ _ _ _) = return $ e{menuAtual = MenuPrincipal (Opcoes True)}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos False)) _ _ _ _) = return $ e{menuAtual = MenuPrincipal (Creditos True)}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal Sair) _ _ _ _) =
    do killProcess
       exitSuccess
reageEventoMenuPrincipal (EventKey _ Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos True)) _ _ _ _) = return $ e{menuAtual = MenuPrincipal (Creditos False)}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ _ _ _ _ _) = 
    do playMenuChange
       return $ e{menuAtual = obtemMenu Cima e}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ _ _ _ _ _) = 
    do playMenuChange
       return $ e{menuAtual = obtemMenu Baixo e} 
reageEventoMenuPrincipal (EventKey (Char 'q') Down _ _) _ =
    do killProcess
       exitSuccess
reageEventoMenuPrincipal _ e = return e

data Direcao' = Cima | Baixo deriving Eq

obtemMenu :: Direcao' -> EstadoGloss -> Menu
obtemMenu dir e =
    case dir of
        Cima ->
            case m of
                (MenuPrincipal Jogar) -> m
                _ -> menus !! (i - 1)
        _ ->
            case m of
                (MenuPrincipal Sair) -> m
                _ -> menus !! (i + 1)
    where m = (menuAtual e)
          i = retiraDoJust $ elemIndex m menus 
          menus = [MenuPrincipal Jogar, MenuPrincipal (Opcoes False), MenuPrincipal (Creditos False), MenuPrincipal Sair]

reageEventoMenuJogar :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoMenuJogar ev e@(EstadoGloss _ (MenuJogar (EscolheMapa _ _)) _ _ _ _) = reageEventoEscolheMapa ev e
reageEventoMenuJogar ev e@(EstadoGloss _ (MenuJogar (ModoArcade _ _)) _ _ _ _) = reageEventoModoArcade ev e
reageEventoMenuJogar ev e@(EstadoGloss _ (MenuJogar (CarregarJogo _)) _ _ _ _) = reageEventoCarregarJogo ev e
reageEventoMenuJogar (EventKey (Char 'q') Down _ _) _ =
    do killProcess
       exitSuccess
reageEventoMenuJogar _ e = return e

reageEventoEscolheMapa :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoEscolheMapa (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False 0)) _ _ _ _) = return $ e{menuAtual = MenuJogar (EscolheMapa False 1)}
reageEventoEscolheMapa (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) j _ hs sa) = 
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = moveDireita j
       if encontraPorta (moveJogador j AndarDireita) == novoC then
           do let novoHS = (verificaHScore x e{scoreAtual = sa + 1} hs)
              escreverFicheiro novoHS
              return $ e{menuAtual = MenuJogar (EscolheMapa False 1), jogo = obtemNivel x, highScore = novoHS, scoreAtual = 0} else if novoJ /= j then return $ e{jogo = novoJ, scoreAtual = sa + 1} else return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) j _ hs sa) = 
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = moveEsquerda j
       if encontraPorta (moveJogador j AndarEsquerda) == novoC && novoJ /= j then
           do let novoHS = (verificaHScore x e{scoreAtual = sa + 1} hs)
              escreverFicheiro novoHS
              return $ e{menuAtual = MenuJogar (EscolheMapa False 1), jogo = obtemNivel x, highScore = novoHS, scoreAtual = 0} else if novoJ /= j then return $ e{jogo = novoJ, scoreAtual = sa + 1} else return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) j _ hs sa) = 
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = podeTrepar j
       if encontraPorta (moveJogador j Trepar) == novoC && novoJ /= j then
           do let novoHS = (verificaHScore x e{scoreAtual = sa + 1} hs)
              escreverFicheiro novoHS
              return $ e{menuAtual = MenuJogar (EscolheMapa False 1), jogo = obtemNivel x, highScore = novoHS, scoreAtual = 0} else if novoJ /= j then return $ e{jogo = novoJ, scoreAtual = sa + 1} else return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) j _ _ sa) = 
    do let novoJ@(Jogo _ (Jogador _ _ _)) = interageCaixa j
       if novoJ /= j then return $ e{jogo = interageCaixa j, scoreAtual = sa + 1} else return e
reageEventoEscolheMapa (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) _ _ _ _) = return $ e{jogo = obtemNivel x, scoreAtual = 0}
reageEventoEscolheMapa (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False 0)) _ _ _ _)= 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (ModoArcade False 0)}      
reageEventoEscolheMapa (EventKey (SpecialKey KeyEnter) Down _ _ ) e@ (EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _) = 
    do killProcess
       return $ e{menuAtual = MenuJogar (EscolheMapa True x)}
reageEventoEscolheMapa (EventKey (SpecialKey KeyEsc) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _) =
    do killProcess
       playMenuPrincipal
       return $ e{menuAtual = MenuPrincipal Jogar}
reageEventoEscolheMapa (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _)
    | not (x `elem` [1,3,5,7,9]) = 
        do playMenuChange
           return $ e{menuAtual = MenuJogar (EscolheMapa False (x - 1)), jogo = obtemNivel (x - 1), scoreAtual = 0}
    | otherwise= return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _)
    | not (x `elem` [2,4,6,8,10]) =
        do playMenuChange
           return $ e{menuAtual = MenuJogar (EscolheMapa False (x + 1)), jogo = obtemNivel (x + 1), scoreAtual = 0}
    | otherwise = return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _)
    | not (x `elem` [9,10]) =
        do playMenuChange
           return $ e{menuAtual = MenuJogar (EscolheMapa False (x + 2)), jogo = obtemNivel (x + 2), scoreAtual = 0}
    | otherwise = return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _)
    | not (x `elem` [1,2]) =
        do playMenuChange
           return $ e{menuAtual = MenuJogar (EscolheMapa False (x - 2)), jogo = obtemNivel (x - 2), scoreAtual = 0}
    | otherwise = return e
reageEventoEscolheMapa (EventKey (Char 'q') Down _ _) _ =
    do killProcess
       exitSuccess
reageEventoEscolheMapa _ e = return e

-- | Verifica se o 'scoreAtual' é menor do que o 'highScore', se sim atribui um novo 'highScore'.
verificaHScore :: Int -> EstadoGloss -> HighScores -> HighScores
verificaHScore x e hs
    | sa < i = alteraHScore (x - 1) sa hs
    | otherwise = hs 
    where i = obtemHScore x e
          sa = (scoreAtual e)

-- | Altera o 'highScore' na posição dada.
alteraHScore :: Int -> Int -> HighScores -> HighScores
alteraHScore _ _ [] = []
alteraHScore x sa (h:t)
    | x == 0 = sa : t
    | otherwise = h : alteraHScore (x - 1) sa t

    -- | Obtém a pontuação mais alta a que se refere o 'Int' dado.
obtemHScore :: Int -> EstadoGloss -> Int
obtemHScore x e = hs !! (x - 1)
    where hs = (highScore e)

-- | Obtém o 'Jogo' a que se refere o 'Int' dado.
obtemNivel :: Int -> Jogo
obtemNivel x = jogos !! (x - 1)
    where jogos = [j1, j2, j3, j4, j5, j6, j7, j8, j9, j10]

reageEventoModoArcade :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoModoArcade (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade False 0)) _ _ _ _) = 
    do killProcess
       return $ e{menuAtual = MenuJogar (ModoArcade True 1)}
reageEventoModoArcade (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade False 0)) _ _ _ _) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False 0)}
reageEventoModoArcade (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade False 0)) _ _ _ _) =
    do playMenuChange
       return $ e{menuAtual = MenuJogar (CarregarJogo False)}
reageEventoModoArcade (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) _ _ _ _) = return $ e{jogo = obtemNivel x, scoreAtual = 0}
reageEventoModoArcade (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) j _ _ _) =
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = moveDireita j
       if encontraPorta (moveJogador j AndarDireita) == novoC then return $ e{menuAtual = MenuJogar (ModoArcade True (x + 1)), jogo = obtemNivel (x + 1)} else return $ e{jogo = novoJ}
reageEventoModoArcade (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) j _ _ _) =
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = moveEsquerda j
       if encontraPorta (moveJogador j AndarEsquerda) == novoC then return $ e{menuAtual = MenuJogar (ModoArcade True (x + 1)), jogo = obtemNivel (x + 1)} else return $ e{jogo = novoJ}
reageEventoModoArcade (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) j _ _ _) =
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = podeTrepar j
       if encontraPorta (moveJogador j AndarDireita) == novoC then return $ e{menuAtual = MenuJogar (ModoArcade True (x + 1)), jogo = obtemNivel (x + 1)} else return $ e{jogo = novoJ}
reageEventoModoArcade (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) j _ _ _) = return $ e{jogo = interageCaixa j}
reageEventoModoArcade (EventKey (Char 'g') Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) j _ _ _) =
    do escreverSGame (jogoParaGuardado j)
       killProcess
       exitSuccess 
reageEventoModoArcade (EventKey (Char 'q') Down _ _) _ =
    do killProcess
       exitSuccess                   
reageEventoModoArcade _ e = return e

reageEventoCarregarJogo :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoCarregarJogo (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (CarregarJogo False)) _ _ _ _) =
    do playMenuChange
       return $ e{menuAtual = MenuJogar (ModoArcade False 0)}
reageEventoCarregarJogo (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuJogar (CarregarJogo False)) _ _ _ _) = 
    do s <- lerSGame
       return $ e{menuAtual = MenuJogar (CarregarJogo True), jogo = guardadoParaJogo s}
reageEventoCarregarJogo (EventKey (Char 'q') Down _ _) _ =
    do killProcess
       exitSuccess      
reageEventoCarregarJogo _ e = return e

-- | Retorna as 'Coordenadas' onde se encontra a 'Porta'.
encontraPorta :: Jogo -> Coordenadas
encontraPorta (Jogo m j) =
    case p of
        Porta -> c
        _ -> encontraPorta (Jogo (constroiMapa t) j)
    where ((p,c@(x,y)):t) = desconstroiMapa m

-- | Função que reage à passagem do tempo.
reageTempoGloss :: 
    Float -> -- ^ Um 'Float' que contabiliza o tempo que passou desde a última chamada da função 'reageTempoGloss'.
    EstadoGloss ->
    IO EstadoGloss
reageTempoGloss n e@(EstadoGloss _ _ _ x _ _) = return e{tempo = x + n}

-- | Desenha o estado do programa, consoante algumas variáveis - transforma um estado numa 'Picture'.
desenhaEstadoGloss :: EstadoGloss -> IO Picture
desenhaEstadoGloss e = do
    (x,y) <- getScreenSize
    let x' = (fromIntegral x) / 1920 -- Largura da imagem.
    let y' = (fromIntegral y) / 1080 -- Altura da imagem.
    let i = (imagens e)
    let (Jogo m _) = (jogo e)
    let t = (tempo e)
    let m' = desconstroiMapa m
    
    case (menuAtual e) of
        MenuPrincipal Jogar -> return $ Scale x' y' $ mpJogar i
        MenuPrincipal (Opcoes False) -> return $ Scale x' y' $ mpOpcoes i
        MenuPrincipal (Creditos False) -> return $ Scale x' y' $ mpCreditos i
        MenuPrincipal Sair -> return $ Scale x' y' $ mpSair i
        MenuPrincipal (Creditos True) -> return $ Scale x' y' $ creditos i

        MenuJogar (EscolheMapa False 0) -> return $ mjEscolhe i
        MenuJogar (ModoArcade False 0) -> return $ mjArcade i
        MenuJogar (CarregarJogo False) -> return $ mjCarregar i

        MenuJogar (EscolheMapa False x) -> return $ Scale x' y' $ obtemPicture x e
        MenuJogar (EscolheMapa True _) -> return $ Pictures [background i, desenhaHScore e, desenhaScoreAtual e, deslocaMapa m' $ Pictures [desenhaMapa e m', desenhaJogador e]]

        MenuJogar (ModoArcade True _) -> return $ Pictures [background i, deslocaMapa m' $ Pictures [desenhaMapa e m', desenhaJogador e]]
        
        MenuJogar (CarregarJogo True) -> return $ Pictures [background i, deslocaMapa m' $ Pictures [desenhaMapa e m', desenhaJogador e]]

-- | Obtém a 'Picture' a que se refere o 'Int' dado.
obtemPicture :: Int -> EstadoGloss -> Picture
obtemPicture x e = aplicaEfeitos x e (pictures !! (x - 1))
    where i = (imagens e) 
          pictures = [mj1 i, mj2 i, mj3 i, mj4 i, mj5 i, mj6 i, mj7 i, mj8 i, mj9 i, mj10 i]

aplicaEfeitos :: Int -> EstadoGloss -> Picture -> Picture 
aplicaEfeitos x e p
    | x `elem` [1,2] = Pictures [p, setaBaixo i]
    | x `elem` [3,4,5,6] = Pictures [p, setaBaixo i, setaCima i]
    | otherwise = Pictures [p, setaCima i]
    where i = (imagens e)

-- | Função auxiliar de 'desenhaEstadoGloss' - desenha o 'Mapa'.
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
          y' = (-1.0) * (fromIntegral y) * 50

-- | Função auxiliar de 'desenhaEstadoGloss' - desenha o 'Jogador'.
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
          y' = (-1.0) * (fromIntegral y) * 50

-- | Função auxiliar de 'desenhaEstadoGloss' - desenha o 'highScore'.
desenhaHScore :: EstadoGloss -> Picture
desenhaHScore e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) _ _ _ _)
    | obtemHScore x e == 1000 = Scale 0.4 0.4 $ Translate 1300 1100 $ Color white $ Text ("High Score: Null")
    | otherwise = Scale 0.4 0.4 $ Translate 1300 1100 $ Color white $ Text ("High Score: " ++ hsParaString)
    where hsParaString = show $ (obtemHScore x e)

-- | Função auxiliar de 'desenhaEstadoGloss' - desenha o 'scoreAtual'.
desenhaScoreAtual :: EstadoGloss -> Picture
desenhaScoreAtual e@(EstadoGloss _ (MenuJogar (EscolheMapa True _)) _ _ _ _) = Scale 0.4 0.4 $ Translate 1300 950 $ Color white $ Text ("Score atual: " ++ saParaString)
    where saParaString = show $ (scoreAtual e)

-- Função auxiliar de 'desenhaEstadoGloss' - centra o 'Mapa' nas coordenadas (0,0).
deslocaMapa :: [(Peca, Coordenadas)] -> Picture -> Picture
deslocaMapa m i = Translate x y i
    where x =  (-1.0) * (fromIntegral (div l 2))
          y = (fromIntegral (div a 2))
          l = (larguraMapa m) * 50
          a = (alturaMapa m) * 50

main :: IO ()
main = do
    playMenuPrincipal
    imagens <- carregaImagens
    highScores <- lerFicheiro    
    let jogoInicial = j1
        estadoGlossInicial = (EstadoGloss imagens (MenuPrincipal Jogar) jogoInicial 0 highScores 0)
    playIO window 
        (greyN 0.32) 
        fr
        estadoGlossInicial
        desenhaEstadoGloss
        reageEventoGloss
        reageTempoGloss
