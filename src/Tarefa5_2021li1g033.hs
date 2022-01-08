{- |
Module      : Tarefa5_2021li1g033
Description : O objetivo desta tarefa é __implementar todas as tarefas anteriormente realizadas__ de modo a __construir uma aplicação gráfica__ que permita ao utilizador jogar. Isto usando a biblioteca _Gloss_.

Nesta tarefa, implementámos um estilo visual baseado num jogo chamado Hollow Knight. 
Ao abrir o jogo, a tela apresenta-nos o Menu Principal dividido em quatro opções: Jogar | Opções | Créditos | Sair . 

Ao escolher a opção Jogar, o utilizador entra noutro menu dividido em três opções: Escolher Mapa | Modo Arcade | Carregar Jogo.
A opção de escolher o mapa permite ao utilizador navegar através de 10 mapas escolhendo o que prefere.
O modo arcade centra-se em completar o jogo, na íntegra (10 mapas), de uma vez só, com a possibilidade de guardar o progresso e regressar mais tarde com a opção carregar jogo.

Dentro do menu opções são mostrados os controlos para jogar e existe ainda uma opção para trocar o background de todos os mapas.
Os créditos apresentam os autores do jogo.

Após a realização das duas fases do projeto, é notória a aprendizagem e o mais à vontade que temos com esta linguagem de programação. 
Foi um projeto desafiante que, muitas vezes, colocou os nossos dias apenas e exclusivamente dedicados a esta UC. 
Este é um jogo simples (comparado com outros jogos existentes nesta época), mas que foi muito exigente em termos de pensamento e estrutura. Inicialmente, nem a primeira tarefa éramos capazes de terminar. 
As principais dificuldades sentidas foram mesmo passar o nosso raciocínio para código.
Gostávamos de ter implementado outras "features", tais como um editor de mapas e a existências de mais músicas ao longo do jogo.

Bibliografia:

-- ** Guião Gloss feito pelo CeSIUM - https://www.youtube.com/watch?v=VDVGO3BTGYk&list=PLadvWyx_6w6XiJ95A4MqSfmIaRVbXWFGS
-- ** Documentação do Gloss - https://hackage.haskell.org/package/gloss
-- ** Fórum Stack Overflow
-- ** Material disponibilizado pelos docentes da UC
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
    scoreAtual :: Int, -- ^ A pontuação atual no 'Jogo' atual.
    bgSelecionado :: Int -- ^ Indica qual o background selecionado pelo utilizador no menu das opções.
}

-- | Os menus disponíveis.
data Menu
    = MenuPrincipal OpcoesP
    | MenuJogar OpcoesJ 
    deriving Eq

-- | As opções do 'MenuPrincipal' disponíveis.
data OpcoesP
    = Jogar
    | Opcoes Bool Int -- O 'Bool' dita se o jogador está ou não dentro do 'Menu' em questão e o 'Int' dita qual dos backgrounds foi selecionado.
    | Creditos Bool -- O 'Bool' dita se o jogador está ou não dentro do 'Menu' em questão.
    | Sair 
    deriving Eq

-- | As opções do 'MenuJogar' disponíveis.
data OpcoesJ
    = EscolheMapa Bool Int -- O 'Bool' dita se o jogador está ou não a jogar um 'Mapa' e o 'Int' indica qual o mapa selecionado ou a ser jogado.
    | ModoArcade Bool Int -- O 'Bool' dita se o jogador está ou não em jogo e o 'Int' indical qual o jogo a ser jogado atualmente.
    | CarregarJogo Bool Int -- O 'Bool' dita se o jogador está ou não em jogo e o 'Int' indica qual o jogo do modo arcade.
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
reageEventoGloss ev e@(EstadoGloss _ (MenuPrincipal _) _ _ _ _ _) = reageEventoMenuPrincipal ev e
reageEventoGloss ev e@(EstadoGloss _ (MenuJogar _) _ _ _ _ _) = reageEventoMenuJogar ev e

-- | Auxilar da função 'reageEventoGloss' - generaliza a função para o menu principal.
reageEventoMenuPrincipal :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoMenuPrincipal (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal Jogar) _ _ _ _ _) = return $ e{menuAtual = MenuJogar (EscolheMapa False 0)}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes False 0)) _ _ _ _ _) = return $ e{menuAtual = MenuPrincipal (Opcoes True 1)}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyEsc) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes True _)) _ _ _ _ _) = return $ e{menuAtual = MenuPrincipal (Opcoes False 0)}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes True x)) _ _ _ _ _) = if x == 4 then return e else 
    do playMenuChange
       return $ e{menuAtual = MenuPrincipal (Opcoes True (x + 1))}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes True x)) _ _ _ _ _ ) = if x `elem` [0,1] then return e else
    do playMenuChange
       return $ e{menuAtual = MenuPrincipal (Opcoes True (x - 1))}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Opcoes True x)) _ _ _ _ _) = return $ e{menuAtual = MenuPrincipal (Opcoes False 0), bgSelecionado = x}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos False)) _ _ _ _ _) = return $ e{menuAtual = MenuPrincipal (Creditos True)}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuPrincipal Sair) _ _ _ _ _) =
    do killProcess
       exitSuccess
reageEventoMenuPrincipal (EventKey _ Down _ _) e@(EstadoGloss _ (MenuPrincipal (Creditos True)) _ _ _ _ _) = return $ e{menuAtual = MenuPrincipal (Creditos False)}
reageEventoMenuPrincipal (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ _ _ _ _ _ _) = if obtemMenu Cima e /= Nothing then 
    do playMenuChange
       return $ e{menuAtual = retiraDoJust $ obtemMenu Cima e} else return e
reageEventoMenuPrincipal (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ _ _ _ _ _ _) = if obtemMenu Baixo e /= Nothing then 
    do playMenuChange
       return $ e{menuAtual = retiraDoJust $ obtemMenu Baixo e} else return e
reageEventoMenuPrincipal (EventKey (Char 'q') Down _ _) _ =
    do killProcess
       exitSuccess
reageEventoMenuPrincipal _ e = return e

-- | Um novo data type que toma os valores da direção. Seta para baixo: Baixo; Seta para cima: Cima.
data Direcao' = Cima | Baixo deriving Eq

-- | Função que óbtem o próximo 'Menu' de acordo com a direção que o utilizador "escolheu".
obtemMenu :: Direcao' -> EstadoGloss -> Maybe Menu
obtemMenu dir e =
    case dir of
        Cima ->
            case m of
                (MenuPrincipal Jogar) -> Nothing
                _ -> Just $ menus !! (i - 1)
        _ ->
            case m of
                (MenuPrincipal Sair) -> Nothing
                _ -> Just $ menus !! (i + 1)
    where m = (menuAtual e)
          i = retiraDoJust $ elemIndex m menus 
          menus = [MenuPrincipal Jogar, MenuPrincipal (Opcoes False 0), MenuPrincipal (Creditos False), MenuPrincipal Sair]

-- | Auxilar da função 'reageEventoGloss' - generaliza a função para o menu jogar.
reageEventoMenuJogar :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoMenuJogar ev e@(EstadoGloss _ (MenuJogar (EscolheMapa _ _)) _ _ _ _ _) = reageEventoEscolheMapa ev e
reageEventoMenuJogar ev e@(EstadoGloss _ (MenuJogar (ModoArcade _ _)) _ _ _ _ _) = reageEventoModoArcade ev e
reageEventoMenuJogar ev e@(EstadoGloss _ (MenuJogar (CarregarJogo _ _)) _ _ _ _ _) = reageEventoCarregarJogo ev e
reageEventoMenuJogar (EventKey (Char 'q') Down _ _) _ =
    do killProcess
       exitSuccess
reageEventoMenuJogar _ e = return e

-- | Auxilar da função 'reageEventoMenujogar' - generaliza a função para a opção escolhe mapa do menu jogar.
reageEventoEscolheMapa :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoEscolheMapa (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False 0)) _ _ _ _ _) = return e -- Dar fix a bug estranho.
reageEventoEscolheMapa (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False 0)) _ _ _ _ _) = return $ e{menuAtual = MenuJogar (EscolheMapa False 1)}
reageEventoEscolheMapa (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) j _ hs sa _) = 
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = moveDireita j
       if encontraPorta (moveJogador j AndarDireita) == novoC then
           do let novoHS = (verificaHScore x e{scoreAtual = sa + 1} hs)
              escreverFicheiro novoHS
              return $ e{menuAtual = MenuJogar (EscolheMapa False 1), jogo = obtemNivel x, highScore = novoHS, scoreAtual = 0} else if novoJ /= j then return $ e{jogo = novoJ, scoreAtual = sa + 1} else return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) j _ hs sa _) = 
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = moveEsquerda j
       if encontraPorta (moveJogador j AndarEsquerda) == novoC && novoJ /= j then
           do let novoHS = (verificaHScore x e{scoreAtual = sa + 1} hs)
              escreverFicheiro novoHS
              return $ e{menuAtual = MenuJogar (EscolheMapa False 1), jogo = obtemNivel x, highScore = novoHS, scoreAtual = 0} else if novoJ /= j then return $ e{jogo = novoJ, scoreAtual = sa + 1} else return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) j _ hs sa _) = 
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = podeTrepar j
       if encontraPorta (moveJogador j Trepar) == novoC && novoJ /= j then
           do let novoHS = (verificaHScore x e{scoreAtual = sa + 1} hs)
              escreverFicheiro novoHS
              return $ e{menuAtual = MenuJogar (EscolheMapa False 1), jogo = obtemNivel x, highScore = novoHS, scoreAtual = 0} else if novoJ /= j then return $ e{jogo = novoJ, scoreAtual = sa + 1} else return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) j _ _ sa _) = 
    do let novoJ@(Jogo _ (Jogador _ _ _)) = interageCaixa j
       if novoJ /= j then return $ e{jogo = interageCaixa j, scoreAtual = sa + 1} else return e
reageEventoEscolheMapa (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) _ _ _ _ _) = return $ e{jogo = obtemNivel x, scoreAtual = 0}
reageEventoEscolheMapa (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False 0)) _ _ _ _ _)= 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (ModoArcade False 0)}      
reageEventoEscolheMapa (EventKey (SpecialKey KeyEnter) Down _ _ ) e@ (EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _ _) = 
    do killProcess
       return $ e{menuAtual = MenuJogar (EscolheMapa True x)}
reageEventoEscolheMapa (EventKey (SpecialKey KeyEsc) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False 0)) _ _ _ _ _) = return $ e{menuAtual = MenuPrincipal Jogar}
reageEventoEscolheMapa (EventKey (SpecialKey KeyEsc) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _ _) = return $ e{menuAtual = MenuJogar (EscolheMapa False 0)}
reageEventoEscolheMapa (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _ _)
    | not (x `elem` [1,3,5,7,9]) = 
        do playMenuChange
           return $ e{menuAtual = MenuJogar (EscolheMapa False (x - 1)), jogo = obtemNivel (x - 1), scoreAtual = 0}
    | otherwise= return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _ _)
    | not (x `elem` [2,4,6,8,10]) =
        do playMenuChange
           return $ e{menuAtual = MenuJogar (EscolheMapa False (x + 1)), jogo = obtemNivel (x + 1), scoreAtual = 0}
    | otherwise = return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _ _)
    | not (x `elem` [9,10]) =
        do playMenuChange
           return $ e{menuAtual = MenuJogar (EscolheMapa False (x + 2)), jogo = obtemNivel (x + 2), scoreAtual = 0}
    | otherwise = return e
reageEventoEscolheMapa (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa False x)) _ _ _ _ _)
    | not (x `elem` [1,2]) =
        do playMenuChange
           return $ e{menuAtual = MenuJogar (EscolheMapa False (x - 2)), jogo = obtemNivel (x - 2), scoreAtual = 0}
    | otherwise = return e
reageEventoEscolheMapa (EventKey (Char 'x') Down _ _) e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) _ _ _ _ _) =
    do playMenuPrincipal
       return $ e{menuAtual = MenuJogar (EscolheMapa False 0), scoreAtual = 0}
reageEventoEscolheMapa (EventKey (Char 'q') Down _ _) _ =
    do killProcess
       exitSuccess
reageEventoEscolheMapa _ e = return e

-- | Verifica se o 'scoreAtual' é menor do que o 'highScore', se sim atribui um novo 'highScore'.
verificaHScore :: 
    Int -> -- ^ O 'Int' referente ao mapa atual.
    EstadoGloss -> 
    HighScores -> -- ^ Os 'HighScores' atuais.
    HighScores -- ^ ^ Os 'HighScores' depois de terem (ou não) sofrido alteração.
verificaHScore x e hs
    | sa < i = alteraHScore (x - 1) sa hs
    | otherwise = hs 
    where i = obtemHScore x e
          sa = (scoreAtual e)

-- | Auxilar da função 'verificaHScore' - altera o 'highScore' na posição dada.
alteraHScore :: Int -> Int -> HighScores -> HighScores
alteraHScore _ _ [] = []
alteraHScore x sa (h:t)
    | x == 0 = sa : t
    | otherwise = h : alteraHScore (x - 1) sa t

-- | Obtém a pontuação mais alta a que se refere o 'Int' dado.
obtemHScore :: 
    Int -- ^ O 'Int' referente ao mapa atual.
    -> EstadoGloss 
    -> Int 
obtemHScore x e = hs !! (x - 1)
    where hs = (highScore e)

-- | Obtém o 'Jogo' a que se refere o 'Int' dado.
obtemNivel :: 
    Int -- ^ O 'Int' referente ao mapa atual.
    -> Jogo -- ^ O 'Jogo' escolhido.
obtemNivel x = jogos !! (x - 1)
    where jogos = [j1, j2, j3, j4, j5, j6, j7, j8, j9, j10]

-- | Auxilar da função 'reageEventoMenujogar' - generaliza a função para a opção modo arcade do menu jogar.
reageEventoModoArcade :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoModoArcade (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade False 0)) _ _ _ _ _) = 
    do killProcess
       return $ e{menuAtual = MenuJogar (ModoArcade True 1)}
reageEventoModoArcade (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade False 0)) _ _ _ _ _) = 
    do playMenuChange
       return $ e{menuAtual = MenuJogar (EscolheMapa False 0)}
reageEventoModoArcade (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade False 0)) _ _ _ _ _) =
    do playMenuChange
       return $ e{menuAtual = MenuJogar (CarregarJogo False 0)}
reageEventoModoArcade (EventKey (SpecialKey KeyEsc) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade False 0)) _ _ _ _ _) = return $ e{menuAtual = MenuPrincipal Jogar}
reageEventoModoArcade (EventKey (Char 'r') Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) _ _ _ _ _) = return $ e{jogo = obtemNivel x}
reageEventoModoArcade (EventKey (SpecialKey KeyRight) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) j _ _ _ _) =
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = moveDireita j
       if encontraPorta (moveJogador j AndarDireita) == novoC then return $ e{menuAtual = MenuJogar (ModoArcade True (x + 1)), jogo = obtemNivel (x + 1)} else return $ e{jogo = novoJ}
reageEventoModoArcade (EventKey (SpecialKey KeyLeft) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) j _ _ _ _) =
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = moveEsquerda j
       if encontraPorta (moveJogador j AndarEsquerda) == novoC then return $ e{menuAtual = MenuJogar (ModoArcade True (x + 1)), jogo = obtemNivel (x + 1)} else return $ e{jogo = novoJ}
reageEventoModoArcade (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) j _ _ _ _) =
    do let novoJ@(Jogo _ (Jogador novoC _ _)) = podeTrepar j
       if encontraPorta (moveJogador j AndarDireita) == novoC then return $ e{menuAtual = MenuJogar (ModoArcade True (x + 1)), jogo = obtemNivel (x + 1)} else return $ e{jogo = novoJ}
reageEventoModoArcade (EventKey (SpecialKey KeyDown) Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) j _ _ _ _) = return $ e{jogo = interageCaixa j}
reageEventoModoArcade (EventKey (Char 'x') Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) _ _ _ _ _) =
    do playMenuPrincipal
       return $ e{menuAtual = MenuJogar (ModoArcade False 0)}
reageEventoModoArcade (EventKey (Char 'g') Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True x)) j _ _ _ _) =
    do escreverSGame (jogoParaGuardado x j)
       killProcess
       exitSuccess
reageEventoModoArcade (EventKey (Char 'v') Down _ _) e@(EstadoGloss _ (MenuJogar (ModoArcade True _)) _ _ _ _ _) = return $ e{menuAtual = MenuJogar (ModoArcade True 11)}
reageEventoModoArcade (EventKey (Char 'q') Down _ _) _ =
    do killProcess
       exitSuccess                   
reageEventoModoArcade _ e = return e

-- | Auxilar da função 'reageEventoMenujogar' - generaliza a função para a opção carregar jogo do menu jogar.
reageEventoCarregarJogo :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoCarregarJogo (EventKey (SpecialKey KeyUp) Down _ _) e@(EstadoGloss _ (MenuJogar (CarregarJogo False 0)) _ _ _ _ _) =
    do playMenuChange
       return $ e{menuAtual = MenuJogar (ModoArcade False 0)}
reageEventoCarregarJogo (EventKey (SpecialKey KeyEnter) Down _ _) e@(EstadoGloss _ (MenuJogar (CarregarJogo False 0)) _ _ _ _ _) = 
    do killProcess
       s@(JogoGuardado m j x) <- lerSGame
       return $ e{menuAtual = MenuJogar (ModoArcade True x), jogo = guardadoParaJogo s}      
reageEventoCarregarJogo (EventKey (SpecialKey KeyEsc) Down _ _) e@(EstadoGloss _ (MenuJogar (CarregarJogo False 0)) _ _ _ _ _) = return $ e{menuAtual = MenuPrincipal Jogar}
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
    Float  -- ^ Um 'Float' que contabiliza o tempo que passou desde a última chamada da função 'reageTempoGloss'.
    -> EstadoGloss 
    -> IO EstadoGloss
reageTempoGloss n e@(EstadoGloss _ _ _ x _ _ _) = return e{tempo = x + n}

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
        MenuPrincipal (Opcoes False 0) -> return $ Scale x' y' $ mpOpcoes i
        MenuPrincipal (Opcoes True x) -> return $ Scale x' y' $ obtemBackgroundOpcoes x e
        MenuPrincipal (Creditos False) -> return $ Scale x' y' $ mpCreditos i
        MenuPrincipal Sair -> return $ Scale x' y' $ mpSair i
        MenuPrincipal (Creditos True) -> return $ Scale x' y' $ creditos i

        MenuJogar (EscolheMapa False 0) -> return $ mjEscolhe i
        MenuJogar (ModoArcade False 0) -> return $ mjArcade i
        MenuJogar (CarregarJogo False 0) -> return $ mjCarregar i

        MenuJogar (EscolheMapa False x) -> return $ Scale x' y' $ obtemPicture x e
        MenuJogar (EscolheMapa True _) -> return $ Pictures [obtemBackground e, desenhaHScore e, desenhaScoreAtual e, deslocaMapa m' $ Pictures [desenhaMapa e m', desenhaJogador e]]

        MenuJogar (ModoArcade True 11) -> return $ Scale x' y' $ victory i
        MenuJogar (ModoArcade True _) -> return $ Pictures [obtemBackground e, deslocaMapa m' $ Pictures [desenhaMapa e m', desenhaJogador e]]

-- | Obtém a 'Picture' a que se refere o 'Int' dado.
obtemPicture :: 
    Int -- ^ O 'Int' referente ao mapa atual.
    -> EstadoGloss 
    -> Picture
obtemPicture x e = aplicaEfeitos x e (pictures !! (x - 1))
    where i = (imagens e) 
          pictures = [mj1 i, mj2 i, mj3 i, mj4 i, mj5 i, mj6 i, mj7 i, mj8 i, mj9 i, mj10 i]

-- | Aplica os efeitos necessários à 'Picture' dada.
aplicaEfeitos :: Int -> EstadoGloss -> Picture -> Picture 
aplicaEfeitos x e p
    | x `elem` [1,2] = Pictures [p, setaBaixo i]
    | x `elem` [3,4,5,6] = Pictures [p, setaBaixo i, setaCima i]
    | otherwise = Pictures [p, setaCima i]
    where i = (imagens e)

-- | Obtém a 'Picture' necessária, para o menu das opções, consoante o 'Int' dado.
obtemBackgroundOpcoes :: Int -> EstadoGloss -> Picture
obtemBackgroundOpcoes x e = bg !! (x - 1)
    where i = (imagens e)
          bg = [eBackground1 i, eBackground2 i, eBackground3 i, eBackground4 i]

-- | Obtém a 'Picture' necessária, para ser background de todos os mapas, consoante o 'Int' dado.
obtemBackground :: EstadoGloss -> Picture
obtemBackground e
    | x == 0 = bg !! 0
    | otherwise = bg !! (x - 1)
    where x = (bgSelecionado e)
          i = (imagens e)
          bg = [background1 i, background2 i, background3 i, background4 i]

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
desenhaHScore e@(EstadoGloss _ (MenuJogar (EscolheMapa True x)) _ _ _ _ _)
    | obtemHScore x e == 1000 = Scale 0.3 0.3 $ Translate 2150 1650 $ Color white $ Text ("High Score: Null")
    | otherwise = Scale 0.3 0.3 $ Translate 2150 1650 $ Color white $ Text ("High Score: " ++ hsParaString)
    where hsParaString = show $ (obtemHScore x e)

-- | Função auxiliar de 'desenhaEstadoGloss' - desenha o 'scoreAtual'.
desenhaScoreAtual :: EstadoGloss -> Picture
desenhaScoreAtual e@(EstadoGloss _ (MenuJogar (EscolheMapa True _)) _ _ _ _ _) = Scale 0.3 0.3 $ Translate 2150 1500 $ Color white $ Text ("Score atual: " ++ saParaString)
    where saParaString = show $ (scoreAtual e)

-- Função auxiliar de 'desenhaEstadoGloss' - centra o 'Mapa' nas coordenadas (0,0).
deslocaMapa :: [(Peca, Coordenadas)] -> Picture -> Picture
deslocaMapa m i = Translate x y i
    where x =  (-1.0) * (fromIntegral (div l 2))
          y = (fromIntegral (div a 2))
          l = (larguraMapa m) * 50
          a = (alturaMapa m) * 50

-- | Função principal.
main :: IO ()
main = do
    playMenuPrincipal
    imagens <- carregaImagens
    highScores <- lerFicheiro    
    let jogoInicial = j1
        estadoGlossInicial = (EstadoGloss imagens (MenuPrincipal Jogar) jogoInicial 0 highScores 0 0)
    playIO window 
        (greyN 0.32) 
        fr
        estadoGlossInicial
        desenhaEstadoGloss
        reageEventoGloss
        reageTempoGloss
