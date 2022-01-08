{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module SaveGame where

import LI12122
import Tarefa3_2021li1g033
import Niveis

import GHC.Generics
import Control.DeepSeq
import System.IO
import qualified System.IO.Strict as SIO

data JogoGuardado = 
    JogoGuardado
        Mapa
        Jogador
        Int
    deriving (Show, Read, Generic, NFData)

lerSGame :: IO (JogoGuardado)
lerSGame = SIO.run $ lerSGame' "data/saveGame.txt"

lerSGame' :: String -> SIO.SIO (JogoGuardado)
lerSGame' fn = do
    handle <- SIO.openFile fn ReadMode
    c <- SIO.hGetContents handle
    SIO.hClose handle
    SIO.return' ((read c) :: JogoGuardado)

escreverSGame :: JogoGuardado -> IO ()
escreverSGame j = SIO.run $ escreverSGame' "data/saveGame.txt" (show j)

escreverSGame' :: String -> String -> SIO.SIO ()
escreverSGame' fn s = do
    handle <- SIO.openFile fn WriteMode
    SIO.hPutStr handle s
    SIO.hClose handle

guardadoParaJogo :: JogoGuardado -> Jogo
guardadoParaJogo (JogoGuardado m j x) = (Jogo m j)

jogoParaGuardado :: Int -> Jogo -> JogoGuardado
jogoParaGuardado x (Jogo m j) = (JogoGuardado m j x)
