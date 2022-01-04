module HighScore where

import System.IO
import qualified System.IO.Strict as SIO

type HighScores = [Int]

lerFicheiro :: IO (HighScores)
lerFicheiro = SIO.run $ lerFicheiro' "data/highScores.txt"

lerFicheiro' :: String -> SIO.SIO (HighScores)
lerFicheiro' fn = do
    handle <- SIO.openFile fn ReadMode
    c <- SIO.hGetContents handle
    let l = map words (lines c)
    SIO.hClose handle
    SIO.return' (geraHScores l)

geraHScores :: [[String]] -> HighScores
geraHScores ([h]:t) = (read h) : (geraHScores t)
geraHScores _ = []

escreverFicheiro :: HighScores -> IO ()
escreverFicheiro hs = SIO.run $ escreverFicheiro' "data/highScores.txt" (geraString hs)

escreverFicheiro' :: String -> String -> SIO.SIO ()
escreverFicheiro' fn s = do
    handle <- SIO.openFile fn WriteMode
    SIO.hPutStr handle s
    SIO.hClose handle

geraString :: HighScores -> String
geraString [] = ""
geraString (h:t) = show h ++ "\n" ++ (geraString t)
