module Audio where

import System.Process

playMenuPrincipal :: IO (ProcessHandle)
playMenuPrincipal = spawnCommand "mpv --volume=70 --no-video --loop audio/mpAudio.mp3"

playMenuChange :: IO (ProcessHandle)
playMenuChange = spawnCommand "mpv --volume=100 --no-video audio/menuChange.mp3"

killProcess :: IO (ProcessHandle)
killProcess = spawnCommand "killall mpv"
