module Audio where

import System.Process

playMenuPrincipal :: IO (ProcessHandle)
playMenuPrincipal = spawnCommand "mpv --volume=60 --no-video --loop audio/mpAudio.mp3"

killProcess :: IO (ProcessHandle)
killProcess = spawnCommand "killall mpv"
