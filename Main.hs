module Main where

import Control.Concurrent
import Data.IORef
import Data.Maybe
import System.IO
import System.Process

import Yesod
import Yesod.Static

import GHCiYesod
import NewGHCi

main :: IO ()
main = do
    putStrLn $ "opening: " ++ (unwords $ ghciPath : ghciArgs)
    
    (Just hin, Just hout, Just herr, ph) <-
      createProcess (shell $ unwords $ ghciPath : ghciArgs) {
              std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe
          }

    hSetBuffering hin NoBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering

    skipIntro hout

    writeIORef hInGHCI hin
    writeIORef hOutGHCI hout
    writeIORef hErrGHCI herr
    
    s <- staticDevel "static"

    warpDebug 3000 $ GHCiOnline s
