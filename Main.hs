module Main where

import Control.Concurrent
import System.IO
import System.Process

import Yesod
import Yesod.Static

import GHCiYesod

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
    
    mv <- newMVar ()

    s <- staticDevel "static"
    warpDebug 3000 $ GHCiOnline s (hin, hout, herr, ph) mv
