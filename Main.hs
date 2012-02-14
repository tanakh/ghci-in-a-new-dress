import Yesod
import Yesod.Static

import System.Directory
import Data.Maybe
import System.Process
import System.IO
import Control.Monad
import Data.IORef


import GHCiYesod
import NewGHCi

main :: IO ()
main = do
    clearFile tempFileName
    clearFile tempDataDefs

    (Just hin, Just hout, Just herr, _) <-
      createProcess (proc ghciPath []) {
              std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe
          }

    hSetBuffering hin NoBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering

    hPutStr hin ":t 1\n"
    readIntro hout

    writeIORef hInGHCI hin
    writeIORef hOutGHCI hout
    writeIORef hErrGHCI herr

    s <- staticDevel "static"

    warpDebug 3000 $ GHCiOnline s

  where
    clearFile f = do
        exist <- doesFileExist f
        when exist $ removeFile f

