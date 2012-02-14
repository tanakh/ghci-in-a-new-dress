-- | GHCi related functions
module NewGHCi where

import Yesod

import Control.Concurrent
import System.IO
import Data.List
import Control.Monad
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Tools
import Utils

{- This is Bad, Dirty, Evil, Not Good, etc. Fix if time.  The
 - difficulty is that we need a way to share variables between
 - main and the handlers. I can't figure out a better way to do
 - that. 
 -
 - Reading material: http://www.haskell.org/haskellwiki/Top_level_mutable_state
 -}

hInGHCI :: IORef Handle
{-# NOINLINE hInGHCI #-}
hInGHCI = unsafePerformIO (newIORef undefined)

hOutGHCI :: IORef Handle
{-# NOINLINE hOutGHCI #-}
hOutGHCI = unsafePerformIO (newIORef undefined)

hErrGHCI :: IORef Handle
{-# NOINLINE hErrGHCI #-}
hErrGHCI = unsafePerformIO (newIORef undefined)

lockGHCI :: MVar Bool
{-# NOINLINE lockGHCI #-}
lockGHCI = unsafePerformIO (newMVar True)

ghciPath :: FilePath
ghciPath = "/home/davidt/Software/ghc-ghci/bin/ghci"

tempFileName :: String
tempFileName = "commandsEntered.hs"

tempDataDefs :: String
tempDataDefs = "dataDefs.hs"

sentinel :: String
sentinel = "1234567890"

queryGHCI :: String -> IO String
queryGHCI input | last input /= '\n' = queryGHCI $ input ++ "\n" -- Append a newline character to the end of input
queryGHCI input = 
  
  -- TODO: Prevent this from running 
  -- Handle Hoogle queries
  if ":doc" `isPrefixOf` input then queryHaddock input else
  if ":hoogle" `isPrefixOf` input then queryHoogle input else do
  
  -- Lock this function. Only 1 person can query
  -- ghci at a time.
  
  -- Get Hlint suggests only if it's Haskell code and not an interpretor command
  hlint <- if ":" `isPrefixOf` input then (return "") else ((hlintCheck input) )
  let hlintSugg = if ":" `isPrefixOf` input then hlint else (hlint ++ "\n")

  -- If "No suggestions", then don't send it in down and if it already ends with '\n', don't do anything
  
  _ <- takeMVar lockGHCI
  hin <- readIORef hInGHCI
  hout <- readIORef hOutGHCI
  herr <- readIORef hErrGHCI

  errors <- if "data " `isPrefixOf` input
              then do
                err <- handleDataInput input hin hout herr
                liftIO $ print "DONE"
                return err
              else do
                hPutStr hin input
                if ":" `isPrefixOf` input
                  then return ""
                  else do
                    hPutStr hin "oopsthisisnotavariable\n"
                    appendFile tempFileName input
                    err <- getErrors herr
                    return err

  
  -- This is a hack that lets us discover where the end of the output is.
  -- We will keep reading until we see the sentinel.
  hPutStr hin (":t " ++ sentinel ++ "\n")

  output <- readUntilDone hout
  putMVar lockGHCI True

  if trimWhitespace(errors) == "" 
    then return (hlintSugg ++ output)
    else return ("ERR: " ++ (show $ parseErrors $ errors))

-- Handles text typed in the REPL that does data constructor declarations (data Color = Black | White)
handleDataInput :: String -> Handle -> Handle -> Handle -> IO String
handleDataInput input hin hout herr = do
  appendFile tempDataDefs ""
  appendFile tempFileName ""

  oldDataDefs <- readFile tempDataDefs
  oldInput <- readFile tempFileName

  liftIO $ print oldDataDefs
  liftIO $ print oldInput

  liftIO $ print "appending"
  liftIO $ print input

  appendFile tempDataDefs input

  hPutStr hin (":load " ++ tempDataDefs ++ "\n")
  _ <- mapM (hPutStr hin) (map (++"\n") (lines oldInput))

  hPutStr hin (":t " ++ sentinel ++ "\n")
  hPutStr hin "oopsthisisnotavariable\n"

  _ <- readUntilDone hout
  errLine <- hGetLine herr

  if "oopsthisisnotavariable" `isInfixOf` errLine
    then return ""
    else do
      liftIO $ print errLine
      -- Error with that data definition. Forget about it.
      errors <- getErrors herr
      liftIO $ print "ERrors received:"
      liftIO $ print errors
      if (trimWhitespace errors) == ""
        then return ""
        else do
          writeFile tempDataDefs oldDataDefs
          --load from known good file
          hPutStr hin (":load " ++ tempDataDefs ++ "\n")
          _ <- mapM (hPutStr hin) (map (++"\n") (lines oldInput))
          hPutStr hin (":t " ++ sentinel ++ "\n")
          _ <- readUntilDone hout
          return errors

getErrors :: Handle -> IO String
getErrors herr' = 
    go herr' ""
  where
    go herr results = do
      line <- hGetLine herr

      if "oopsthisisnotavariable" `isInfixOf` line
        then return(results)
        else go herr (results ++ "\n" ++ line)

readIntro :: Handle -> IO ()
readIntro hout = do
  line <- hGetLine hout
  if "Prelude" `isInfixOf` line
    then return ()
    else readIntro hout

readUntilDone :: Handle -> IO String
readUntilDone hout = do
    line <- hGetLine hout 
    if sentinel `isInfixOf` line
      then return "\n"
      else go (line ++ "\n")
  where
    go resultSoFar = do
      line <- hGetLine hout

      if sentinel `isInfixOf` line
        then return (resultSoFar)
        else go (resultSoFar ++ line ++ "\n")

