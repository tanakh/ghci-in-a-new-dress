-- | GHCi related functions
module NewGHCi where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar, threadDelay)
import qualified Data.ByteString.Char8 as B
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf)
import System.IO (Handle, hGetLine, hPutStr, stdout)
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
ghciPath = "ghci"

ghciArgs :: [String]
ghciArgs = ["-XSafe", "-fpackage-trust", {-"-distrust-all-packages",-} "-trust base"]

sentinel :: String
sentinel = "1234567890"

queryGHCI :: String -> IO String
queryGHCI input | last input /= '\n'           = queryGHCI $ input ++ "\n"
queryGHCI input | ":doc" `isPrefixOf` input    = queryHaddock input
queryGHCI input | ":hoogle" `isPrefixOf` input = queryHoogle input
queryGHCI input = do
    -- Lock this function. Only 1 person can query ghci at a time.
    _ <- takeMVar lockGHCI

    -- Get Hlint suggests only if it's Haskell code and not an interpretor
    -- command If "No suggestions", then don't send it in down and if it already
    -- ends with '\n', don't do anything
    hlint <- if ":" `isPrefixOf` input
              then return ""
              else hlintCheck input
    let hlintSugg = if ":" `isPrefixOf` input
                      then hlint
                      else hlint ++ "\n"
    
    hin  <- readIORef hInGHCI
    hout <- readIORef hOutGHCI
    herr <- readIORef hErrGHCI

    hPutStr hin input
    
    errors <- do
        hPutStr hin "oopsthisisnotavariable\n"
        err <- getErrors herr
        return err
  
    -- This is a hack that lets us discover where the end of the output is.
    -- We will keep reading until we see the sentinel.
    hPutStr hin (":t " ++ sentinel ++ "\n")

    output <- readUntilDone hout

    putMVar lockGHCI True

    if trimWhitespace errors == "" 
        then return $ hlintSugg ++ output
        else return $ "ERR: " ++ show (parseErrors errors)

getErrors :: Handle -> IO String
getErrors herr' = 
    go herr' ""
  where
    go herr results = do
      line <- hGetLine herr
      putStrLn $ "Error: " ++ line

      if "oopsthisisnotavariable" `isInfixOf` line
        then return(results)
        else go herr (results ++ "\n" ++ line)

readUntilDone :: Handle -> IO String
readUntilDone hout = go []
  where
    go acc = do
      l <- hGetLine hout
      putStrLn $ "Input: " ++ l
      if sentinel `isInfixOf` l
        then return $ done acc
        else go (l:acc)
    
    done [] = "\n"
    done xs = unlines $ reverse xs

skipIntro :: Handle -> IO ()
skipIntro h = do
  threadDelay $ 2 * 10 ^ (6 :: Int)
  B.hPutStrLn stdout =<< B.hGetNonBlocking h (2^(31::Int))
