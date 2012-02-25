{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeFamilies, QuasiQuotes #-}
module GHCiYesod where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar, threadDelay)
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.List
import qualified Data.Text as T
import System.IO
import System.IO.Unsafe
import System.Process

import Yesod
import Yesod.Static

import Tools
import Utils

staticFiles "static"

mkYesod "GHCiOnline" [parseRoutes|
/       HomeR   GET
/ghci   GHCIR   POST
/static StaticR Static helloWorldStatic
|]

data GHCiOnline
  = GHCiOnline
    { helloWorldStatic :: Static
    , ghci :: (Handle, Handle, Handle, ProcessHandle)
    }

instance Yesod GHCiOnline where
    approot = ApprootRelative

postGHCIR :: Handler RepHtml
postGHCIR = do
  -- This is how you get post data. 
  -- type of postTuples is [(Data.Text, Data.Text)] - key value pairs
  (postTuples, _) <- runRequestBody
  let content = unescape $ T.unpack (snd $ postTuples !! 0)
  result <- queryGHCI content
  defaultLayout [whamlet|#{result}|]

getHomeR :: Handler RepHtml
getHomeR = do
  -- _ <- liftIO $ queryGHCI ":t 5.0\n"
  defaultLayout [whamlet|
<html>
  <head>
    <title> GHCi Online (Powered by Safe Haskell)
    <link rel="stylesheet" type="text/css" href="static/style.css">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js">
    <script src="static/main.js">
  <body>
    <ul id="autocomplete"> 
    <div id="typeannotations">
    <div id="calltips">
    <div id="program">
      <div id="console">
        <div class="input" id="active">
          <span id="prompt">
            \$ 
          <span id="content">
          <span id="cursor">_
        <div id="sidebar">
          <h3>Inspector
          <ul id="sidelist">
|]

lockGHCI :: MVar Bool
{-# NOINLINE lockGHCI #-}
lockGHCI = unsafePerformIO (newMVar True)

ghciPath :: FilePath
ghciPath = "ghci"

ghciArgs :: [String]
ghciArgs = ["-XSafe", "-fpackage-trust", {-"-distrust-all-packages",-} "-trust base"]

sentinel :: String
sentinel = "1234567890"

queryGHCI :: String -> Handler String
queryGHCI input | last input /= '\n'           = queryGHCI $ input ++ "\n"
queryGHCI input | ":doc" `isPrefixOf` input    = liftIO $ queryHaddock input
queryGHCI input | ":hoogle" `isPrefixOf` input = liftIO $ queryHoogle input
queryGHCI input = do
    -- Lock this function. Only 1 person can query ghci at a time.
    _ <- liftIO $ takeMVar lockGHCI

    -- Get Hlint suggests only if it's Haskell code and not an interpretor
    -- command If "No suggestions", then don't send it in down and if it already
    -- ends with '\n', don't do anything
    hlint <- if ":" `isPrefixOf` input
              then return ""
              else liftIO $ hlintCheck input
    let hlintSugg = if ":" `isPrefixOf` input
                      then hlint
                      else hlint ++ "\n"
    
    GHCiOnline { ghci = (hin, hout, herr, _) } <- getYesod
    
    liftIO $ hPutStr hin input
    
    errors <- liftIO $ do
        hPutStr hin "oopsthisisnotavariable\n"
        err <- getErrors herr
        return err
  
    -- This is a hack that lets us discover where the end of the output is.
    -- We will keep reading until we see the sentinel.
    liftIO $ hPutStr hin (":t " ++ sentinel ++ "\n")

    output <- liftIO $ readUntilDone hout

    liftIO $ putMVar lockGHCI True

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

