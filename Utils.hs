-- | Utility functions
module Utils where

import Control.Monad (liftM)
import Data.List (isPrefixOf, isInfixOf)

isSpace :: Char -> Bool
isSpace c = (c == ' ') || (c == '\n')

trimWhitespace :: String -> String
trimWhitespace = f . f
   where f = reverse . dropWhile isSpace

unescape :: String -> String
unescape str' = go str' ""
  where
    go :: String -> String -> String
    go "" result = result
    go str result | "\\\\" `isPrefixOf` str = go (drop 2 str) (result ++ "\\")
    go str result | "&lt;" `isPrefixOf` str = go (drop 4 str) (result ++ "<")
    go str result | "&gt;" `isPrefixOf` str = go (drop 4 str) (result ++ ">")
    go str result | otherwise = go (drop 1 str) (result ++ [head str])

lineIsFunctionDefinition :: String -> Bool
lineIsFunctionDefinition line =
  ("=" `isInfixOf` line) && (not (" " `isPrefixOf` line)) && (not ("data" `isPrefixOf` line))

getFunctionName :: String -> String
getFunctionName line =
  (words line) !! 0

stripLet :: String -> String
stripLet str =
  if "let" `isPrefixOf` str
    then drop 4 str
    else str

-- Strip double definitions out of a Haskell file, retaining
-- only the last one.
stripDoubleDefs :: String -> IO [String]
stripDoubleDefs file = do
    contents <- liftM lines $ readFile file
    let result = go contents [""] [""]

    return (result)
  where
    go :: [String] -> [String] -> [String] -> [String]
    -- Build valid Haskell program, putting data first, and
    -- all commands inside of a do block.
    go [] result dataLines = dataLines ++ result

    -- Gather all inputted lines.
    go (l:ls) result dataLines =
      if lineIsFunctionDefinition l
        then
          let _name = getFunctionName l
          in if (any (==l) (map getFunctionName ls))
              then go ls result dataLines
              else go ls (result ++ [stripLet $ l]) dataLines
        else
          if "data" `isPrefixOf` l
            then go ls result (dataLines ++ [l])
            else go ls (result) dataLines

-- Take the interactive output and make it a little more JavaScript friendly.
-- Return a tuple of (position, position, details)
parseErrors :: String -> [String]
parseErrors str' = go str' "" "" 0
  where
    -- <interactive>:1:1:
    go :: String -> String -> String -> Int -> [String]
    go rest first second 3 = [first, second, rest]

    go (s:str) first second seen =
      if s == ':'
        then go str first second (seen + 1)
        else
          case seen of
            1 -> go str (first ++ [s]) second seen
            2 -> go str first (second ++ [s]) seen
            _ -> go str first second seen

    go [] first second _ = [first, second]

