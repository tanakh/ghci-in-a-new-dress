-- | External helpers
module Tools where

import Yesod

import qualified Data.Text as T
import Data.Maybe
import System.Process
import System.IO
import Data.List
import Control.Monad

import Text.XmlHtml
import Data.ByteString.Char8 (pack)

data Func = Func {
        name :: String,
        type_ :: String,
        doc  :: String
    } deriving (Show)

test :: IO [Func]
test = do
        html <- readFile "test.html"
        return (parseHaddock html)

parseHaddock :: String -> [Func]
parseHaddock d =
    let eitherDoc     = (parseHTML "html" $ pack d)
        parseEl     e = (Func (extractName e) (extractType e) (extractDoc  e))
        extractName e = T.unpack $ nodeText $ head $ childElements $ head $ childElements e
        extractType e = concat $ map T.unpack $ map nodeText $ init $ drop 1 $ childNodes $ head $ childElements e
        extractDoc  e = convertNodeToMarkup $ (!!) (childElements e) 1

    in case eitherDoc of 
            (Right v) -> map parseEl $ parseTree (head (docContent v))
            (Left  _) -> []

  where
    parseTree n =
        let nodeTag = tagName n
            cls = getAttribute (T.pack "class") n
        in if not ((isNothing nodeTag) || (isNothing cls) || (not ((fromJust cls) == (T.pack "top"))))
               then ([n] ++ concat (map parseTree $ childNodes n))
               else concat (map parseTree $ childNodes n)

    convertNodeToMarkup n
        | isElement n = let tagName_ = T.unpack $ fromJust $ tagName n
                            -- attrs = map (\x -> ((T.unpack $ fst x) ++ "=" ++ (T.unpack $ snd x))) (elementAttrs n)
                            -- attrs
                        in "<" ++ tagName_ ++ " " ++ ">"  ++ (concat $ map convertNodeToMarkup $ childNodes n) ++ "</" ++ tagName_ ++ ">"
        | isTextNode n = T.unpack $ nodeText n

        | otherwise = error "bad node!"

queryHaddock :: String -> IO String
queryHaddock keyword = do
    -- TODO: Fix newline issue and handle Hoogle errors
    let keywords =
            case (stripPrefix ":doc" keyword) of
                (Just x) -> takeWhile (\y -> not (y == '\n')) $ dropWhile (\y -> y == ' ') x
                Nothing  -> "list"
    liftIO $ print "Haddock"
    docVal <- getDocForFn keywords
    liftIO $ print docVal
    return docVal

  where 
    getDocForFn n = do
      html <- readFile "test.html"
      liftIO $ print n
      let fns   = parseHaddock html
          fnDoc = filter (\x -> name x == n) fns
          val   = if length fnDoc > 0
                    then "DOC" ++ (doc $ head fnDoc)
                    else "DOC<div class=\"error\">Nothing was found</div>"
      return val

queryHoogle :: String -> IO String
queryHoogle keyword = do
    -- TODO: Fix newline issue and handle Hoogle errors
    let keywords = case (stripPrefix ":hoogle" keyword) of
                        (Just x) -> dropWhile (\y -> y == ' ') x
                        Nothing -> "list"
    liftIO $ print "Hoogle"
    liftIO $ print keyword
    (Just _, Just hout, _, _) <- createProcess (proc "hoogle" ["--count=20", keywords]) { std_out = CreatePipe, std_in = CreatePipe }
    output <- hGetContents hout
    liftIO $ print output
    return output

hlintCheck :: String -> IO String
hlintCheck code = do
    -- `code` already ends with a newline character
    writeFile "/tmp/temp.hs" ( "main = do" ++ "\n\t" ++ code ++ "\t" ++ "return ()" )
    ( Just _, Just hout, _ , _ ) <- createProcess (proc "hlint" ["/tmp/temp.hs"]) { std_out = CreatePipe, std_in = CreatePipe }
    output <- hGetContents hout
    return output

