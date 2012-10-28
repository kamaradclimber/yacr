module Yacr.Core where

import Yacr.Parsers
import Yacr.Types

import Data.List
import Data.Maybe
import Data.Time.Clock
import Network.URI
import System.IO
import Text.Sundown.Html.String
import Text.ParserCombinators.Parsec hiding (getInput)
import Text.RSS

hParseConvert :: Args -> Handle -> Handle -> IO()
hParseConvert a i o = do
    content <- hGetContents i 
    hPutStrLn o $ parseConvert a content

parseConvert :: Args -> String -> String
parseConvert a content =
    case parse changelogP "changelog" content of
        Left err -> show err
        Right c  -> conv a c

changelogEntry2itemElem :: Args -> ChangelogEntry -> Item
changelogEntry2itemElem a c = [
      Title  $ "Release " ++ show ( release $ metadata c)
    , Author $ email $ metadata c
    , PubDate UTCTime { 
          utctDay  = date $ metadata c
        , utctDayTime = secondsToDiffTime 49020
    }
    , Description $  markdown $ intercalate "\n\n" (entries c)
    , Link $ getUri a
    ]

markdown :: String -> String
markdown s = renderHtml s allExtensions noHtmlModes True Nothing 


changelog2rss :: Args -> Changelog -> RSS
changelog2rss a ch = RSS  (title a) 
    (getUri a)
    "here is the changelog of ...."
    [Generator "yacr"]
    (map (changelogEntry2itemElem a) ch)
    
 
conv :: Args -> Changelog -> String
conv a = showXML . rssToXML . changelog2rss a


getUri :: Args -> URI
getUri a = fromMaybe nullURI (parseURI $ url a)
