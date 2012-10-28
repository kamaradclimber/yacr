module Main where

import Yacr.Core 
import Yacr.Types
import System.IO
import System.Console.CmdArgs

cliOptions :: Args
cliOptions = Args {
    input  = def &= help "input file, stdin if omitted" &= typFile,
    output = def &= help "output file, stdout if omitted" &= typFile,
    title  = "Test changelog"  &= help "title of the feed" &= typ "TITLE",
    url    = "http://www.kernel.org" &= help "url of the feed" &= typ "URL"
} &= summary "Convert changelog into rss"
  &= helpArg [explicit, name "help", name "h"]
  &= program "yacr"

main :: IO ()
main = realMain =<< cmdArgs cliOptions

realMain :: Args -> IO ()
realMain a = do
    inp <- getInput a
    out <- getOutput a
    hParseConvert a inp out
    hClose inp
    hClose out

getInput :: Args -> IO Handle
getInput a = 
    case input a of
        [] -> return stdin
        p  -> openFile p ReadMode 

getOutput :: Args -> IO Handle
getOutput a = 
    case output a of
        [] -> return stdout
        p  -> openFile p WriteMode 

