module Yacr.Parsers where
import Data.List
import Data.Version
import Data.Time.Calendar
import Data.Time.Format
import System.Locale
import Yacr.Types

import Text.ParserCombinators.Parsec

year :: Parser String
year = count 4 digit

twoDigits :: Parser String
twoDigits = count 2 digit

hyphen :: Parser Char
hyphen = oneOf "-"

dot :: Parser Char
dot = char '.'

dateP :: Parser Day
dateP = do
    y <- year
    _ <- hyphen
    m <- twoDigits
    _ <- hyphen
    d <- twoDigits
    return $ readTime defaultTimeLocale "%F" (y ++ "-" ++ m ++ "-" ++ d)

releaseP :: Parser Semver
releaseP = do
    vs <- many1 digit `sepBy1` dot
    pre <- option [] releaseSuffix
    let v = map read vs in
      return $ Semver $ Version v pre

releaseSuffix :: Parser [String]
releaseSuffix = do
    _ <- char '-'
    many1 alphaNum `sepBy1` dot

firstLine :: Parser ChangelogM
firstLine = do
    d <- dateP
    spaces
    _ <- string "Release"
    spaces
    v <- releaseP
    spaces
    owner <- manyTill anyChar (char '<')
    e <- emailP
    _ <- char '>'
    _ <- newline
    return $ ChangelogM d  owner e v

indentedLine :: Parser String
indentedLine = do
    skipMany1 space
    c <- many1 (noneOf "\n")
    skipMany newline <|> skipMany1 eof
    return c

intermediate :: Parser ChangelogEntry
intermediate = do
    ch <- firstLine
    _ <- many1 $ char '-'
    _ <- newline
    a  <- many1 indentedLine
    return $ ChangelogEntry ch a

changelogP :: Parser Changelog
changelogP =  many intermediate

emailP :: Parser String
emailP = do
    n <- dottedWords
    _ <- char '@'
    d <- dottedWords
    return $ n ++ "@" ++ d

dottedWords :: Parser String
dottedWords = do
    d <- many1 alphaNum `sepBy1` dot
    return $ intercalate "." d
