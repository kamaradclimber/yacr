{-# LANGUAGE DeriveDataTypeable #-}
module Yacr.Types where

import Data.List
import Data.Time.Calendar
import Data.Version
import System.Console.CmdArgs

data ChangelogM = ChangelogM {
      date :: Day
    , author :: String
    , email :: String
    , release :: Semver
  } deriving (Eq, Show)

data ChangelogEntry = ChangelogEntry {
      metadata :: ChangelogM
    , entries :: [String]
  } deriving (Eq, Show)

type Changelog = [ChangelogEntry]

newtype Semver = Semver Version
    deriving (Eq, Ord)

instance Show Semver where
    show (Semver v) = 
        --intercalate "." (map show $ versionBranch v) ++ intercalate "."  (versionTags v)
        concatMap (intercalate ".")  [map show $ versionBranch v,  versionTags v]
        {-concatMap (intercalate ".")  $ [map show . versionBranch,  versionTags ] << v where
           (<<) fs x = map (x ><) fs
           (><) a b = b a
        -}

data Args = Args {
      input  :: String
    , output :: String
    , title  :: String
    , url    :: String
    } deriving (Show, Data, Typeable)

