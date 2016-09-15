{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Shared datatypes for this application
-- | Author  : Xiao Ling
-- | Date    : 9/8/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Core (

    DirectoryPath
  , Input
  , Output
  , QueryResult

  , Config
  , config
  , corpus
  , weakStrong
  , strongWeak

  ) where 

import System.FilePath.Posix
import System.Directory

import Data.Text (Text, unpack)

{-----------------------------------------------------------------------------
  Output of query ngrams
------------------------------------------------------------------------------}

type DirectoryPath = FilePath

-- * Results from a single `Query` to ngram documents
-- * the first field is preprocess text, second field is orginal text
-- * the third field is number of results
type Input         = (Text, Text, Text)
type Output        = (Integer, [QueryResult])
type QueryResult   = (Text,Text,Integer,Text)

{-----------------------------------------------------------------------------
    Filepath configuration
------------------------------------------------------------------------------}


data Config = Con {
      corpus         :: DirectoryPath
    , weakStrong     :: [String]
    , strongWeak     :: [String]
} deriving (Show, Eq)


-- * @USE : config "path/to/data.txt" "path/to/patterns"
-- * First line in main is to ensure the filestructure is appropriately set up
-- * and the necessary patterns and data are present
-- * Construct a legal System config by ensuring that all filepaths are valid
-- * read in weakstrong and strongweak patterns from disk if exists
config :: DirectoryPath -> DirectoryPath -> IO (Maybe Config)
config corpusD patternD = do
  b1 <- doesDirectoryExist corpusD
  b2 <- doesDirectoryExist patternD
  if b1 && b2 then do
    let ws = patternD ++ "weak-strong-patterns.txt"
    let sw = patternD ++ "strong-weak-patterns.txt"
    ws' <- lines <$> readFile ws
    sw' <- lines <$> readFile sw
    return . Just $ Con corpusD ws' sw'
  else 
    return Nothing































