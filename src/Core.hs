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
  , Config (..)
  , config

  ) where 

import System.FilePath.Posix
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
      inputs     :: [FilePath]
    , weakStrong :: FilePath
    , strongWeak :: FilePath
} deriving (Show, Eq)


-- * @USE: config "path/to/project-name" ["path/to/data.txt"] 
-- *       given path to project and path to data,
-- *       output Config 
-- * Note: this makes strong assumptions on the directory structure of the project
config :: FilePath -> [FilePath] -> Config
config p fs = Con fs 
                 (p ++ "/patterns/weak-strong-patterns.txt") 
                 (p ++ "/patterns/strong-weak-patterns.txt")