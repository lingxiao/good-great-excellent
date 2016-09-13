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
  , Output
  , QueryResult
  , Config (..)

  ) where 

import System.Directory
import System.FilePath.Posix
import qualified System.IO as S

import Data.Time.Clock
import Data.Text (Text, unpack)
import Data.List.Split (splitOn)
import Data.Attoparsec.Text 


{-----------------------------------------------------------------------------
  Output of query ngrams
------------------------------------------------------------------------------}

type DirectoryPath = FilePath

-- * Results from a single `Query` to ngram documents
-- * the first field is preprocess text, second field is orginal text
-- * the third field is number of results
type QueryResult   = (Text,Text,Integer)
type Output        = (Integer, [QueryResult])


{-----------------------------------------------------------------------------
    Filepath configuration
------------------------------------------------------------------------------}

data Config = Con {
      onegram    :: FilePath
    , ngrams     :: [FilePath]
    , strongWeak :: FilePath
    , weakStrong :: FilePath
} deriving (Show, Eq)