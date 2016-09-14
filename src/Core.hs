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

  ) where 


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
      dataRoot   :: DirectoryPath
    , strongWeak :: FilePath
    , weakStrong :: FilePath
} deriving (Show, Eq)