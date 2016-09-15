{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Query files for certain patterns
-- | Author  : Xiao Ling
-- | Date    : 9/13/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Query (
    query
  , queryIn
  ) where


import Control.Monad.State

import Data.Conduit 
import Conduit              (foldlC)
import Data.Attoparsec.Text hiding (count)
import Data.Text            (Text, unpack, pack, splitOn)
import qualified Data.ByteString as B
import qualified Data.Conduit.Combinators as C

import Lib
import Core

{-----------------------------------------------------------------------------
  Query all files in directory
------------------------------------------------------------------------------}

query :: Op m => Parser Text -> DirectoryPath -> m Output
query p f = eval 
              $ [f] `sourceDirectories` ".txt" 
              $= openFile 
              $= prepFile 
              $$ queryFile p

{-----------------------------------------------------------------------------
  Query one file
------------------------------------------------------------------------------}

-- * query preprocessed text file
queryIn :: Op m => Parser Text -> FilePath -> m Output
queryIn p f = eval $ sourceFileE f $= prepFile $$ queryFile p


prepFile :: FileOpS m s => Conduit B.ByteString m QueryResult
prepFile =  linesOn "\n"
             $= C.map head
             $= C.map    (splitOn $ pack "\t")
             $= C.filter (\x -> length x == 3)
             $= C.map    (\[a,b,c] -> ( preprocess a
                                      , a
                                      , read . unpack $ b
                                      , c))

queryFile :: FileOpS m [QueryResult]
          => Parser Text
          -> Consumer QueryResult m Integer
queryFile p = C.filter     (\(t,_,_,_)  -> p <**? t)
           $= awaitForever (\t@(_,_,n,_) -> do
                       ts <- lift get
                       let ts' = t:ts
                       lift . put $ ts'
                       yield n
            )     
            =$= foldlC (+) 0








