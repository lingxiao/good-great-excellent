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
  ) where

import Control.Monad.Trans
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
  Query using list streaming solution build from Data.Conduit
------------------------------------------------------------------------------}


query :: Op m => FilePath -> Parser Text -> m Output
query f p = eval $ openFile' f $$ queryFile p


openFile' :: FileOpS m s => FilePath -> Source m QueryResult
openFile' f =   sourceFileE f
             $= linesOn "\n"
             $= C.map head
             $= C.map (splitOn $ pack "\t")
             $= C.map (\[a,b,c] -> ( preprocess a
                                   , a
                                   , read . unpack $ b :: Integer
                                   , c))


queryFile :: FileOpS m [QueryResult]
          => Parser Text
          -> Consumer QueryResult m Integer
queryFile p = C.filter (\(t,_,_,_) -> p <**? t)
             $= awaitForever (\t@(_,_,n,_) -> do
                        ts <- lift get
                        let ts' = t:ts
                        lift . put $ ts'
                        yield n
                )     
             =$= foldlC (+) 0








