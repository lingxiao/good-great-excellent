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
  , query_at
  , total_freq
  ) where


import Control.Monad.State

import Data.Conduit 
import Conduit              (foldlC)
import Data.Attoparsec.Text hiding (count)
import Data.Text            (Text, unpack, pack, splitOn)
import Data.Conduit.Binary  (sourceFile, sinkFile)
import qualified Data.ByteString as B
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Combinators as C


import Lib
import Core


{-----------------------------------------------------------------------------
  Query all files in directory
------------------------------------------------------------------------------}

query :: Op m => Parser Text -> DirectoryPath -> m Output
query p f =  eval 
          $  [f] `sourceDirectories` ".txt" 
          $= openFile 
          $= prepFileWith CT.utf8
          $$ queryFile p

{-----------------------------------------------------------------------------
  Query one file
------------------------------------------------------------------------------}

-- * query preprocessed text file
query_at :: Op m => Parser Text -> FilePath -> m Output
query_at p f =  eval
             $  sourceFile f 
             $= prepFileWith CT.utf8
             $$ queryFile p

{-----------------------------------------------------------------------------
  Sum all frequencies in a file
------------------------------------------------------------------------------}

total_freq :: Op m => FilePath -> m Integer
total_freq inp =   run 
               $   sourceFile inp
               $=  prepFileWith CT.iso8859_1
               $=  awaitForever (\(_,_,n,_) -> yield n)
               $$  foldlC (+) 0

{-----------------------------------------------------------------------------
  Conduits
------------------------------------------------------------------------------}

-- linesOn "\n"
prepFileWith :: FileOpS m s 
         => CT.Codec
         -> Conduit B.ByteString m QueryResult
prepFileWith c = CT.decode c 
              $= CT.lines
              $= C.map    (splitOn . pack $ "\n")
              $= C.map    head
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








