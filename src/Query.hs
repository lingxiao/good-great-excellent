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
  , raw_freq
  ) where


import System.FilePath.Posix
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
import ScrubData


{-----------------------------------------------------------------------------
  Query all files in directory
------------------------------------------------------------------------------}

query :: Op m => Parser Text -> DirectoryPath -> m Output
query p f =  eval 
          $  [f] `sourceDirectories` ".txt" 
          $= openFile 
          $= toInput CT.utf8
          $$ queryFile p

{-----------------------------------------------------------------------------
  Query one file
------------------------------------------------------------------------------}

-- * query preprocessed text file
query_at :: Op m => Parser Text -> FilePath -> m Output
query_at p f =  eval
             $  sourceFile f 
             $= toInput CT.utf8
             $$ queryFile p

{-----------------------------------------------------------------------------
  Sum all frequencies in a file
------------------------------------------------------------------------------}

-- * count total frequency, compatible with file that `conform_pattern`
-- * and file that `not_conform_pattern`
total_freq :: Op m => FilePath -> m Integer
total_freq inp =   run 
               $   sourceFile inp
               $=  toInput CT.utf8
               $=  C.map (\(_,n) -> read . unpack $ n)
               $$  foldlC (+) 0


-- * count total frequency in greped file
raw_freq :: Op m => FilePath -> m Integer
raw_freq inp =  run
             $  sourceFile inp   
             $= toInput CT.utf8
             $= C.map (\(_,n) -> read . unpack $ n)
             $$ foldlC (+) 0


-- * Subroutines * --

queryFile :: FileOpS m [Input]
          => Parser Text
          -> Consumer Input m Integer
queryFile p = C.filter     (\(t,_)  -> p <**? t)
           $= awaitForever (\t@(_,n) -> do
                       ts <- lift get
                       let ts' = t:ts
                       lift . put $ ts'
                       yield . read . unpack $ n
            )     
            =$= foldlC (+) 0





