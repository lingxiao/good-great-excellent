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

{-


                $= CT.decode CT.utf8
                $= CT.lines
                $= logi
                $= C.map    (splitOn . pack $ "\t"      )
                $= C.map    (\[_,n] -> read . unpack $ n)
                $$ foldlC (+) 0

        $= C.filter ((==2) . length               )
-}                

{-----------------------------------------------------------------------------
  Subroutines
------------------------------------------------------------------------------}


prepFileWith :: FileOpS m s 
         => CT.Codec
         -> Conduit B.ByteString m QueryResult
prepFileWith c = CT.decode c 
              $= CT.lines
              $= C.map    (splitOn . pack $ "\n")
              $= C.map    head
              $= C.map    (splitOn $ pack "\t")
              $= C.filter (\x -> length x == 3)
              $= C.map    (\[t,n] -> ( t
                                       , read . unpack $ n
                                      ))


queryFile :: FileOpS m [QueryResult]
          => Parser Text
          -> Consumer QueryResult m Integer
queryFile p = C.filter     (\(t,_)  -> p <**? t)
           $= awaitForever (\t@(_,n) -> do
                       ts <- lift get
                       let ts' = t:ts
                       lift . put $ ts'
                       yield n
            )     
            =$= foldlC (+) 0








