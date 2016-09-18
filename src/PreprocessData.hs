{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A collection of scripts to preprocess data
-- | Author  : Xiao Ling
-- | Date    : 9/13/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 
module PreprocessData (

    filterByPattern
  , filterByPattern'

  ) where


import System.Directory
import System.FilePath.Posix

import Data.Text.Encoding
import Data.Text (Text, unpack, pack, splitOn, append)
import Data.Attoparsec.Text hiding (count)
import qualified Data.Text as T
import qualified Data.ByteString as B

import Conduit
import qualified Data.Conduit.Combinators as C

import Lib 
import Core

type InPath  = FilePath
type OutPath = FilePath

{-----------------------------------------------------------------------------
  Filter grepped files
------------------------------------------------------------------------------}

-- * Given parser `p` and `inpath` to ngrams file, take all lines
-- * in file recognized by `p` and save to output file in `outpath`
filterByPattern :: InPath -> OutPath -> Parser Text ->  IO ()
filterByPattern inpath outpath p  =  run 
                                  $  sourceFile inpath 
                                  $= toInput
                                  $= C.filter (\(t,_,_) -> p <**? preprocess t)
                                  $= fromInput
                                  $$ sinkFile outpath


toInput :: FileOp m => Conduit B.ByteString m Input
toInput = linesOn "\n" 
     $= C.map head
     $= C.map    (splitOn $ pack ":"                   ) 
     $= C.filter ((==2) . length                       )
     $= C.map    (\[a,b]   -> (a:splitOn (pack "\t") b))
     $= C.filter ((==3) . length                       )
     $= C.map    (\[s,t,n] -> (t, n, s)                )


fromInput :: FileOp m => Conduit Input m B.ByteString
fromInput = C.map (\(a,b,c) -> encodeUtf8 
                          $  T.concat [ a
                                      , pack "\t"
                                      , b
                                      , pack "\t"
                                      , c
                                      , pack "\n"])

{-----------------------------------------------------------------------------
  Filter raw ngram files
------------------------------------------------------------------------------}

filterByPattern' :: InPath -> OutPath -> Parser Text -> IO ()
filterByPattern' inpath outpath p =  run 
                                  $  sourceFile inpath
                                  $= toInput'
                                  $= C.filter (\(t,_,_) -> p <**? preprocess t)
                                  $= fromInput
                                  $$ sinkFile outpath


toInput' :: FileOp m => Conduit B.ByteString m Input
toInput' = linesOn "\n"
       $= C.map head
       $= C.map    (splitOn $ pack "\t"     )
       $= C.filter ((==2) . length          )
       $= C.map    (\[t,n] -> (t,n, pack ""))








