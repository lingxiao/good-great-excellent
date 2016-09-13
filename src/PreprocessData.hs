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


  ) where

import System.IO
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

p      = compile "* (,) or very *" Star Star
psw    = "/Users/lingxiao/Documents/research/data/ngrams/subset/strong-weak-input.txt"
pshort = "/Users/lingxiao/Documents/research/data/ngrams/dummy/ws_short.txt"


{-----------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type Raw = (Text, Text, Text)

{-----------------------------------------------------------------------------
  Code
------------------------------------------------------------------------------}

-- * Given parser `p` and `inpath` to ngrams file, take all lines
-- * in file recognized by `p` and save to output file in `outpath`
filterByPattern :: Parser Text -> FilePath -> IO ()
filterByPattern p inpath = do
  let outpath = takeDirectory inpath ++ "/" ++ name p ++ ".txt"
  run $ filterByPattern' p inpath outpath


-- * try again with conduit
filterByPattern' :: FileOp m => Parser Text -> FilePath -> FilePath -> m ()
filterByPattern' p inpath outpath =  sourceFileE inpath 
                                  $= toRaw
                                  $= C.filter (\(t,_,_) -> p <**? preprocess t)
                                  $= fromRaw
                                  $= sinkFile outpath
                                  $$ cap

toRaw :: FileOp m => Conduit B.ByteString m Raw
toRaw = linesOn "\n" 
     $= C.map head
     $= C.map    (splitOn (pack ":")) 
     $= C.filter ((==2) . length)
     $= C.map    (\[a,b]   -> (a:splitOn (pack "\t") b))
     $= C.filter ((==3) . length)
     $= C.map    (\[s,t,n] -> (t, n, s)                )


fromRaw :: FileOp m => Conduit Raw m B.ByteString
fromRaw = C.map (\(a,b,c) -> encodeUtf8 
                          $  T.concat [ a
                                      , pack "\t"
                                      , b
                                      , pack "\t"
                                      , c
                                      , pack "\n"])


