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
 
module ScrubData (

    conformToPattern
  , notConformToPattern
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
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Combinators as C

import Lib 
import Core

type InPath  = FilePath
type OutPath = FilePath

{-----------------------------------------------------------------------------
  case fold data
------------------------------------------------------------------------------}

inroot :: DirectoryPath
inroot = "/Users/lingxiao/Documents/research/data/ngrams/search/4gms/"
name   = "4gm-0005.txt"
inp    = inroot ++ name
outp   = inroot ++ "scrub/" ++ name

-- * `preprocess` each line of file found at `inp` and save to `outp`
scrub :: InPath -> OutPath -> CT.Codec -> IO ()
scrub inp outp c =  run 
                 $  sourceFile inp
                 $= CT.decode c
                 $= CT.lines
                 $= C.map    (splitOn . pack $ "\t"           )
                 $= C.filter ((==2) . length                  )
                 $= C.map    (\[t,n] -> T.concat [ preprocess t
                                                 , pack "\t"
                                                 , n
                                                 , pack "\n"] )
                 $= CT.encode c
                 $$ sinkFile outp


foo :: IO ()
foo = scrub inp outp CT.utf8

{-----------------------------------------------------------------------------
  Filter grepped files
------------------------------------------------------------------------------}

-- * Given parser `p` and `inpath` to ngrams file, take all lines
-- * in file recognized by `p` and save to output file in `outpath`
conformToPattern :: InPath -> OutPath -> Parser Text ->  IO ()
conformToPattern inpath outpath p  = go inpath outpath p
                                     (\(t,_,_) -> p <**? preprocess t)


-- * Given parser `p` and `inpath` to ngrams file, take all lines
-- * in file not recognized by `p` and save to output file in `outpath`
-- * may be used for debuggin parser on production data
notConformToPattern :: InPath -> OutPath -> Parser Text -> IO ()                                  
notConformToPattern inpath outpath p = go inpath outpath p
                                       (\(t,_,_) -> not $ p <**? preprocess t)


-- * Given parser `p` and `inpath` to ngrams file, take all lines
-- * in file satisfying `predicate` and save to output file in `outpath`scan :: InPath -> OutPath -> Parser Text -> (Input -> Bool) -> IO ()
go :: InPath -> OutPath -> Parser Text -> (Input -> Bool) -> IO ()
go inpath outpath p predicate =  run 
                               $  sourceFile inpath 
                               $= toInput
                               $= C.filter predicate
                               $= fromInput
                               $$ sinkFile outpath


  -- *linesOn "\n" 

toInput :: FileOp m => Conduit B.ByteString m Input
toInput = CT.decode CT.iso8859_1
       $= CT.lines
       $= C.map    (splitOn . pack $ "\n"                )
       $= C.map    head
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
toInput' = CT.decode CT.utf16_le
       $= CT.lines
       $= C.map    (splitOn . pack $ "\n")
       $= C.map    head
       $= C.map    (splitOn $ pack "\t"     )
       $= C.filter ((==2) . length          )
       $= C.map    (\[t,n] -> (t,n, pack ""))








