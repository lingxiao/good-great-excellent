{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A collection of scripts to normalize data
-- | Author  : Xiao Ling
-- | Date    : 9/13/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 
module ScrubData (

    scrub
  , conform_pattern
  , not_conform_pattern

  , toInput
  , fromInput

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
  normalize data
------------------------------------------------------------------------------}

-- * `normalize` each line of file found at `inp` and save to `outp`
-- * decode and encode using codec `c`
scrub :: CT.Codec -> InPath -> OutPath -> IO ()
scrub c inp outp =  run 
                 $  sourceFile inp
                 $= CT.decode c
                 $= CT.lines
                 $= C.map    (splitOn . pack $ "\t"           )
                 $= C.filter ((==2) . length                  )
                 $= C.map    (\[t,n] -> T.concat [ normalize t
                                                 , pack "\t"
                                                 , n
                                                 , pack "\n"] )
                 $= CT.encode c
                 $$ sinkFile outp


{-----------------------------------------------------------------------------
  Filter grepped normalized-ngram files
------------------------------------------------------------------------------}

-- * Given parser `p` and `inpath` to normalized ngrams file, take all lines
-- * in file recognized by `p` and save to output file in `outpath`
conform_pattern :: Parser Text -> InPath -> OutPath -> IO ()
conform_pattern p  = go (\(t,_) -> p <**? t) p CT.utf8

-- * Given parser `p` and `inpath` to normalized ngrams file, take all lines
-- * in file not recognized by `p` and save to output file in `outpath`
-- * may be used for debuggin parser on production data
not_conform_pattern :: Parser Text -> InPath -> OutPath -> IO ()                                  
not_conform_pattern p = go (\(t,_) -> not $ p <**? t) p CT.utf8


-- * Subroutines * --

-- * Given parser `p` and `inpath` to ngrams file, take all lines
-- * in file satisfying `predicate` and save to output file in `outpath`
go :: (Input -> Bool) 
   -> Parser Text 
   -> CT.Codec 
   -> InPath 
   -> OutPath 
   -> IO ()
go predicate p code inp outp = run 
                            $  sourceFile inp 
                            $= toInput code
                            $= C.filter predicate
                            $= fromInput code
                            $$ sinkFile outp

-- * hypothesis: there's problems here
-- * convert greped ngram files to inputs
toInput :: Op m => CT.Codec -> Conduit B.ByteString m Input
toInput code =  CT.decode code
             $= CT.lines
             $= awaitForever (\xs -> yield . toTuple $ xs)

-- * convert filered files back to bytestring
fromInput :: Op m => CT.Codec -> Conduit Input m B.ByteString
fromInput code = C.map (\(t,n) -> T.concat [ t
                                           , pack "\t"
                                           , n
                                           , pack "\n"]
                        )
               $= CT.encode code

toTuple :: Text -> (Text, Text)
toTuple xs = (t,n)
  where [t,n] = splitOn (pack "\t") xs

