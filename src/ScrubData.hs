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

    scrub
  , conform_pattern
  , conform_pattern'
  , not_conform_pattern

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

-- * `preprocess` each line of file found at `inp` and save to `outp`
-- * by case folding and whitespace stripping
scrub :: CT.Codec -> InPath -> OutPath -> IO ()
scrub c inp outp =  run 
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

{-----------------------------------------------------------------------------
  Filter grepped files
------------------------------------------------------------------------------}

-- * Given parser `p` and `inpath` to ngrams file, take all lines
-- * in file recognized by `p` and save to output file in `outpath`
conform_pattern :: Parser Text -> InPath -> OutPath -> IO ()
conform_pattern p  = go (\(t,_,_) -> p <**? preprocess t) p CT.utf8

-- * Given parser `p` and `inpath` to ngrams file, take all lines
-- * in file not recognized by `p` and save to output file in `outpath`
-- * may be used for debuggin parser on production data
not_conform_pattern :: Parser Text -> InPath -> OutPath -> IO ()                                  
not_conform_pattern p = go (\(t,_,_) -> not $ p <**? preprocess t) p CT.utf8

{-----------------------------------------------------------------------------
  Filter raw ngram files
------------------------------------------------------------------------------}

-- * Given parser `p` and `inpath` to ngrams file, take all lines
-- * in file recognized by `p` and save to output file in `outpath`
conform_pattern' ::  Parser Text -> InPath -> OutPath -> IO ()
conform_pattern' p inp outp =  run 
                           $  sourceFile inp
                           $= toInput' CT.utf8
                           $= C.filter (\(t,_,_) -> p <**? preprocess t)
                           $= fromInput CT.utf8
                           $$ sinkFile outp

{-----------------------------------------------------------------------------
 Subroutines
------------------------------------------------------------------------------}

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

-- * convert greped ngram files to inputs
toInput :: FileOp m => CT.Codec -> Conduit B.ByteString m Input
toInput code = CT.decode code
       $= CT.lines
       $= C.map    (splitOn . pack $ "\n"                )
       $= C.map    head
       $= C.map    (splitOn $ pack ":"                   ) 
       $= C.filter ((==2) . length                       )
       $= C.map    (\[a,b]   -> (a:splitOn (pack "\t") b))
       $= C.filter ((==3) . length                       )
       $= C.map    (\[s,t,n] -> (t, n, s)                )


-- * convert raw ngrams files to inputs
toInput' :: FileOp m => CT.Codec -> Conduit B.ByteString m Input
toInput' code =  CT.decode code
              $= CT.lines
              $= C.map    (splitOn . pack $ "\n"   )
              $= C.map    head
              $= C.map    (splitOn $ pack "\t"     )
              $= C.filter ((==2) . length          )
              $= C.map    (\[t,n] -> (t,n, pack ""))

-- * convert filered files back to bytestring
fromInput :: FileOp m => CT.Codec -> Conduit Input m B.ByteString
fromInput code = C.map (\(a,b,c) -> T.concat [ a
                                             , pack "\t"
                                             , b
                                             , pack "\t"
                                            , c
                                            , pack "\n"])
               $= CT.encode code


