-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A collection of scripts to be run in main
-- | Author  : Xiao Ling
-- | Date    : 9/13/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 
module Subroutines where

import System.IO
import System.FilePath.Posix

import Data.Text (Text)
import Data.Attoparsec.Text 


import Src
import Lib 



base :: FilePath
base     = "/Users/lingxiao/Documents/research/data/ngrams/search/"

inpath_ws :: [FilePath]
inpath_ws = ((++) base) <$> [ "but-not.txt"
                            , "if-not.txt"
                            , "although-not.txt"
                            , "though-not.txt"
                            , "andor-even.txt"
                            , "andor-almost.txt"
                            , "not-just.txt"
                            , "not-only.txt"   
                            ]
 
inpath_sw :: [FilePath]
inpath_sw = ((++) base ) <$> [ "not-just.txt"
                             , "but-just.txt"
                             , "not-still.txt"
                             , "but-still.txt"
                             , "although-still.txt"
                             , "though-still.txt"
                             , "or-very.txt"
                             ]

p_ws :: FilePath
p_ws = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/weak-strong-patterns.txt"

p_sw :: FilePath
p_sw = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/strong-weak-patterns.txt"


{-----------------------------------------------------------------------------
  preprocesss data
------------------------------------------------------------------------------}

-- * @Use : main_filterByPattern ["path/to/data.txt"] "path/to/patterns.txt"
-- *        filters each data.txt by corresponding pattern found in patter.txt and
-- * Note : input data.txt must correspond to the appropriate pattern
-- *        or all data.txt will be filtered out
-- *        save each output in "path/to/"
main_filterByPattern :: [FilePath] 
                     -> FilePath 
                     -> IO [(FilePath, String)]
main_filterByPattern fs patterns = do
  fs <- lines <$> readFile patterns
  let ps  = (\f -> compile f Star Star) <$> fs
  let fps = zip fs ps
  uncurry filterByPattern' `mapM` fps
  return $ (\(f,p) -> (f, name p)) <$> fps


-- @Use : filterByPattern' "path/to/pattern-data.txt" p
-- *      save lines of patter-data.txt recongized by p 
-- *      in output directory "path/to/pattern-data_filtered.txt"
filterByPattern' :: FilePath -> Parser Text -> IO FilePath
filterByPattern' inp p = do
  let outp =  takeDirectory inp
          ++ "/" 
          ++ takeBaseName inp
          ++ "_filtered"
          ++ ".txt"

  is <- lines <$> readFile inp
  os <- filterByPattern inp outp p 
  return outp
  










































