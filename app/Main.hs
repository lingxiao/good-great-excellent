-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Main
-- | Author  : Xiao Ling
-- | Date    : 9/11/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Main where

import System.FilePath.Posix
import System.Directory
import Data.Attoparsec.Text 
import Data.Text hiding (head, replicate, filter)


import Src
import Lib
import Subroutines



{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}


-- * do this on 
-- * do exhaustive search over all 5grams
p1    = compile "not * (,) although still *" Star Star
p2    = compile "not * (,) though still *" Star Star
rootl = "/Users/lingxiao/Documents/research/data/ngrams/raw/5gms/"
rootr = "/nlp/data/xiao/ngrams/raw/5gms/"

main :: IO ()
main = do
    filter5mgs rootr p1
    filter5mgs rootr p2


-- * run this because we fail to grab any data from 89, 90 or 91 gms
-- * also didnt get data from not-star.txt
filter5mgs :: FilePath -> Parser Text ->  IO ()
filter5mgs root p = do
    createDirectoryIfMissing False $ root ++ name p

    fs      <- getDirectoryContents root

    let files = filter (\f -> takeExtension f == ".txt") 
              $ fmap   ((++) root) fs

    (\f -> filterByPattern' f (root ++ name p ++ "/" ++ takeFileName f) p1) `mapM` files

    return ()           


{-----------------------------------------------------------------------------
    Paths
------------------------------------------------------------------------------}

root_local, root_remote, data_local :: DirectoryPath
root_local  = "/Users/lingxiao/Documents/research/code/good-great-excellent"
root_remote = "/home1/l/lingxiao/xiao/good-great-excellent"
data_local  = "/Users/lingxiao/Documents/research/data/ngrams"

not_star_r  = "/nlp/data/xiao/ngrams/raw-not/not-star.txt"
not_star_l  = "/Users/lingxiao/Documents/research/data/ngrams/raw-not/not-star.txt"






