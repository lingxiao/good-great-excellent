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

import System.Directory
import Data.Text hiding (head, replicate)

import Src
import Lib
import Subroutines



{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

p1 = compile "not * (,) but just *" Star Star

main :: IO ()
main = do
    filterPattern not_star_r $ compile "not * (,) but still *"      Star Star
    filterPattern not_star_r $ compile "not * (,) although still *" Star Star
    filterPattern not_star_r $ compile "not * (,) though still *"   Star Star
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






