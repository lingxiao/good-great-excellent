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
import Control.Monad.Trans.Reader
import Data.Attoparsec.Text 
import Data.Text hiding (head, replicate, filter, foldr, zip)

import Src
import Lib
import Scripts

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}


    -- * Next: need to take statisitcs over all word pairs and save
    -- * then need to figure out why althoug not, though not
    -- * have no occurences in query


-- * current problem: number's dont line up remotely!!
-- * strategy: count grep "but not" raw number
-- *           to upper bound stuff
-- *  problem: capitalization 
-- * solution: grepping subset of permutation of capitlazation right now

-- * current progress : grepping thoruhg subset of all permutations
-- *                    of weak-strong and strong-weak patterns
-- * alternatively    : preprocess entire corpus and then grep it     

-- * then form: a file of positive and negative examples

pbut = compile "* (,) but not (a|an|the) *" Star Star
pif  = compile "* (,) if not (a|an|the) *" Star Star

main :: IO ()
main = do
    --con <- config_l
    --main_pattern_freq (corpus con    ) 
                      --(strongWeak con) 
                      --"strong-weak-occurences-short"

   -- * filter greped data                   
   main_split_by_pattern grep_ws [pif]

{-----------------------------------------------------------------------------
    Configurations
------------------------------------------------------------------------------}

grep_ws = "/Users/lingxiao/Documents/research/data/ngrams/raw-weak-strong/"
grep_sw = "/Users/lingxiao/Documents/research/data/ngrams/raw-strong-weak/"


corpus_l   = "/Users/lingxiao/Documents/research/data/ngrams/corpus/"
patterns_l = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"

root_ws    = "/Users/lingxiao/Documents/research/data/ngrams/grep-weak-strong/"
root_sw    = "/Users/lingxiao/Documents/research/data/ngrams/grep-strong-weak/"


config_l :: IO Config
config_l = do
    Just con <- config corpus_l patterns_l
    return con
























