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


-- * where you left off:
-- *      (1) consider decode >> preprocess >> encode utf8
-- *      (2) consider case where cannot decode,
-- *          simply skip over line



pbut = compile "* (,) but not (a|an|the) *" Star Star
pif  = compile "* (,) if not (a|an|the) *" Star Star

main :: IO ()
main = return ()
  -- * hypothesis: since we took care of cases
  -- * this should conform to example --> but actually have less

  {-

  let name = echo pif
  n <- total_freq $ corpus_l ++ name ++ ".txt"
  print (name,n)
  return ()
  -}

  -- * where you left off: reallly need to
  -- * take care of case folding and encoding up front
  -- * or you'll have tons of problems later
  -- * consider using tworkenize


  -- * decode with utf-8, if decoding no good then
  -- * put in symbol unknown



    --con <- config_l
    --main_pattern_freq (corpus con    ) 
                      --(strongWeak con) 
                      --"strong-weak-occurences-short"

   -- * filter greped data                   
   --main_split_by_pattern grep_ws [pif]

{-----------------------------------------------------------------------------
    Configurations
------------------------------------------------------------------------------}

-- * remote
-- * 4gm, 4gm_scrub, 5gm, 5gm_scrub :: DirectoryPath
--4gm       = "/nlp/data/xiao/ngrams/raw/4gms/"
--4gm_scrub = 4gm ++ "scrub/"
--5gm       = "/nlp/data/xiao/ngrams/raw/5gms/"
--5gm_scrub = 5gm ++ "scrub/"




-- * local
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
























