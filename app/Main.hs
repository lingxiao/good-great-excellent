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
import qualified Data.Conduit.Text as CT


import Src
import Lib
import Scripts

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

pbut = compile "* (,) but not (a|an|the) *" Star Star
pif  = compile "* (,) if not (a|an|the) *" Star Star


{-

next:

1. grep files for but not
2. count total occurences, call it m
3. (confrom_to_pattern, not_conform_to_pattern)
4. count occurence of each to make sure it sums to m
5. reference against vendor data, if still off
   by orders of magnitude then something else is going on

-}
main :: IO ()
main = do
  fs'       <- getDirectoryContents r5gm
  let fs    = filter (\f -> takeExtension f == ".txt") fs'
  let inps  = (++) r5gm <$> fs
  let outps = (\p -> r5gm ++ "scrub/" ++ p) <$> fs
  let ps    = zip inps outps
  mapM (\(i,o) -> scrub i o CT.utf8) ps
  return ()

foo :: Int -> IO ()
foo n = do
  let inroot = "/Users/lingxiao/Documents/research/data/ngrams/search/4gms/"
  let inp    = inroot ++ "4gm-00" ++ show n ++ ".txt"
  let outp   = inroot ++ "scrub/" ++ "4gm-00" ++ show n ++ ".txt"
  scrub CT.utf8 inp outp 



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
r4gm, r4gm_scrub, r5gm, r5gm_scrub :: DirectoryPath
r4gm       = "/nlp/data/xiao/ngrams/raw/4gms/"
r4gm_scrub = r4gm ++ "scrub/"
r5gm       = "/nlp/data/xiao/ngrams/raw/5gms/"
r5gm_scrub = r5gm ++ "scrub/"



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
























