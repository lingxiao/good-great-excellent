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


import Data.Text hiding (head, replicate, filter, foldr, zip)
import System.FilePath.Posix


import Src
import Lib
import Scripts

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

pbut = "* (,) but not (a|an|the) *" 
pif  = "* (,) if not (a|an|the) *" 

p1 = "* (,) but not (a|an|the) *" 
p2 = "* (,) if not (a|an|the) *" 
p3 = "* (,) although not (a|an|the) *" 
p4 = "* (,) though not (a|an|the) *" 
p5 = "* (,) (and|or) even (a|an|the) *" 
p6 = "* (,) (and|or) almost (a|an|the) *" 
p7 = "not only * (,) but *" 
p8 = "not just * (,) but *" 


r1 = "not (a|an|the) * (,) just (a|an|the) *"     
r2 = "not (a|an|the) * (,) but just (a|an|the) *"     
r3 = "not (a|an|the) * (,) still (a|an|the) *"        
r4 = "not (a|an|the) * (,) but still (a|an|the) *"    
r5 = "not (a|an|the) * (,) although still (a|an|the) *"
r6 = "not (a|an|the) * (,) though still (a|an|the) *" 
r7 = "* (,) or very *"



{-

next:

0. scrubbed all files                             - DONE
1. grep files for but not                         - remote
2. count total occurences, call it m              - local
3. (confrom_to_pattern, not_conform_to_pattern)   - local
4. count occurence of each to make sure it sums to m
5. reference against vendor data, if still off
   by orders of magnitude then something else is going on

where you left off:

conform + not-conform /= raw-total
strategy ???
  count on reduced text to see where the bug appear
  see what causes the bug

  grep separate file and check numbers


strategy:

normalize text:
   strip comma
   lower case
   strip whitespace or tab characters


total      : 91,414
conform    : 9,844
not-conform: 81,570
total      : 91,414

-}



main :: IO ()
main = do
  (main_normalize r5gm "5gm") `mapM` [0..131]
  return ()

  -- * loop to count occurences
  --step1 grep_sm p1
  --step2 grep_sm p1
  --step3 grep_sm p1


step1 :: DirectoryPath -> PatternExpr -> IO ()
step1 root p = do
  let path = root ++ p ++ ".txt"
  n <- raw_freq path
  print p
  print "------------------------------------------------"
  print $ "raw frequency: " ++ show n


step2 :: DirectoryPath -> PatternExpr -> IO ()
step2 root p = main_split_by_pattern root [compile p Star Star]


step3 :: DirectoryPath -> PatternExpr -> IO ()
step3 root xs = do
  n <- total_freq $ root ++ "/out/" ++ xs ++ ".txt"
  m <- total_freq $ root ++ "/out/leftover-" ++ xs ++ ".txt"
  print $ "conform    : " ++ show n
  print $ "not-conform: " ++ show m
  print $ "total      : " ++ show (n + m)


{-

let name = echo pif
n <- total_freq $ corpus_l ++ name ++ ".txt"
print (name,n)
return ()


con <- config_l
main_pattern_freq (corpus con    ) 
                  (strongWeak con) 
                  "strong-weak-occurences-short"

 * filter greped data                   
main_split_by_pattern grep_ws [pif]


-}




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
grep_sm = "/Users/lingxiao/Documents/research/data/ngrams/grep-small/"
grep_ws = "/Users/lingxiao/Documents/research/data/ngrams/grep-weak-strong/"
grep_sw = "/Users/lingxiao/Documents/research/data/ngrams/grep-strong-weak/"


corpus_l   = "/Users/lingxiao/Documents/research/data/ngrams/corpus/"
patterns_l = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"

root_ws    = "/Users/lingxiao/Documents/research/data/ngrams/grep-weak-strong/"
root_sw    = "/Users/lingxiao/Documents/research/data/ngrams/grep-strong-weak/"


config_l :: IO Config
config_l = do
    Just con <- config corpus_l patterns_l
    return con
























