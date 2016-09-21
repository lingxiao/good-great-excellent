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


import Data.Text (Text, unpack, pack)
import System.FilePath.Posix
import Data.Conduit.Text 



import Src
import Lib
import Scripts

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

weak_strong :: [PatternExpr]
weak_strong = ["* (,) but not (a|an|the) *"
             ,"* (,) if not (a|an|the) *"
             ,"* (,) although not (a|an|the) *"
             ,"* (,) though not (a|an|the) *"
             ,"* (,) (and|or) even (a|an|the) *"
             ,"* (,) (and|or) almost (a|an|the) *"
             ,"not only * (,) but *"
             ,"not just * (,) but *"
             ]

strong_weak :: [PatternExpr]
strong_weak = ["not (a|an|the) * (,) just (a|an|the) *"
             ,"not (a|an|the) * (,) but just (a|an|the) *"
             ,"not (a|an|the) * (,) still (a|an|the) * "
             ,"not (a|an|the) * (,) but still (a|an|the) *"
             ,"not (a|an|the) * (,) although still (a|an|the) *"
             ,"not (a|an|the) * (,) though still (a|an|the) * "
             ,"* (,) or very *"
             ]

-- * right now: run through normalize >> step1 >> step2 >> step3
main :: IO ()
main = do
  main_split_by_pattern rgrep_ws $ (\p -> compile p Star Star) <$> weak_strong


{-----------------------------------------------------------------------------
  task stack
------------------------------------------------------------------------------}


  -- * loop to count occurences
  --step1 grep_sm p1
  --step2 grep_sm p1
  --step3 grep_sm p1

-- * count raw frequencies
step1 :: DirectoryPath -> PatternExpr -> IO ()
step1 root p = do
  let path = root ++ p ++ ".txt"
  n <- raw_freq path
  print p
  print "------------------------------------------------"
  print $ "raw frequency: " ++ show n

-- * filter by pattern
step2 :: DirectoryPath -> PatternExpr -> IO ()
step2 root p = main_split_by_pattern root [compile p Star Star]

-- * count filtered out and filtered in to make sure:
-- *   count (out ++ in) = count(out) + count(in) = count (raw)
step3 :: DirectoryPath -> PatternExpr -> IO ()
step3 root xs = do
  n <- total_freq $ root ++ "/out/" ++ xs ++ ".txt"
  m <- total_freq $ root ++ "/out/leftover-" ++ xs ++ ".txt"
  print $ "conform    : " ++ show n
  print $ "not-conform: " ++ show m
  print $ "total      : " ++ show (n + m)

{-----------------------------------------------------------------------------
  Paths
------------------------------------------------------------------------------}

-- * remote
rgrep_ws, rgrep_sw :: DirectoryPath
rgrep_ws  = "/nlp/data/xiao/ngrams/greped/weak-strong/"
rgrep_sw  = "/nlp/data/xiao/ngrams/greped/strong-weak/"


-- * local
grep_sm = "/Users/lingxiao/Documents/research/data/ngrams/grep-small/"
grep_ws = "/Users/lingxiao/Documents/research/data/ngrams/greped/weak-strong/"
grep_sw = "/Users/lingxiao/Documents/research/data/ngrams/greped/strong-weak/"

corpus_l   = "/Users/lingxiao/Documents/research/data/ngrams/corpus/"
patterns_l = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"


config_l :: IO Config
config_l = do
    Just con <- config corpus_l patterns_l
    return con
























