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
import Data.Text (Text, unpack, pack, splitOn)

import Src
import Lib
import Scripts

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

weak_strong :: [PatternExpr]
weak_strong = ["* (,) but not (a|an|the) *"              --  no misparse in grep
              ,"* (,) if not (a|an|the) *"               --  no misparse in grep
              ,"* (,) although not (a|an|the) *"         --  no misparse in grep
              ,"* (,) though not (a|an|the) *"           --  no misparse in grep
              ,"* (,) (and|or) even (a|an|the) *"        --  no misparse in and even
              ,"* (,) (and|or) almost (a|an|the) *"      --
              ,"not only * (,) but *"                    --
              ,"not just * (,) but *"                    -- 
              ]

strong_weak :: [PatternExpr]
strong_weak = ["not (a|an|the) * (,) just (a|an|the) *"
              ,"not (a|an|the) * (,) but just (a|an|the) *"
              ,"not (a|an|the) * (,) still (a|an|the) *"
              ,"not (a|an|the) * (,) but still (a|an|the) *"
              ,"not (a|an|the) * (,) although still (a|an|the) *"
              ,"not (a|an|the) * (,) though still (a|an|the) *"
              ,"* (,) or very *"
              ]


-- * debug

fs = "/Users/lingxiao/Documents/research/data/ngrams/small/greped.txt"

-- * right now: run through normalize >> step1 >> step2 >> step3
main :: IO ()
main = do
  step1 grep_ws `mapM` ["* (,) although not (a|an|the) *", "* (,) though not (a|an|the) *"]
  step2 grep_ws `mapM` ["* (,) although not (a|an|the) *", "* (,) though not (a|an|the) *"]
  step3 grep_ws `mapM` ["* (,) although not (a|an|the) *", "* (,) though not (a|an|the) *"]
  return ()


{-----------------------------------------------------------------------------
  task stack
------------------------------------------------------------------------------}

-- * count raw frequencies
step1 :: DirectoryPath -> PatternExpr -> IO ()
step1 root p = do
  let path = root ++ p ++ ".txt"
  n <- raw_freq path
  print p
  print $ "raw frequency: " ++ show n
  print "------------------------------------------------"

-- * filter by pattern
step2 :: DirectoryPath -> PatternExpr -> IO ()
step2 root p = main_split_by_pattern root [compile p Star Star]

-- * count filtered out and filtered in to make sure:
-- *   count (out ++ in) = count(out) + count(in) = count (raw)
step3 :: DirectoryPath -> PatternExpr -> IO ()
step3 root xs = do
  n <- total_freq $ root ++ "/out/" ++ xs ++ ".txt"
  m <- total_freq $ root ++ "/out/leftover-" ++ xs ++ ".txt"
  print xs
  print $ "conform    : " ++ show n
  print $ "not-conform: " ++ show m
  print $ "total      : " ++ show (n + m)
  print "------------------------------------------------"


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
























