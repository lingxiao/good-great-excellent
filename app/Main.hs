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



import Data.Time.Clock
import Data.Text (Text, unpack, pack, splitOn)
import qualified System.IO as S

import Src
import Lib
import Scripts

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}



w_good_bad = [ "good"
             , "better"
             , "best"
             , "acceptable"
             , "satisfactory"
             , "good"
             , "great"
             , "solid"
             , "superb"
             ]



main :: IO ()
main = do
  con <- config_l
  let weak_strong = weakStrong con

  let pws = [(u,v) | u <- w_good_bad, v <- w_good_bad ]

  query_save weak_strong (corpus con) `mapM` pws
  return ()


{-----------------------------------------------------------------------------
  Paths
------------------------------------------------------------------------------}

-- * local
grep_sm = "/Users/lingxiao/Documents/research/data/ngrams/grep-small/"
grep_ws = "/Users/lingxiao/Documents/research/data/ngrams/greped/weak-strong/"
grep_sw = "/Users/lingxiao/Documents/research/data/ngrams/greped/strong-weak/"

corpus_l   = "/Users/lingxiao/Documents/research/data/ngrams/corpus/"
patterns_l = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"


-- * remote
corpus_r    = "/nlp/data/xiao/ngrams/corpus"
patterns_r  = "/home1/l/lingxiao/xiao/good-great-excellent/inputs"


config_l :: IO Config
config_l = do
    Just con <- config corpus_r patterns_r
    return con
























