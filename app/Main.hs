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



good_bad = [   "good"
             , "bad"
             , "better"
             , "best"
             , "acceptable"
             , "satisfactory"
             , "good"
             , "great"
             , "solid"
             , "superb"
             ]

wet_dry = [ "wet"
          , "dry"
          , "muddy"
          , "sticky"
          , "humid"
          , "tacky"
          , "moist"
          , "damp"
          , "steamy"
          , "wet"
          , "drippy"
          , "watery"
          , "boggy"
          , "soggy"
          , "rainy"
          , "waterlogged"
          ]


good_bad' = [ "good"
            , "bad"
            , "evil"
            , "negative"
            , "mediocre"
            , "poor"
            , "bad"
            , "worse"
            , "awful"
            , "worst"
            , "terrible"
            ]

soph_naif = ["innocent", "simple", "naive", "childlike", "naif"]

char_unchar = ["characteristic"
              , "limited"
              , "special"
              , "peculiar"
              , "specific"
              , "particular"
              , "uncharacteristic"
              , "unique"
              ]

main :: IO ()
main = do
  con <- config_l
  let weak_strong = weakStrong con
  let strong_weak = strongWeak con

  let pws = [(u,v) | u <- good_bad, v <- good_bad ]

  query_save weak_strong (corpus con) "good-bad-2-weak-strong" `mapM` power good_bad'
  query_save strong_weak (corpus con) "good-bad-2-strong-weak" `mapM` power good_bad'

  query_save weak_strong (corpus con) "soph-naif-weak-strong" `mapM` power soph_naif
  query_save strong_weak (corpus con) "soph-naif-strong-weak" `mapM` power soph_naif

  query_save weak_strong (corpus con) "char-unchar-weak-strong" `mapM` power char_unchar
  query_save strong_weak (corpus con) "char-unchar-strong-weak" `mapM` power char_unchar

  return ()

power :: [a] -> [(a,a)]
power xs = [(u,v) | u <- xs, v <- xs]


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
corpus_r    = "/nlp/data/xiao/ngrams/corpus/"
patterns_r  = "/home1/l/lingxiao/xiao/good-great-excellent/inputs/"


config_l :: IO Config
config_l = do
    Just con <- config corpus_r patterns_r
    return con
























