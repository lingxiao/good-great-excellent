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
import qualified Data.Conduit.Text as CT


import Src
import Lib
import Scripts

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}



good_bad =    [   "good"
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

wet_dry =     [ "wet"
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


good_bad' =   [ "good"
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

soph_naif =   ["innocent", "simple", "naive", "childlike", "naif"]

char_unchar = ["characteristic"
              , "limited"
              , "special"
              , "peculiar"
              , "specific"
              , "particular"
              , "uncharacteristic"
              , "unique"
              ]

-- * query for word frequence of every word in here
main :: IO ()
main = do
  count_word "/nlp/data/xiao/ngrams/normalized/1gm/vocab.txt"
             $ good_bad 
             ++ wet_dry 
             ++ good_bad' 
             ++ soph_naif 
             ++ char_unchar

  return ()

{-----------------------------------------------------------------------------
  Paths
------------------------------------------------------------------------------}

-- * local
grep_sm    = "/Users/lingxiao/Documents/research/data/ngrams/grep-small/"
grep_ws    = "/Users/lingxiao/Documents/research/data/ngrams/greped/weak-strong/"
grep_sw    = "/Users/lingxiao/Documents/research/data/ngrams/greped/strong-weak/"

corpus_l   = "/Users/lingxiao/Documents/research/data/ngrams/corpus/"
patterns_l = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"


-- * remote
corpus_r   = "/nlp/data/xiao/ngrams/corpus/"
patterns_r = "/home1/l/lingxiao/xiao/good-great-excellent/inputs/"


config_l :: IO Config
config_l = do
    Just con <- config corpus_r patterns_r
    return con
























