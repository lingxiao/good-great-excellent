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
import Data.Text (Text, unpack, pack, splitOn)
import Data.Set  (Set, union, fromList, toList)
import qualified System.IO as S
import qualified Data.Conduit.Text as CT


import Src
import Lib
import Scripts

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

good = [ "good"
       , "bad"
       , "better"
       , "best"
       , "acceptable"
       , "satisfactory"
       , "great"
       , "solid"
       , "superb"]

wet = [ "wet"
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
      , "waterlogged"]


bad = [ "good"
      , "bad"
      , "evil"
      , "negative"
      , "mediocre"
      , "poor"
      , "bad"
      , "worse"
      , "awful"
      , "worst"
      , "terrible"]

simple =  [ "innocent"
          , "simple"
          , "naive"
          , "childlike"
          , "naif"]

special = ["characteristic"
          , "limited"
          , "special"
          , "peculiar"
          , "specific"
          , "particular"
          , "uncharacteristic"
          , "unique"]

main :: IO ()
main = do
  con <- sysConfig
  let inpath = corpus con
  let weak   = weakStrong con
  let strong = strongWeak con

  count_phrase weak   inpath "temp" `mapM` pset special
  count_phrase strong inpath "temp" `mapM` pset special

  count_phrase weak   inpath "temp" `mapM` pset wet
  count_phrase strong inpath "temp" `mapM` pset wet

  count_phrase weak   inpath "temp" `mapM` pset bad
  count_phrase strong inpath "temp" `mapM` pset bad

  count_phrase weak   inpath "temp" `mapM` pset simple
  count_phrase strong inpath "temp" `mapM` pset simple

  return ()

pset :: [a] -> [(a,a)]
pset xs = [(u,v) | u <- xs, v <- xs ]

-- * query for word frequence of every word in here
main_count_words :: IO ()
main_count_words = do
  con <- sysConfig
  let inpath = corpus con ++ "vocab.txt"
  count_words inpath "out" special
  return ()

{-----------------------------------------------------------------------------
  Paths
------------------------------------------------------------------------------}

sysConfig :: IO Config
sysConfig = do
  d <- take 6 <$> getCurrentDirectory 
  if d == "/Users" then config_l
  else config_r


config_l :: IO Config
config_l = do
    Just con <- config corpus_l patterns_l
    return con

config_r :: IO Config
config_r = do
    Just con <- config corpus_r patterns_r
    return con

-- * local

corpus_l   = "/Users/lingxiao/Documents/research/data/ngrams/corpus/"
patterns_l = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"

-- * remote
corpus_r   = "/nlp/data/xiao/ngrams/corpus/"
patterns_r = "/home1/l/lingxiao/xiao/good-great-excellent/inputs/"





















