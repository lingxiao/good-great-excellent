-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Main
-- | Author  : Xiao Ling
-- | Date    : 10/31/2016
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

first = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth"]
known = ["known","famous","legendary"]
dim   = ["dim","gloomy","dark","black"]
close = ["close","near","intimate"]
near  = ["nearby", "near","close","adjacent"]
suffi = ["sufficient","good", "wide","full"]

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

main :: IO ()
main = do
  collect suffi
  collect near
  collect dim
  collect known
  collect first
  collect close

{-----------------------------------------------------------------------------
  routine
------------------------------------------------------------------------------}

collect :: [String] -> IO ()
collect wrds = do
  con        <- sysConfig
  let inpath = corpus con
  let weak   = weakStrong con
  let strong = strongWeak con

  main_count_words wrds
  count_phrase strong inpath "strong" `mapM` pset wrds
  count_phrase weak   inpath "weak"   `mapM` pset wrds
  return ()


-- * query for word frequence of every word in here
main_count_words :: [String] -> IO ()
main_count_words xs = do
  con <- sysConfig
  let inpath = corpus con ++ "vocab.txt"
  count_words inpath "words" xs
  return ()

pset :: Eq a => [a] -> [(a,a)]
pset xs = [(u,v) | u <- xs, v <- xs]

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





















