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

main :: IO ()
main = do
    con <- config_l
    main_pattern_freq (corpus con    ) 
                      (strongWeak con) 
                      "strong-weak-occurences-short"
    --main_pattern_freq con (weakStrong con) "weak-strong-occurences"

{-----------------------------------------------------------------------------
    Configurations
------------------------------------------------------------------------------}


corpus_l   = "/Users/lingxiao/Documents/research/data/ngrams/corpus/"
patterns_l = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"

root_ws    = "/Users/lingxiao/Documents/research/data/ngrams/grep-weak-strong/"
root_sw    = "/Users/lingxiao/Documents/research/data/ngrams/grep-strong-weak/"


config_l :: IO Config
config_l = do
    Just con <- config corpus_l patterns_l
    return con
























