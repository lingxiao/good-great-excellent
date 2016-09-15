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
import Data.Text hiding (head, replicate, filter, zip)


import Src
import Lib
import Subroutines



{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

p1 = compile "not * (,) just *"           
p2 = compile "not * (,) but just *"       
p3 = compile "not * (,) still *"          
p4 = compile "not * (,) but still *"      
p5 = compile "not * (,) although still *" 
p6 = compile "not * (,) though still *"   
p7 = compile "* (,) or very *" 


p  = compile "* (,) but not *" Star Star


corpus_l   = "/Users/lingxiao/Documents/research/data/ngrams/corpus/"
patterns_l = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"

main :: IO ()
main = do
    con     <- config_l
    (n,os)  <- runReaderT (cnt $ p7 (S "great") (S "good")) con
    print n
    mapM print os
    return ()


{-----------------------------------------------------------------------------
    Paths
------------------------------------------------------------------------------}

config_l :: IO Config
config_l = do
    Just con <- config corpus_l patterns_l
    return con







