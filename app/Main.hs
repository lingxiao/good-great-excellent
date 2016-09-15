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

import Src
import Lib
import Subroutines

import Data.Text hiding (head, replicate)


{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}


main :: IO ()
main = do
    --ps <- filter_weakStrong con_local_ws
    --ps <- filter_strongWeak con_remote_not
    --mapM print ps
    filterPattern not_star_r $ compile "not * (,) but just *"       Star Star
    filterPattern not_star_r $ compile "not * (,) still *"          Star Star
    filterPattern not_star_r $ compile "not * (,) but still *"      Star Star
    filterPattern not_star_r $ compile "not * (,) although still *" Star Star
    filterPattern not_star_r $ compile "not * (,) though still *"   Star Star

    return ()

{-----------------------------------------------------------------------------
    Paths
------------------------------------------------------------------------------}

root_local, root_remote, data_local :: DirectoryPath
root_local  = "/Users/lingxiao/Documents/research/code/good-great-excellent"
root_remote = "/home1/l/lingxiao/xiao/good-great-excellent"
data_local  = "/Users/lingxiao/Documents/research/data/ngrams"

not_star_r  = "/nlp/data/xiao/ngrams/raw-not/not-star.txt"
not_star_l  = "/Users/lingxiao/Documents/research/data/ngrams/raw-not/not-star.txt"

con_local_ws, con_local_sw, con_remote_not :: Config
con_local_ws   = config root_local  $ inpath_raw_ws data_local
con_local_sw   = config root_local  $ inpath_raw_sw data_local
con_remote_not = config root_remote $ replicate 100 not_star_r


inpath_raw_ws :: DirectoryPath -> [FilePath]
inpath_raw_ws d = ((++) (d ++ "/raw-weak-strong/"))
        <$> [ "but-not.txt"
            , "if-not.txt"
            , "although-not.txt"
            , "though-not.txt"
            , "andor-even.txt"
            , "andor-almost.txt"
            , "not-only.txt"   
            , "not-just.txt"
            ]
 
inpath_raw_sw :: DirectoryPath -> [FilePath]
inpath_raw_sw d = ((++) (d ++ "/raw-strong-weak/"))
        <$> [ "not-*-just-*.txt"
            , "not-*-but-just-*.txt"
            , "not-*-still-*.txt"
            , "not-*-but-still-*.txt"
            , "not-*-although-still-*.txt"
            , "not-*-though-still-*.txt"
            , "*-or-very-*.txt"
            ]








