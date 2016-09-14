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


{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}


main :: IO ()
main = do
    --ps <- filter_weakStrong con_local_ws
    ps <- filter_strongWeak con_remote_not
    -- mapM print ps
    return ()

{-----------------------------------------------------------------------------
    Paths
------------------------------------------------------------------------------}

root_local, root_remote, data_local :: DirectoryPath
root_local  = "/Users/lingxiao/Documents/research/code/good-great-excellent"
root_remote = "/home1/l/lingxiao/xiao/good-great-excellent"
data_local  = "/Users/lingxiao/Documents/research/data/ngrams"


con_local_ws, con_local_sw, con_remote_not :: Config
con_local_ws   = config root_local  $ inpath_ws data_local
con_local_sw   = config root_local  $ inpath_sw data_local
con_remote_not = config root_remote
               $ replicate 100 "/nlp/data/xiao/ngrams/raw-not/not-star.txt"


inpath_ws :: DirectoryPath -> [FilePath]
inpath_ws d = ((++) (d ++ "/raw-weak-strong/"))
        <$> [ "but-not.txt"
            , "if-not.txt"
            , "although-not.txt"
            , "though-not.txt"
            , "andor-even.txt"
            , "andor-almost.txt"
            , "not-only.txt"   
            , "not-just.txt"
            ]
 
inpath_sw :: DirectoryPath -> [FilePath]
inpath_sw d = ((++) (d ++ "/raw-strong-weak/"))
        <$> [ "not-*-just-*.txt"
            , "not-*-but-just-*.txt"
            , "not-*-still-*.txt"
            , "not-*-but-still-*.txt"
            , "not-*-although-still-*.txt"
            , "not-*-though-still-*.txt"
            , "*-or-very-*.txt"
            ]








