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
    ps <- main_filterByPattern inpath_sw p_sw
    mapM print ps
    return ()


{-----------------------------------------------------------------------------
    Pathss  
------------------------------------------------------------------------------}



base :: FilePath
base  = "/Users/lingxiao/Documents/research/data/ngrams/"
tbase = "/Users/lingxiao/Documents/research/code/good-great-excellent/test/assets/r"

inpath_ws :: [FilePath]
inpath_ws = ((++) (base ++ "raw-strong-weak/"))
        <$> [ "but-not.txt"
            , "if-not.txt"
            , "although-not.txt"
            , "though-not.txt"
            , "andor-even.txt"
            , "andor-almost.txt"
            , "not-only.txt"   
            , "not-just.txt"
            ]
 
inpath_sw :: [FilePath]
inpath_sw = ((++) (base ++ "raw-strong-weak/"))
        <$> [ "not-*-just-*.txt"
            , "not-*-but-just-*.txt"
            , "not-*-still-*.txt"
            , "not-*-but-still-*.txt"
            , "not-*-although-still-*.txt"
            , "not-*-though-still-*.txt"
            , "*-or-very-*.txt"
            ]

p_ws :: FilePath
p_ws = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/weak-strong-patterns.txt"

p_sw :: FilePath
p_sw = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/strong-weak-patterns.txt"

