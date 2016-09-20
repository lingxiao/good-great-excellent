-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Test preprocess routines
-- | Author  : Xiao Ling
-- | Date    : 8/19/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module PreprocessTest where

import Test.HUnit
import Data.Text 

import Preprocess

{-----------------------------------------------------------------------------
   Run all tests
------------------------------------------------------------------------------}

main :: IO ()
main = do
    runTestTT . TestList 
              $ [ tpreprocess
                ]
    return ()

{-----------------------------------------------------------------------------
    Tasks
------------------------------------------------------------------------------}

tpreprocess :: Test
tpreprocess = let preprocess' = preprocess . pack
           in let e = pack "good but not great" 
           in "preprocess"
        ~: TestList [ 
                      -- * identity
                      preprocess' "good but not great"        ~?= e
                      -- * case fold
                    , preprocess' "Good BUT not gREAt"        ~?= e
                      --  * white space 
                    , preprocess' "  good but    not great  "  ~?= e

                      -- * hyphen preserved
                    , preprocess' "good-great"                 ~?= pack "good-great"

                      -- * comma base case
                    , preprocess' "good, but     not   "      ~?= pack "good , but not"

                      -- * comma no space behind 
                    , preprocess' "good ,but     not   "      ~?= pack "good , but not"

                      -- * comma space in front and behind
                    , preprocess' "good , but     not   "     ~?= pack "good , but not"
                    ]
















