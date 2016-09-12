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
              $ [ tfoldstrip
                ]
    return ()

{-----------------------------------------------------------------------------
    Tasks
------------------------------------------------------------------------------}

tfoldstrip :: Test
tfoldstrip = let e           = pack "good but not great" 
           in let foldStrip' = foldStrip . pack
           in "foldStrip" 
        ~: TestList [ foldStrip' "good but not great"        ~?= e
                    , foldStrip' "Good BUT not gREAt"        ~?= e
                    , foldStrip' "   good but not great   "  ~?= e
                    ]
