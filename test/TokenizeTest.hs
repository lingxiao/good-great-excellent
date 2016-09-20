-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Test preprocess routines
-- | Author  : Xiao Ling
-- | Date    : 8/19/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module TokenizeTest where

import Test.HUnit
import Data.Text 

import Tokenize

{-----------------------------------------------------------------------------
   Run all tests
------------------------------------------------------------------------------}

main :: IO ()
main = do
    runTestTT . TestList 
              $ [ ttokenize
                ]
    return ()

{-----------------------------------------------------------------------------
    Tasks
------------------------------------------------------------------------------}

ttokenize :: Test
ttokenize = let foo = pack "foo"
        in "tokenize" 
        ~: TestList [ -- * base case
                      tokenize  (pack "foo")   ~?= [foo]
                      -- * initial and final punctation
                    , tokenize (pack ",foo"  ) ~?= [pack ",", foo  ]
                    , tokenize (pack "foo,"  ) ~?= [foo, pack ","  ]
                    , tokenize (pack ", foo" ) ~?= [pack ",", foo  ]
                    , tokenize (pack "foo , ") ~?= [foo, pack ","  ]

                    -- * hypen
                    , tokenize (pack "foo-bar") ~?= [pack "foo-bar"]

                    -- * apostrophe preservation
                    , tokenize (pack "her's")    ~?= [pack "her's"   ] 
                    , tokenize (pack "would've") ~?= [pack "would've"] 

                    -- * slash preservation
                    , tokenize (pack "foo/bar") ~?= [pack "foo/bar"]

                    -- * url/email perservation
                    , tokenize (pack "http://example.org") ~?= [pack "http://example.org"]
                    , tokenize (pack "email@example.com" ) ~?= [pack "email@example.com"]

                    -- * ad hoc tests
                    , tokenize (pack "This shouldn't happen.")
                      ~?= (pack <$> ["This","shouldn't","happen","."])
                    , tokenize (pack "Some 'quoted' stuff")
                      ~?= (pack <$> ["Some","'","quoted","'","stuff"])
                    , tokenize (pack "ReferenceError #1065 broke my debugger!")
                      ~?= (pack <$> ["ReferenceError","#","1065","broke","my","debugger","!"])
                    , tokenize (pack "I would've gone.")
                      ~?= (pack <$> ["I","would've","gone","."])
                    , tokenize (pack "They've been there.")
                      ~?= (pack <$> ["They've","been","there","."])
                    ]

















                    
