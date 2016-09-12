{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Test library of attoparsec parsers
-- | Author  : Xiao Ling
-- | Date    : 8/14/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module ParsersTest where

import Test.HUnit
import Data.Text hiding (foldr)
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator

import Parsers


{-----------------------------------------------------------------------------
   Run all tests
------------------------------------------------------------------------------}

main :: IO ()
main = do
    runTestTT . TestList 
              $ [ talgebra
                , tspaces
                , tspaces1
                , tNotAlphaDigitSpace
                , teow
                , tword
                , tanyWord
                , tcomma
                ]
    return ()



right :: String -> Either String Text
right = Right . pack

name' :: Show a => Parser a -> Either String a
name' = Left . name

{-----------------------------------------------------------------------------
    Basic parsers
------------------------------------------------------------------------------}

talgebra :: Test
talgebra = let p    = word "hello"
        in let q    = word "world"
        in let r    = word "stuff"
        in let pq   = p <+> q
        in let pqr  = p <+> q <+> r
        in let pqr' = p <+> (q <+> r)
        in "talgebra"
        ~: TestList [ (p <+> q) <** (pack "hello world") 
                      ~?= right "hello world"
              
                      -- * commutative property w/ correct inputs
                    , pqr      <** (pack "hello world stuff") 
                      ~?= pqr' <** (pack "hello world stuff")

                     -- * commutative property w/ failed inputs
                    , pqr    <** (pack "foo bar baz") 
                    ~?= pqr' <** (pack "foo bar baz")

                    -- * identity w/ correct inputs
                    , pq <+> pzero  <** (pack "hello world") 
                    ~?= pq          <** (pack "hello world")

                    -- * identity w/ failed inputs
                    , pq <+> pzero <** (pack "foo bar")     
                    ~?= pq         <** (pack "foo bar")
                    ]

tword :: Test
tword =  let p   = word  "hello"
      in let o   = right "hello"
      in let err = name' p 
      in "word"                    
      ~: TestList [ p <** (pack "hello"  ) ~?= o
                  , p <** (pack "hello!" ) ~?= o
                  , p <** (pack "  hello") ~?= o
                  , p <** (pack "hello " ) ~?= o
                  , p <** (pack "foo"    ) ~?= err
                  , p <** (pack "hello1" ) ~?= err
                  , p <** (pack "helloo" ) ~?= err

                  , p <** (pack "hello..."    ) ~?= o
                  , p <** (pack "hello.f"     ) ~?= err
                  , p <** (pack "hello......f") ~?= err
                  , p <** (pack "hello-/hello") ~?= err
                  , p <** (pack "hello'ol"    ) ~?= err
                  , p <** (pack "hello.com"   ) ~?= err
                  , p <** (pack "hello.kw.net") ~?= err
                  ]


tanyWord :: Test
tanyWord = let o   = right "hello" 
        in let err = name' anyWord
        in "anyWord"
        ~: TestList [ anyWord <** (pack "hello"      ) ~?= o
                    , anyWord <** (pack "hello world") ~?= o
                    , anyWord <** (pack "hello..."   ) ~?= o
                    , anyWord <** (pack "h"          ) ~?= right "h"    

                    , anyWord <** (pack "!"          ) ~?= err
                    , anyWord <** (pack "9"          ) ~?= err
                    , anyWord <** (pack ""           ) ~?= err
                    , anyWord <** (pack "    "       ) ~?= err

                    , anyWord <** (pack "good.f"     ) ~?= err
                    , anyWord <** (pack "good.....f" ) ~?= err
                    , anyWord <** (pack "good-/good" ) ~?= err
                    , anyWord <** (pack "good'ol"    ) ~?= err
                    , anyWord <** (pack "good.com"   ) ~?= err
                    ]


tcomma :: Test
tcomma = let o = right "(,)"
       in "comma'" 
       ~: TestList [ comma <** (pack ","  ) ~?= o
                   , comma <** (pack "  ,") ~?= o
                   , comma <** (pack " ")   ~?= o
                   , comma <** (pack "h"  ) ~?= name' comma
                   ]


tspaces :: Test
tspaces = "spaces" 
        ~: TestList [ spaces <** (pack ""   ) ~?= right " "
                    , spaces <** (pack "   ") ~?= right " "
                    , spaces <** (pack "hel") ~?= right " "
                    ]



tspaces1 :: Test
tspaces1 = "spaces1" 
        ~: TestList [ spaces1<**  (pack ""   ) ~?= name' spaces1
                    , spaces1<**  (pack "hel") ~?= name' spaces1
                    , spaces1<**  (pack "   ") ~?= right " "
                    ]



tNotAlphaDigitSpace :: Test
tNotAlphaDigitSpace = 
    let err = name' notAlphaDigitSpace
    in     "notAlphaDigitSpace"
     ~: TestList [ notAlphaDigitSpace <** (pack ".") ~?= Right '.'
                 , notAlphaDigitSpace <** (pack " ") ~?= err
                 , notAlphaDigitSpace <** (pack "1") ~?= err
                 , notAlphaDigitSpace <** (pack "h") ~?= err
     ]


teow :: Test
teow = "eow"
    ~: TestList [ eow <** (pack "..."  ) ~?= right "..."
                , eow <** (pack ".. "  ) ~?= right ".."
                , eow <** (pack ".. hi") ~?= right ".."

                , eow <** (pack "..1"  ) ~?= name' eow
                , eow <** (pack ".h" )   ~?= name' eow
                , eow <** (pack "..h")   ~?= name' eow
    ]





