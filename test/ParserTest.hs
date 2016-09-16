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
              $ [ tcombinators
                , tspaces
                , tspaces1
                , tNotAlphaDigitSpace
                , teow
                , tword
                , tanyWord
                , tmaybeWord
                , tcomma
                ]
    return ()


right :: String -> Either String Text
right = Right . pack

echo' :: Show a => Parser a -> Either String a
echo' = Left . echo

{-----------------------------------------------------------------------------
    Basic parsers
------------------------------------------------------------------------------}

tcombinators :: Test
tcombinators = let p    = word "hello"
        in let q    = word "world"
        in let r    = word "stuff"
        in let pq   = p <+> q
        in let pqr  = p <+> q <+> r
        in let pqr' = p <+> (q <+> r)
        in let p_q  = p <||> q
        in "combinators"
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

                    -- * parser or
                    , p_q <** (pack "hello") ~?= right "hello"
                    , p_q <** (pack "world") ~?= right "world"
                    , p_q <** (pack "foooo") ~?= echo' p_q

                    ]


tword :: Test
tword =  let p   = word  "hello"
      in let o   = right "hello"
      in let err = echo' p 
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
        in let err = echo' anyWord
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


tmaybeWord :: Test
tmaybeWord = let p   = opt . word $ "foo"
          in let o   = right     "(foo)"
          in let err = echo'     p
          in "maybeWord"
          ~: TestList [ p <** (pack "foo"         ) ~?= o
                      , p <** (pack "foo word"    ) ~?= o
                      , p <** (pack "  foo   word") ~?= o
                      , p <** (pack " word"       ) ~?= o

                      , p <** (pack "word"        ) ~?= err
                      , p <** (pack "fooo"        ) ~?= err
                      ]



tcomma :: Test
tcomma = let comma = opt . word $ ","
       in let o = right "(,)"
       in "comma'" 
       ~: TestList [ comma <** (pack ","  ) ~?= o
                   , comma <** (pack "  ,") ~?= o
                   , comma <** (pack " ")   ~?= o
                   , comma <** (pack "h"  ) ~?= echo' comma
                   ]


tspaces :: Test
tspaces = "spaces" 
        ~: TestList [ spaces <** (pack ""   ) ~?= right " "
                    , spaces <** (pack "   ") ~?= right " "
                    , spaces <** (pack "hel") ~?= right " "
                    ]



tspaces1 :: Test
tspaces1 = "spaces1" 
        ~: TestList [ spaces1<**  (pack ""   ) ~?= echo' spaces1
                    , spaces1<**  (pack "hel") ~?= echo' spaces1
                    , spaces1<**  (pack "   ") ~?= right " "
                    ]



tNotAlphaDigitSpace :: Test
tNotAlphaDigitSpace = 
    let err = echo' notAlphaDigitSpace
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

                , eow <** (pack "..1"  ) ~?= echo' eow
                , eow <** (pack ".h" )   ~?= echo' eow
                , eow <** (pack "..h")   ~?= echo' eow
    ]





