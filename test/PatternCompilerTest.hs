{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Test Pattern Compiler Test
-- | Author  : Xiao Ling
-- | Date    : 8/14/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module PatternCompilerTest where

import Test.HUnit
import Data.Text hiding (foldr)
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator

import Lib

{-----------------------------------------------------------------------------
   Run all tests
------------------------------------------------------------------------------}

main :: IO ()
main = do
    runTestTT . TestList 
              $ [ ttoken
                , ttokenizer
                , tcompile
                , tcompile'
                , tbutnot
                , talmost
                ]
    return ()

right :: String -> Either String Text
right = Right . pack

name' :: Show a => Parser a -> Either String a
name' = Left . name

{-----------------------------------------------------------------------------
   tokenizerr
------------------------------------------------------------------------------}

ttoken :: Test
ttoken = "token" 
       ~: TestList [ token "*"          ~?= Hole
                   , token ","          ~?= Comma
                   , token "(,)"        ~?= OptComma
                   , token "hello"      ~?= Word "hello"
                   , token "(hello)"    ~?= Word "hello"
                   , token "(and/or)"   ~?= Word "and" `Or` Word "or"
                   , token "(or/or/or)" ~?= Word "or" `Or` Word "or" `Or` Word "or"
                   , token "."          ~?= Word "."
                   ]


ttokenizer :: Test
ttokenizer = "tokenizer"
         ~: TestList [ tokenizer "* (,) but not *" 
                       ~?= [Hole,OptComma,Word "but",Word "not",Hole]
                     , tokenizer "* (,) (and/or) even *"
                       ~?= [Hole,OptComma,Or (Word "and") (Word "or"),Word "even",Hole]
                     , tokenizer "not only * but *"
                       ~?= [Word "not",Word "only",Hole,Word "but",Hole]
                      , tokenizer "not * (,) just *"
                       ~?= [Word "not", Hole, OptComma, Word "just", Hole]
                      , tokenizer "not *, but still *"
                       ~?= [Word "not",Hole,Comma,Word "but",Word "still",Hole]
                     ]



{-----------------------------------------------------------------------------
   Compiler test
------------------------------------------------------------------------------}

tcompile :: Test
tcompile =  let p1 = (compile "* or *"                ) (S "good") (S "great")
         in let p2 = (compile "or * or *"             ) (S "good") (S "great")
         in let p3 = (compile "* (and/or) *"          ) (S "good") (S "great")
         in let p4 = (compile "* or *"                ) Star Star
         in "compile"
         ~: TestList [ p1 <** pack "good or great" ~?= right "good or great"
                     , p1 <** pack "good bu great" ~?= name' p1

                     , p2 <** pack "or good or great" ~?= right "or good or great"
                     , p2 <** pack "or good bu great" ~?= name' p2

                     , p3 <** pack "good and great"   ~?= right "good and great"
                     , p3 <** pack "good or great"    ~?= right "good or great"
                     , p3 <** pack "good bu great"    ~?= name' p3

                     , p4 <** pack "foo or bar"       ~?= right "* or *"
                     , p4 <** pack "bar or foo"       ~?= right "* or *"
                     , p4 <** pack "bar and foo"      ~?= name' p4
         ]

tcompile' :: Test
tcompile' = let p = compile' "hello"
         in let q = compile' "*"
          in "compile"         
          ~: TestList [ p <** pack "hello" ~?= right "hello"
                      , p <** pack "world" ~?= name' p

                      -- * Note q never fails. It is the identity pattern
                      , q <** pack "*"     ~?= right ""
                      , q <** pack ""      ~?= right ""
                      , q <** pack "hel"   ~?= right ""
                      , q <** pack "13/"   ~?= right ""
                      ]


{-----------------------------------------------------------------------------
   Adhoc test compiler on specific phrases
------------------------------------------------------------------------------}

tbutnot :: Test
tbutnot =  let o1 = right "good (,) but not great"
        in let o2 = right "* (,) but not *"
        in let p1 = (compile "* (,) but not *") (S "good") (S "great")
        in let p2 = (compile "* (,) but not *") Star Star
        in "but not"
        ~: TestList [ p1 <** (pack "good but not great"        ) ~?= o1
                    , p1 <** (pack "good, but not great"       ) ~?= o1
                    , p1 <** (pack "good ,  but not great"     ) ~?= o1
                    , p1 <** (pack "good but not great comment") ~?= o1

                    -- * TODO: This test fails 
                    , p1 <** (pack "good ,but not great "      ) ~?= o1

                    , p1 <** (pack "foo but not bar"           ) ~?= name' p1
                    , p1 <** (pack "foo,  but not bar"         ) ~?= name' p1

                    , p2 <** (pack "foo but not bar"           ) ~?= o2
                    , p2<** (pack "foo,  but not bar"         )  ~?= o2
                    ]

talmost :: Test
talmost =  let o1 = right "good (,) and almost great"
        in let o2 = right "good (,) or almost great"
        in let o3 = right "* (,) and almost *"
        in let o4 = right "* (,) or almost *"

        in let p1 = (compile "* (,) (and/or) almost *") (S "good") (S "great")
        in let p2 = (compile "* (,) (and/or) almost *") Star Star
        in "* (,) (and/or) almost *"

        ~: TestList [ p1 <** (pack "good and almost great"     ) ~?= o1
                    , p1 <** (pack "good, and almost great"    ) ~?= o1
                    , p1 <** (pack "good or almost great"      ) ~?= o2
                    , p1 <** (pack "good, or almost great"     ) ~?= o2
                    , p1 <** (pack "good but almost great"     ) ~?= name' p1

                    , p2 <** (pack "foo and almost bar"        ) ~?= o3
                    , p2 <** (pack "foo, or almost bar"        ) ~?= o4
                    , p2 <** (pack "foo, ord almost bar"       ) ~?= name' p2
                    ]








