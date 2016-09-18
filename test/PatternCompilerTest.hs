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
                , ttokenize
                , tcompile
                , tcompile'

                , tbutnot
                , talmost
                , talthoughNot
                ]
    return ()

right :: String -> Either String Text
right = Right . pack

echo' :: Show a => Parser a -> Either String a
echo' = Left . echo

{-----------------------------------------------------------------------------
   tokenizer
------------------------------------------------------------------------------}

ttoken :: Test
ttoken = "token" 
       ~: TestList [ token "*"          ~?= Hole
                   , token ","          ~?= Word ","
                   , token "(,)"        ~?= Opt (Word ",")
                   , token "hello"      ~?= Word "hello"
                   , token "(hello)"    ~?= Opt (Word "hello")
                   , token "and|or"     ~?= Word "and" `Or` Word "or"
                   , token "(and|or)"   ~?= Opt (Word "and" `Or` Word "or")
                   , token "or|or|or"   ~?= Word "or" `Or` Word "or" `Or` Word "or"

                   ]



ttokenize :: Test
ttokenize = "tokenize"
         ~: TestList [ tokenize "* foo"   ~?= [Hole, Word "foo"]
                     , tokenize "foo *"   ~?= [Word "foo", Hole]
                     , tokenize ", foo"   ~?= [Word ",", Word "foo"]
                     , tokenize "foo,"    ~?= [Word "foo", Word ","]
                     , tokenize "(,) foo" ~?= [Opt (Word ","), Word "foo"]
                     , tokenize "foo (,)" ~?= [Word "foo", Opt (Word ",")]

                     , tokenize "foo (,) bar|baz" 
                     ~?= [Word "foo", Opt (Word ","), Or (Word "bar") (Word "baz")]
                     , tokenize "foo (,) bar|baz *" 
                     ~?= [Word "foo",Opt (Word ","),Or (Word "bar") (Word "baz"),Hole]
                     , tokenize "foo (,) bar|baz (a|an|the) *" 
                     ~?= [Word "foo",Opt (Word ","),Or (Word "bar") (Word "baz"),Opt (Or (Or (Word "a") (Word "an")) (Word "the")),Hole]
                     , tokenize "foo, bar|baz (a|an|the) *"  
                     ~?= [Word "foo",Word ",",Or (Word "bar") (Word "baz"),Opt (Or (Or (Word "a") (Word "an")) (Word "the")),Hole]

                     , tokenize "* (,) but not *" 
                     ~?= [Hole,Opt (Word ","),Word "but",Word "not",Hole]
                     , tokenize "* (,) (and|or) even *"
                     ~?= [Hole,Opt (Word ","),Opt (Or (Word "and") (Word "or")),Word "even",Hole]
                      , tokenize "not *, but still *"
                     ~?= [Word "not",Hole,Word ",",Word "but",Word "still",Hole]
                     ]



{-----------------------------------------------------------------------------
   Compiler test
------------------------------------------------------------------------------}

tcompile :: Test
tcompile =  let p1 = (compile "* or *"                ) (S "good") (S "great")
         in let p2 = (compile "or * or *"             ) (S "good") (S "great")
         in let p3 = (compile "* (and|or) *"          ) (S "good") (S "great")
         in let p4 = (compile "* or *"                ) Star Star
         in "compile"
         ~: TestList [ p1 <** pack "good or great" ~?= right "good or great"
                     , p1 <** pack "good bu great" ~?= echo' p1

                     , p2 <** pack "or good or great" ~?= right "or good or great"
                     , p2 <** pack "or good bu great" ~?= echo' p2

                     , p3 <** pack "good and great"   ~?= right "good (and|or) great"
                     , p3 <** pack "good or great"    ~?= right "good (and|or) great"
                     , p3 <** pack "good bu great"    ~?= echo' p3

                     , p4 <** pack "foo or bar"       ~?= right "foo or bar"
                     , p4 <** pack "bar or foo"       ~?= right "bar or foo"
                     , p4 <** pack "bar and foo"      ~?= echo' p4
         ]

tcompile' :: Test
tcompile' = let p = compile' "hello"
         in let q = compile' "*"
          in "compile"         
          ~: TestList [ p <** pack "hello" ~?= right "hello"
                      , p <** pack "world" ~?= echo' p

                      -- * Note q never fails. It is the identity pattern
                      , q <** pack "*"     ~?= right ""
                      , q <** pack ""      ~?= right ""
                      , q <** pack "hel"   ~?= right ""
                      , q <** pack "13/"   ~?= right ""
                      ]


{-----------------------------------------------------------------------------
   Compiling project specific parsers
------------------------------------------------------------------------------}


talthoughNot :: Test
talthoughNot = let p = compile "* (,) though|although not (a|an|the) *" Star Star
            in let q = compile "* (,) though|although not (a|an|the) *" (S "good") (S "great")
            in "* (,) though|although not (a|an|the) *  and @ good, great"

            ~: TestList [ -- * no comma, no article
                          p <** pack "foo though not bar"       ~?= right "foo (,) though not (a|an|the) bar"
                        , p <** pack "foo although not bar"     ~?= right "foo (,) although not (a|an|the) bar"

                          -- * comma
                        , p <** pack "foo, though not bar"      ~?= right "foo (,) though not (a|an|the) bar"
                        , p <** pack "foo, although not bar"    ~?= right "foo (,) although not (a|an|the) bar"

                          -- * article
                        , p <** pack "foo although not a bar"   ~?= right "foo (,) although not (a|an|the) bar"
                        , p <** pack "foo although not an bar"  ~?= right "foo (,) although not (a|an|the) bar"
                        , p <** pack "foo although not the bar" ~?= right "foo (,) although not (a|an|the) bar"

                          -- * wrong sentence that passes test due to option
                        , p <** pack "foo although not not bar" ~?= right "foo (,) although not (a|an|the) not"

                          -- * wrong non-optional word
                        , p <** pack "foo but not foo bar"      ~?= echo' p

                          -- * same variation as above but with concrete words
                        , q <** pack "good though not great"       ~?= right "good (,) though not (a|an|the) great"
                        , q <** pack "good, though not great"      ~?= right "good (,) though not (a|an|the) great"
                        , q <** pack "good although not great"     ~?= right "good (,) although not (a|an|the) great"
                        , q <** pack "good although not a great"   ~?= right "good (,) although not (a|an|the) great"
                        , q <** pack "good although not an great"  ~?= right "good (,) although not (a|an|the) great"
                        , q <** pack "good although not the great" ~?= right "good (,) although not (a|an|the) great"

                        -- * this case fails, which is good because it's what you want to not overcount
                        , q <** pack "good although not not great" ~?=  echo' q
                        ]

tbutnot :: Test
tbutnot =  let p1 = (compile "* (,) but not *") (S "good") (S "great")
        in let p2 = (compile "* (,) but not *") Star Star
        in "but not"
        ~: TestList [ p1 <** (pack "good but not great"        ) ~?= right "good (,) but not great"        
                    , p1 <** (pack "good, but not great"       ) ~?= right "good (,) but not great"       
                    , p1 <** (pack "good ,  but not great"     ) ~?= right "good (,) but not great"     
                    , p1 <** (pack "good but not great comment") ~?= right "good (,) but not great"

                    -- * TODO: This test fails 
                    --, p1 <** (pack "good ,but not great "      ) ~?= o1

                    , p1 <** (pack "foo but not bar"           ) ~?= echo' p1
                    , p1 <** (pack "foo,  but not bar"         ) ~?= echo' p1

                    , p2 <** (pack "foo but not bar"           ) ~?= right "foo (,) but not bar"
                    , p2<** (pack "foo,  but not bar"         )  ~?= right "foo (,) but not bar"
                    ]

talmost :: Test
talmost =  let p1 = (compile "* (,) and|or almost *") (S "good") (S "great")
        in let p2 = (compile "* (,) and|or almost *") Star Star
        in "* (,) (and|or) almost *"

        ~: TestList [ p1 <** (pack "good and almost great"     ) ~?= right "good (,) and almost great"
                    , p1 <** (pack "good, and almost great"    ) ~?= right "good (,) and almost great"
                    , p1 <** (pack "good or almost great"      ) ~?= right "good (,) or almost great"
                    , p1 <** (pack "good, or almost great"     ) ~?= right "good (,) or almost great"
                    , p1 <** (pack "good but almost great"     ) ~?= echo' p1

                    , p2 <** (pack "foo and almost bar"        ) ~?= right "foo (,) and almost bar"
                    , p2 <** (pack "foo, or almost bar"        ) ~?= right "foo (,) or almost bar"
                    , p2 <** (pack "foo, ord almost bar"       ) ~?= echo' p2
                    ]





