{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Test how parsers interact
-- | Author  : Xiao Ling
-- | Date    : 9/16/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module ParserInteractionTest where

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
              $ [ test1
                , test2
                , test3
                , test4
                , test5
                , test6
                , test7
                ]
    return ()

right :: String -> Either String Text
right = Right . pack

echo' :: Show a => Parser a -> Either String a
echo' = Left . echo

{-----------------------------------------------------------------------------
   A series of tests on raw parsers
   building up to project specific parsers

   (..)      parens denote optional string
   {..}      curly parense denotes required string
   {a} {b} == a b

   (foo|bar) means foo or bar or space
   {foo|bar} means foo or bar

  * (,) but not (a|an|the) *

  * (,) if not (a|an|the) *

  * (,) although|though not (a|an|the)*

  * (,) (and|or) even (a|an|the)*

  * (,) almost (a|an|the) * 

  * (,) (and|or) almost (a|an|the) *

  not only|just * (,) but *


------------------------------------------------------------------------------}

test1 :: Test
test1 =  let p1  = opt (word "a") <+> word "bar"
      in let p2  = opt (word "a") <+> star

      in let p3  = word "bar" <+> opt (word "a")
      in let p4  = star       <+> opt (word "a")

      in let o1  = right "(a) bar"
      in let o2  = right "(a) *"

      in "(a) bar,   (a) *,  bar (a),  * (a)"

      ~: TestList [ p1 <** pack "a bar" ~?= o1
                  , p1 <** pack " bar"  ~?= o1
                  , p1 <** pack "f bar" ~?= echo' p1

                  , p2 <** pack "a foo"   ~?= right "(a) foo"
                  , p2 <** pack "a bar"   ~?= right "(a) bar"
                  , p2 <** pack " bar"    ~?= right "(a) bar"
                  , p2 <** pack "bar"     ~?= right "(a) bar"
                  , p2 <** pack "the bar" ~?= right "(a) the"

                  , p3 <** pack "bar a"   ~?= right "bar (a)"
                  , p3 <** pack "bar "    ~?= right "bar (a)"
                  -- * note a space is consumed so (opt $ word "a") is satisfied
                  , p3 <** pack "bar the" ~?= right "bar (a)"

                  , p4 <** pack "ear a"   ~?= right "ear (a)"
                  , p4 <** pack "qar "    ~?= right "qar (a)"
                  -- * note a space is consumed so (opt $ word "a") is satisfied
                  , p4 <** pack "por the" ~?= right "por (a)"
                  ]


test2 :: Test
test2 =  let p1  = word "a" <||> word "an"
     in  let o1  = right "a|an"
     in "a|an"
     ~:  TestList [ p1 <** pack "a"   ~?= right "a"
                  , p1 <** pack "an"  ~?= right "an"
                  , p1 <** pack " "   ~?= echo' p1
                  , p1 <** pack "the" ~?= echo' p1
                  ]

test3 :: Test
test3 =  let p1  = opt $ word "a" <||> word "an"
     in "(a|an)"
     ~:  TestList [ p1 <** pack "a"   ~?= right "(a|an)"
                  , p1 <** pack "an"  ~?= right "(a|an)"
                  , p1 <** pack " "   ~?= right "(a|an)"
                  , p1 <** pack "the" ~?= right "(a|an)"
                  ]


test4 :: Test
test4 =  let q  = word "a" <||> word "an"

     in  let p1  = q <+> word "bar"
     in  let p2  = word "bar" <+> q
     in  let p3  = q <+> star
     in  let p4  = star <+> q


     in "a|an bar, a|an *, * a|an"
     ~:  TestList [ p1 <** pack "a  bar"   ~?= right "a bar"
                  , p1 <** pack "an  bar"  ~?= right "an bar"
                  , p1 <** pack "the bar"  ~?= echo' p1
                  , p1 <** pack "    bar"  ~?= echo' p1

                  , p2 <** pack "bar a"   ~?= right "bar a"
                  , p2 <** pack "bar an"  ~?= right "bar an"
                  , p2 <** pack "bar the" ~?= echo' p2
                  , p2 <** pack "bar    " ~?= echo' p2

                  , p3 <** pack "a  zar"   ~?= right "a zar"
                  , p3 <** pack "an  ear"  ~?= right "an ear"
                  , p3 <** pack "the ear"  ~?= echo' p3
                  , p3 <** pack "    ear"  ~?= echo' p3

                  , p4 <** pack "zar a"    ~?= right "zar a"
                  , p4 <** pack "ear an"   ~?= right "ear an"
                  , p4 <** pack "ear the"  ~?= echo' p4
                  , p4 <** pack "ear    "  ~?= echo' p4
                  ]


test5 :: Test
test5 =  let q  = opt $ word "a" <||> word "an"

     in  let p1  = q <+> word "bar"
     in  let p2  = word "bar" <+> q
     in  let p3  = q <+> star
     in  let p4  = star <+> q


     in "(a|an) bar, (a|an) *, * (a|an)"
     ~:  TestList [ p1 <** pack "a  bar"   ~?= right "(a|an) bar"
                  , p1 <** pack "an  bar"  ~?= right "(a|an) bar"
                  , p1 <** pack "    bar"  ~?= right "(a|an) bar"
                  , p1 <** pack "the bar"  ~?= echo' p1

                  , p2 <** pack "bar a"   ~?= right "bar (a|an)"
                  , p2 <** pack "bar an"  ~?= right "bar (a|an)"
                  , p2 <** pack "bar   "  ~?= right "bar (a|an)"
                  -- * note a space is consumed so parser q is satisfied
                  , p2 <** pack "bar the" ~?= right "bar (a|an)"

                  , p3 <** pack "a  zar"   ~?= right "(a|an) zar"
                  , p3 <** pack "an  ear"  ~?= right "(a|an) ear"
                  , p3 <** pack "    ear"  ~?= right "(a|an) ear"
                  , p3 <** pack "the ear"  ~?= right "(a|an) the"

                  , p4 <** pack "zar a"    ~?= right "zar (a|an)"
                  , p4 <** pack "ear an"   ~?= right "ear (a|an)"
                  , p4 <** pack "ear    "  ~?= right "ear (a|an)"
                  -- * note a space is consumed so parser q is satisfied
                  , p4 <** pack "ear the"  ~?= right "ear (a|an)"
                  ]



test6 :: Test
test6 = let p1 =   star            
              <+> (opt $ word ",") 
              <+> word "but" 
              <+> word "not"
              <+> opt (word "a" <||> word "an")
              <+> star
      in let p2 = word "good"      
              <+> (opt $ word ",") 
              <+> word "but" 
              <+> word "not"
              <+> opt (word "a" <||> word "an")
              <+> word "great"
      in "* (,) but not (a|an) *, good (,) but not (a|an) great"

      ~: TestList [ p1 <** pack "foo but not bar"      ~?= right "foo (,) but not (a|an) bar"     
                  , p1 <** pack "aaa but not bbb"      ~?= right "aaa (,) but not (a|an) bbb"  
                  , p1 <** pack "foo but not a bar"    ~?= right "foo (,) but not (a|an) bar"  
                  , p1 <** pack "foo but not an bar"   ~?= right "foo (,) but not (a|an) bar"  
                  , p1 <** pack "foo, but not bar"     ~?= right "foo (,) but not (a|an) bar"  
                  , p1 <** pack "foo, but not a bar"   ~?= right "foo (,) but not (a|an) bar"  
                  , p1 <** pack "foo, but not an bar"  ~?= right "foo (,) but not (a|an) bar"  

                  -- * this is a problem, how do you get around this??
                  , p1 <** pack "foo, but not not bar" ~?= right "foo (,) but not (a|an) not"

                  , p2 <** pack "good but not great"      ~?= right "good (,) but not (a|an) great"     
                  , p2 <** pack "good but not great"      ~?= right "good (,) but not (a|an) great"  
                  , p2 <** pack "good but not a great"    ~?= right "good (,) but not (a|an) great"  
                  , p2 <** pack "good but not an great"   ~?= right "good (,) but not (a|an) great"  
                  , p2 <** pack "good, but not great"     ~?= right "good (,) but not (a|an) great"  
                  , p2 <** pack "good, but not a great"   ~?= right "good (,) but not (a|an) great"  
                  , p2 <** pack "good, but not an great"  ~?= right "good (,) but not (a|an) great"  

                  -- * this is correct, this is the case you're worried about
                  -- * as long as this is fine your scores shouldn't suffer
                  , p2 <** pack "good, but not not great" ~?= echo' p2
                  ]



test7 :: Test
test7 = let p =  star             
             <+> (opt $ word ",")
             <+> (word "although" <||> word "though")
             <+> word "not"
             <+> (opt $ word "a" <||> word "an")
             <+> star

        in "* (,) although|though not (a|an) *"

        ~: TestList [ p <** pack "foo although not bar"     ~?= right "foo (,) although not (a|an) bar"
                    , p <** pack "foo though not bar"       ~?= right "foo (,) though not (a|an) bar"
                    , p <** pack "foo, although not bar"    ~?= right "foo (,) although not (a|an) bar"
                    , p <** pack "foo, though not bar"      ~?= right "foo (,) though not (a|an) bar"
                    , p <** pack "foo though not a bar"     ~?= right "foo (,) though not (a|an) bar"
                    , p <** pack "foo though not an bar"    ~?= right "foo (,) though not (a|an) bar"
                    , p <** pack "foo although not a bar"   ~?= right "foo (,) although not (a|an) bar"
                    , p <** pack "foo although not an bar"  ~?= right "foo (,) although not (a|an) bar"

                    -- * TODO: is this a problem?
                    , p <** pack "foo although not not bar" ~?= right "foo (,) although not (a|an) not"

                    , p <** pack "foo but not bar"          ~?= echo' p
                    ]



