{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Compile string patterns into Attoparsec parsers
-- | Author  : Xiao Ling
-- | Date    : 9/7/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module PatternCompiler (

    Pattern
  , Token  (..)
  , PInput (S, Star)

  , compile
  , compile'
  , token
  , tokenizer

  , (<**)
  , name

  ) where

import Data.List.Split 
import Data.Attoparsec.Text
import Data.Text (Text, unpack, pack)

import Control.Applicative
import Parsers

{-----------------------------------------------------------------------------
    Tokenization and Parsing
------------------------------------------------------------------------------}

data Token = Word String 
           | Hole 
           | Slash
           | Comma
           | OptComma
           | Or Token Token
           deriving (Eq, Show)

-- * Input into Pattern
data PInput = S String      -- * Pattern match some Text
           | Star           -- * Pattern match any string of alphabetical symbol 
           | Nil            -- * trivial PInput so that 
                            -- * compile "fillod" Nil Nil = word "fillod"
           deriving (Eq, Show) 


-- * A pattern of form `R * *` relates two 
-- * strings `*` with some relation `R`
type Pattern = PInput -> PInput -> Parser Text


{-----------------------------------------------------------------------------
    Top level function
------------------------------------------------------------------------------}

-- * given an expression, output pattern
compile :: String -> Pattern
compile = compiler . tokenizer

-- * compile a string into a parser, the pattern described by the string 
-- * does not have any `Hole`s in it
-- * Note: compile' xs = word xs 
-- *       so that compile' "*" = pzero, it is the identity pattern
compile' :: String -> Parser Text
compile' xs = compile xs Nil Nil

{-----------------------------------------------------------------------------
    Tokenizer
------------------------------------------------------------------------------}

-- * maps a string to some set of tokens
tokenizer :: String -> [Token]
tokenizer = fmap token . concat . fmap recoverComma . splitOn " "

-- * `token`ize a string
-- * note if `token` sees a string `xs` it does not recognize,
-- * it just outputs a `Word xs`
-- * TODO: quick and dirty here, consider doing something real
token :: String -> Token
token "*"    = Hole
token ","    = Comma
token "(,)"  = OptComma
token xs     = case splitOn "/" xs of
  y:ys  -> Word (stripParens y) `catOr` ys
  _     -> Word $ stripParens xs

catOr :: Token -> [String] -> Token
catOr t = foldr (\x ts -> ts `Or` Word (stripParens x)) t

{-----------------------------------------------------------------------------
    Compiler
------------------------------------------------------------------------------}

-- * compile a list of tokens into binary pattern `BinPattern`
-- * note by construction this function fails with identity parser under (<+>)
compiler :: [Token] -> Pattern
compiler ts = \u v -> [u,v] `fill` ts

-- * Given stack of strings `w:ws` as eventual input to the 
-- * binary pattern, and list of tokens `t:ts`, 
-- * create a `Parser Text` by `fill`ing in all the `Hole`s
-- * If the stack is empty before tokens are, then all remaining
-- * `Hole` tokens are mapped to parser `star`
fill :: [PInput] -> [Token] -> Parser Text
fill (i:is) (Hole:ts) = case i of
  S w   -> word w <+> fill is ts 
  Star  -> star   <+> fill is ts
  Nil   -> pzero  <+> fill is ts
fill _       []       = pzero  
fill is      (t:ts)   = toP t <+> fill is ts


-- * convert token to parser, note `Hole` is sent to `star` which accept
-- * any string of alphabetical symbols
toP :: Token -> Parser Text
toP (Word xs)  = word xs
toP Hole       = star
toP Slash      = pzero
toP OptComma   = comma
toP Comma      = word ","
toP (Or t1 t2) = toP t1 <|> toP t2


{-----------------------------------------------------------------------------
    Utils
------------------------------------------------------------------------------}

recoverComma :: String -> [String]
recoverComma []                  = []
recoverComma xs | last xs == ',' = [init xs, ","]
                | otherwise      = [xs]

-- * aggressively remove all occurences of "(" and/or ")" in a string
stripParens :: String -> String
stripParens = foldr strip mempty
    where strip c cs | c == '('   = cs
                     | c == ')'   = cs
                     | otherwise  = c:cs
















