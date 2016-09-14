{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : a library of parser utility functions and parser combinators
-- | Author  : Xiao Ling
-- | Date    : 8/14/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Parsers (

    (<**)
  , (<**?)
  , (<+>)
  , pzero
  , name

  , word
  , anyWord
  , spaces
  , spaces1
  , eow
  , comma
  , star
  , notAlphaDigitSpace

  ) where

import Prelude hiding   (concat, takeWhile)
import Control.Monad
import Control.Applicative hiding (empty)

import Data.Char
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text hiding (head, foldr, takeWhile)

{-----------------------------------------------------------------------------
   Run parser, name parser, and parser algebra
------------------------------------------------------------------------------}

-- * identity parser under (<+>)
pzero :: Parser Text
pzero =  (return . pack $ "")
     <?> "pzero"

-- * Parser combination, combines two parsers `p` and `q`
-- * by concating their outputs and joining names with "_"
-- * Note parsers form non-associative, commutiative algebra 
-- * under (<+>) where pzero is identity
-- * note mzero and mempty are *NOT* identities, 
-- * they always fail
infixr 9 <+>
(<+>) :: Parser Text -> Parser Text -> Parser Text
p <+> q = (\u v -> if unpack v == "" then u 
                   else concat [u, pack " ", v])
      <$> p <*> q
      <?> p `addName` q

addName :: Parser Text -> Parser Text -> String
addName p q = case (name p, name q) of
  ("", "") -> ""
  ("", _ ) -> name q
  (_, "" ) -> name p
  (_, _  ) -> name p ++ "-" ++ name q


name :: Show a => Parser a -> String
name p = case (p >> mzero) <** empty of
  Left n  -> n

-- * parse text `t` using parser `p`
infixr 8 <**
(<**) :: Parser a -> Text -> Either String a
p <** t = case parse p t of
    Done _ r   -> Right r
    Fail _ [] _ -> Left ""
    Fail _ m _  -> Left . head $ m
    Partial c -> case c "" of
        Done _ r    -> Right r
        Fail _ [] _ -> Left ""
        Fail _ m _  -> Left . head $ m

-- * check if parser `p` recognized text `t`
infixr 8 <**?
(<**?) :: Parser a -> Text -> Bool
p <**? t = case p <** t of
  Right _ -> True
  _       -> False


{-----------------------------------------------------------------------------
   Basic parsers
------------------------------------------------------------------------------}

word :: String -> Parser Text
word w =  spaces *> string (pack w) <* eow
      <?> w

-- * parse any alphabetical string with 0 or more spaces infront
anyWord :: Parser Text
anyWord = spaces *> takeWhile1 isAlpha <* eow
      <?> "*"

-- * next char could either be a comma or 
-- * one or more spacesW
comma :: Parser Text
comma = output "(,)" <$> (word "," <|> spaces1)

-- * parses any word and outputs "*"
star :: Parser Text
star = output "*" <$> anyWord


-- * parse zero or more spaces and ouput one space
spaces :: Parser Text
spaces = output " " <$> many' space
      <?> " "

-- * parse one or more spaces and ouput one space
spaces1 :: Parser Text
spaces1 = output " " <$> many1' space
      <?> " "

-- * look ahead all nonAlpha symbols and end in space or eoi
eow :: Parser Text
eow =   (lookAhead 
    $   pack 
    <$> many' notAlphaDigitSpace <* (const () <$> space <|> endOfInput))

{-----------------------------------------------------------------------------
  Utility
------------------------------------------------------------------------------}

output :: String -> a -> Text
output t = const . pack $ t

notAlphaNum :: Parser Char
notAlphaNum = satisfy (not . isAlphaNum)
          <?> "not_alpha_num"

-- * things not allowed: alphabets, numbers, space
notAlphaDigitSpace :: Parser Char
notAlphaDigitSpace = satisfy (\c 
                   -> not (isDigit c || isAlpha c || isSpace c))
                  <?> "not_alpha_digit_space"


