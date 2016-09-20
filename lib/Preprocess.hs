-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Preprocess text before parsing
-- | Author  : Xiao Ling
-- | Date    : 8/19/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Preprocess where

import Data.Text (Text, pack, unpack, empty)
import qualified Data.Text as T
import Tokenize


import Data.Char
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Parsers


{-----------------------------------------------------------------------------
    use all processing functions
------------------------------------------------------------------------------}

-- * preprocess text
-- *    (1) fold case
-- *    (2) fold space
-- *    (3) interspace punctations
preprocess :: Text -> Text
preprocess = T.toCaseFold . fromWords . toWords

{-----------------------------------------------------------------------------
    specific tasks
------------------------------------------------------------------------------}

fromWords :: [Text] -> Text
fromWords = T.intercalate (pack " ")

-- * problem: need to determine what to do w/ punctuations
-- * 1. proper tokenizaton
-- * 2. normalize ngrams
-- * 3. now run grep over the data set
toWords :: Text -> [Text]
toWords =   concat 
          . fmap tokenize 
          . filter ((/=) empty) 
          . T.splitOn (pack " ") 







































