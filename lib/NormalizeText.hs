-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Preprocess text before parsing
-- | Author  : Xiao Ling
-- | Date    : 8/19/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module NormalizeText (

    normalize

  ) where


import Control.Monad
import Data.Text (Text, pack, unpack, empty)
import qualified Data.Text as T
import Tokenize


{-----------------------------------------------------------------------------
    use all processing functions
------------------------------------------------------------------------------}

-- * preprocess text
-- *    (1) fold case
-- *    (2) fold space
-- *    (3) interspace punctations
normalize :: Text -> Text
normalize = T.toCaseFold . fromWords . toWords

{-----------------------------------------------------------------------------
    specific tasks
------------------------------------------------------------------------------}

fromWords :: [Text] -> Text
fromWords = T.intercalate (pack " ")


toWords :: Text -> [Text]
toWords =   concat 
          . fmap tokenizer
          . filter ((/=) empty) 
          . T.splitOn (pack " ") 


-- * (1) strip white space
-- * (2) preserve urls and web addresses
-- * (3) put space around punctations, 
-- * (4) preserve contractions such as he's, he'll
tokenizer :: Text -> [Text]
tokenizer =   run 
          $   whitespace
          >=> uris
          >=> punctuation 
          >=> contractions           

































