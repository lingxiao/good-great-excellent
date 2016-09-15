{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : All functions used to find score
-- | Author  : Xiao Ling
-- | Date    : 9/14/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Score (
    sumCnt
  , cnt

  , w1
  , s1
  , w2
  , s2

  ) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text (Text, unpack, pack)

import Lib
import Core
import Query

{-----------------------------------------------------------------------------
  Score 
------------------------------------------------------------------------------}

w1 :: String -> String -> ReaderT Config IO Output
w1 a1 a2 = do
  wss <- weakStrong <$> ask 
  sumCnt $ (\ws -> compile ws (S a1) (S a2)) <$> wss

s1 :: String -> String -> ReaderT Config IO Output
s1 a1 a2 = do
  sws <- strongWeak <$> ask 
  sumCnt $ (\sw -> compile sw (S a1) (S a2)) <$> sws

w2 :: String -> String -> ReaderT Config IO Output
w2 = flip w1

s2 :: String -> String -> ReaderT Config IO Output
s2 = flip s1

p1 :: ReaderT Config IO Output
p1 = undefined

{-----------------------------------------------------------------------------
  Count
------------------------------------------------------------------------------}

-- * sum the results of multiple `count`s 
-- * and sum their counts, list all results
sumCnt :: [Parser Text] -> ReaderT Config IO Output
sumCnt ps = do
  rrs <- mapM cnt ps
  let ns = fst <$> rrs
  let rs = snd <$> rrs
  return (sum ns, concat rs)

cnt :: Parser Text -> ReaderT Config IO Output
cnt p = do
  d  <- corpus <$> ask
  queryAll d p
  
