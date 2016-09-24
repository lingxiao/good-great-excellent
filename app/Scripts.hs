-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A collection of scripts to be run in main,
-- |           with no gaurantee that they will work
-- | Author  : Xiao Ling
-- | Date    : 9/13/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 
module Scripts (

    count_phrase
  , count_word
  , main_prep_data
  , main_normalize
  , main_normalize_words

  , main_split_by_pattern
  , main_pattern_freq

  , pattern_freq
  , total_freq

  ) where


import System.IO
import System.Directory
import System.FilePath.Posix
import qualified System.IO as S

import Data.Time.Clock
import Data.Text (Text, unpack, pack, splitOn)
import Data.Attoparsec.Text 
import qualified Data.Conduit.Text as CT

import Core
import Src
import Lib 


{-----------------------------------------------------------------------------
  Count and save for occurences of specific patterns
------------------------------------------------------------------------------}

count_phrase :: [PatternExpr] 
           -> FilePath 
           -> String
           -> (String,String) 
           -> IO (Integer,[Output])
count_phrase ps fin dirname (u,v) = do
  root     <- makeDirUnder "good-great-excellent" dirname
  let pats = (\p -> compile p (S u) (S v)) <$> ps
  os       <- mapM (\p -> query p fin) pats
  let tot  = foldr (+) 0 $ fst <$> os
  let rs   = zip ps os
  let path = root ++ u ++ "-" ++ v ++ ".txt"
  save_queries path tot rs
  return (tot,os)


count_word :: FilePath 
           -> String
           -> [PatternExpr] 
           -> IO (Integer, [Output])
count_word inpath name ws = do
  
  root     <- makeDirUnder "good-great-excellent" "out"

  let ps   = compile' <$> ws

  -- os       <- flip query_at inpath `mapM` ps
--   let tot  = foldr (+) 0 $ fst <$> os
--  let rs   = zip ws os
  let path = root ++ name ++ ".txt"
  save_queries path 0 []
  return (0,[])
--  save_queries path tot rs
--  return (tot,os)


save_queries :: DirectoryPath 
            -> Integer 
            -> [(String, Output)]
            -> IO ()
save_queries path tot rs = do

  time <- show <$> getCurrentTime
  h    <- S.openFile path S.WriteMode

  S.hPutStrLn h time
  S.hPutStrLn h mark
  S.hPutStrLn h $ "cumulative occurrences : " ++ show tot
  S.hPutStrLn h mark
  S.hClose h
        where mark  = foldr (++) mempty $ (const "-") <$> [1..50] 

{-
  mapM (\(patt,(n,xs)) -> do
    S.hPutStrLn h mark
    S.hPutStrLn h patt
    S.hPutStrLn h $ "total: " ++ show n
    mapM (\(t,m) -> S.hPutStrLn h 
                 $  unpack t ++ " " ++ unpack m) xs
    ) rs
  return ()
-}



{-----------------------------------------------------------------------------
  Prep data
------------------------------------------------------------------------------}

main_prep_data :: DirectoryPath -> PatternExpr -> IO ()
main_prep_data root p = do

    -- * count raw frequencies
    let path = root ++ p ++ ".txt"
    n <- raw_freq path
    print $ "raw frequency: " ++ show n
    print "------------------------------------------------"

    -- * filter by pattern
    main_split_by_pattern root [compile p Star Star]

  -- * count filtered out and filtered in to make sure:
  -- *   count (out ++ in) = count(out) + count(in) = count (raw)
    n <- total_freq $ root ++ "/out/" ++ p ++ ".txt"
    m <- total_freq $ root ++ "/out/leftover-" ++ p ++ ".txt"
    print p
    print $ "conform    : " ++ show n
    print $ "not-conform: " ++ show m
    print $ "total      : " ++ show (n + m)
    print "------------------------------------------------"

{-----------------------------------------------------------------------------
  normalize raw ngrams
------------------------------------------------------------------------------}

main_normalize :: DirectoryPath -> String -> Int -> IO ()
main_normalize root filename n = do
  let intr = root ++ filename ++ "/"
  let outr = intr ++ "scrub/"
  let idx  = (concat $ replicate (4 - (length . show $ n)) "0")  
           ++ show n
  let name = filename ++ "-" ++ idx ++ ".txt"
  let inp  = intr     ++ name
  let outp = outr     ++ name
  print inp
  print outp
  print "-----------------------------------------------"
  scrub CT.utf8 inp outp

main_normalize_words :: FilePath -> FilePath -> IO ()  
main_normalize_words inp outp = do
  print "running scrub ... "
  scrub CT.utf8 inp outp
  print "Done!"
 

{-----------------------------------------------------------------------------
  parse greped data
------------------------------------------------------------------------------}

-- * @Use: main = main_split_by_pattern "path/to/greped-raw" [parsers]
-- *       Assume we have a files in directory at `root` named
-- *       `echo p`.txt for each p in `patterns`,
-- *       filter the file for any items caught by grep but 
-- *       does not `conformToPattern` to repsective p \in patterns
-- *       also save list of items does `notConformToPattern`
main_split_by_pattern :: DirectoryPath -> [Parser Text] -> IO ()
main_split_by_pattern root patterns = do
    createDirectoryIfMissing False $ root ++ "out"
    mapM (split_by_pattern root) patterns
    return ()

split_by_pattern :: DirectoryPath -> Parser Text  -> IO ()
split_by_pattern root p = do
  let inp  = root ++ echo p ++ ".txt"
  let out1 = root ++ "out/" ++ echo p ++ ".txt"
  let out2 = root ++ "out/" ++ "leftover-" ++ echo p ++ ".txt"
  conform_pattern     p inp out1
  not_conform_pattern p inp out2

{-----------------------------------------------------------------------------
  count total occurences of each pattern in 
  reduced corpus built from: `grepped >> main_filterByPattern`
------------------------------------------------------------------------------}

-- * @USE : main_pattern_freq "path\to\corpus" [patterns] "output-file-name"
main_pattern_freq :: DirectoryPath 
                  -> [PatternExpr]
                  -> String 
                  -> IO ()
main_pattern_freq d xs name = do
    let ps    = (\p -> compile p Star Star) <$> xs
    os        <- pattern_freq d `mapM` ps
    let total = foldr (+) 0 (snd <$> os)

    f <- makeDirUnder "good-great-excellent" "out"
    let path  = f ++ name ++ ".txt"

    h    <- S.openFile path S.WriteMode
    time <- show <$> getCurrentTime

    S.hPutStrLn h name
    S.hPutStrLn h time
    S.hPutStrLn h mark
    S.hPutStrLn h $ "total: " ++ show total
    S.hPutStrLn h mark
  
    mapM (\(xs,n) ->  S.hPutStrLn h
               $  xs
               ++ "     " 
               ++ show n) os


    print $ "total: " ++ show total

    return ()
      where mark = foldr (++) mempty $ (const "-") <$> [1..50] 

  
pattern_freq :: DirectoryPath -> Parser Text -> IO (String, Integer)
pattern_freq d p = do
    let name = echo p
    let path = d ++ name ++ ".txt"
    (n,_) <- p `query_at` path
    return (name,n)























