-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A collection of misc scripts to be run in main w/ no gaurantee that they will work
-- | Author  : Xiao Ling
-- | Date    : 9/13/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 
module Scripts (

    main_split_by_pattern
  , main_pattern_freq

  ) where


import System.IO
import System.Directory
import System.FilePath.Posix
import qualified System.IO as S


import Data.Time.Clock
import Data.Text (Text)
import Data.Attoparsec.Text 

import Core
import Src
import Lib 


{-----------------------------------------------------------------------------
  preprocesss data
    
    patterns <- strongWeak <$> config_l
    let parsers = (\p -> compile p Star Star) <$> patterns
    let ns = echo <$> parsers
    mapM print ns
    main_filterByPattern root_sw parsers

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
  let path = root ++ echo p ++ ".txt"
  let out1 = root ++ "out/" ++ echo p ++ ".txt"
  let out2 = root ++ "out/" ++ "leftover-" ++ echo p ++ ".txt"
  conformToPattern    path out1 p
  --notConformToPattern path out2 p

{-----------------------------------------------------------------------------
  count total occurences of each pattern in 
  reduced corpus built from: `grepped >> main_filterByPattern`
------------------------------------------------------------------------------}

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
    (n,_) <- p `queryIn` path
    print name
    print n
    return (name,n)





























