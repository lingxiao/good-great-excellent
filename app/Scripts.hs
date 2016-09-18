-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A collection of misc scripts to be run in main w/ no gaurantee that they will work
-- | Author  : Xiao Ling
-- | Date    : 9/13/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 
module Scripts where


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


-- * @Use: main = main_filterByPattern "path/to/raw" [parsers]
main_filter_by_pattern :: DirectoryPath -> [Parser Text] -> IO ()
main_filter_by_pattern root patterns = do
    createDirectoryIfMissing False $ root ++ "out"
    mapM (\p -> filterByPattern (root ++ echo p ++ ".txt")
                                (root ++ "out/" ++ echo p ++ ".txt")
                                p) patterns

    return ()

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





























