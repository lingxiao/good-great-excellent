-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A collection of scripts to be run in main
-- | Author  : Xiao Ling
-- | Date    : 9/13/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 
module Subroutines where


import System.IO
import System.Directory
import System.FilePath.Posix

import Data.Text (Text)
import Data.Attoparsec.Text 

import Src
import Lib 


{-----------------------------------------------------------------------------
  preprocesss data
------------------------------------------------------------------------------}

-- @Use : go "path/to/pattern-data.txt" p
-- *      save lines of pattern-data.txt recongized by p 
-- *      in output directory "path/to/pattern-data_filtered.txt"
filterPattern :: FilePath -> Parser Text -> IO FilePath
filterPattern inp p = do
  let root = takeDirectory inp ++ "/filtered/"
  createDirectoryIfMissing False root
  let outp =  root
          ++ name p
          ++ ".txt"

  is <- lines <$> readFile inp
  os <- filterByPattern inp outp p 
  return outp
  


      
main_filter_strong_weak :: DirectoryPath -> [Parser Text] -> IO ()
main_filter_strong_weak root p_sw = do
    createDirectoryIfMissing False $ root ++ "out"
    mapM (\p -> filterByPattern (root ++ name p ++ ".txt")
                                (root ++ "out/" ++ name p ++ ".txt")
                                p) p_sw

    return ()




  

{-



-- * @Use : main_filterByPattern ["path/to/data.txt"] "path/to/patterns.txt"
-- *        filters each data.txt by corresponding pattern found in patter.txt and
-- * Note : input data.txt must correspond to the appropriate pattern
-- *        or all data.txt will be filtered out
-- *        save each output in "path/to/"
filterPatterns :: [FilePath] 
                     -> FilePath 
                     -> IO [(FilePath, String)]
filterPatterns dataPath patterns = do
  fs <- lines <$> readFile patterns
  let ps  = (\f -> compile f Star Star) <$> fs
  let fps = zip dataPath ps
  uncurry filterPattern `mapM` fps
  return $ (\(f,p) -> (f, name p)) <$> fps

-}

































