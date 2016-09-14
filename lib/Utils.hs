{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Misc scripts to operate on files, and others
-- | Author  : Xiao Ling
-- | Date    : 8/27/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Utils (

      takeN
    , sourceDirs
    , makeDirUnder

    , untarFiles
    , shardFiles
    , concatFiles
    , cutFiles

    ) where


import System.Directory
import System.FilePath.Posix
import qualified System.IO as S


import Data.Conduit 
import Data.Time.Clock
import Data.List.Split
import Data.List.Split (splitOn)

import Conduits


type DirectoryPath = FilePath

{-----------------------------------------------------------------------------
  Pure
------------------------------------------------------------------------------}


takeN :: Int -> [a] -> [a]
takeN 0 _      = []
takeN n (x:xs) = x : takeN (n-1) xs
takeN _ _      = []


{-----------------------------------------------------------------------------
  Read from Disk
------------------------------------------------------------------------------}

-- * Given directory paths `ds` and file extension `ext`
-- * list all files in directories with this extension
sourceDirs :: String -> [FilePath] -> IO [FilePath]
sourceDirs ext ds = do
  dds <- sequence $  sourceDir ext <$> ds
  return $ concat dds


-- * Given directory path `d` and file extension `ext`
-- * list all files in directory with this extension
sourceDir :: String -> FilePath -> IO [FilePath]
sourceDir ext d = do
  fs      <- getDirectoryContents d
  let fs' = filter (\f -> takeExtension f == ext) fs
  return $ (\f -> d ++ "/" ++ f) <$> fs'


{-----------------------------------------------------------------------------
  Write to Disk
------------------------------------------------------------------------------}

-- * create directory `f` under `project` folder named: `name`
makeDirUnder :: String -> String -> IO FilePath
makeDirUnder project name = do
      xs <- getCurrentDirectory
      let top:_   = splitOn project xs
      let dir     = top ++ project ++ "/" ++ name
      createDirectoryIfMissing False dir
      return dir

{-----------------------------------------------------------------------------
   Modify files on Disk
------------------------------------------------------------------------------}

-- * @Use: untar "/path/to/file" ".gz" ".txt"
-- * Untar all files with extension `e1` found at directory `p`
-- * and save them in the same directory with extension `e2` 
untarFiles :: Op m => DirectoryPath -> String -> String -> m ()
untarFiles p e1 e2 = run
                $  [p] `sourceDirectories` e1
                $$  untarSaveAs e2
                =$= cap

-- * @Use: shardFiles ".txt" 50000 "/path/to/src" "/path/to/tgt"
-- * Shard all files with `ext` found at directory `p`
-- * into chunks of n lines each
-- * and save in output directory `o`
shardFiles :: Op m
         => String 
         -> Int
         -> DirectoryPath 
         -> DirectoryPath 
         -> m ()
shardFiles ext n p o = run
                $   [p] `sourceDirectories` ext
                $$  shardFile ext o n
                -- =$= logm "Sharded all files!"
                =$= cap                


-- * @Use: conatFiles "/path/to/src" "/path/to/tgt"
-- * Concat all .txt files found at directory `d`
-- * and save at output path `o`
concatFiles :: DirectoryPath -> DirectoryPath -> IO ()
concatFiles d o = do
  fs   <- sourceDirs ".txt" [d]
  file <- sequence $ readFile <$> fs
  let ts   = concat file
  let path = o ++ "/" ++ "catGrams.txt"

  o <- S.openFile path S.WriteMode
  S.hPutStrLn o ts
  S.hClose o
  return ()


-- * @Use: cutFiles 100 "path/to/hugeFiles"
-- * downsize all files found at directory `d` to size `n`
-- * and save at newly created directory directory "d_small"
-- * if `n` is larger than size of file, the original file is output
cutFiles :: Int -> DirectoryPath -> IO FilePath
cutFiles n d = do
  let dir   = takeDirectory d
  let name' = takeBaseName  d
  let name  = name' ++ "_small"
  let d'    = dir ++ "/" ++ name
  createDirectoryIfMissing False d'

  fs <- sourceDir ".txt" d
  mapM (\f -> cutFile n d' f) fs
  return d'


-- * open file found at `f`, truncate and save in directory `d`
cutFile :: Int -> DirectoryPath -> DirectoryPath -> IO [String]
cutFile n d f = do
    xs <- readFile f

    let ys   = splitOn "\n" xs
    let ys'  = takeN n ys
    let name = (takeBaseName . takeFileName $ f) ++ ".txt"
    let out  = d ++ "/" ++ name

    h  <- S.openFile out S.WriteMode
    mapM (S.hPutStrLn h) ys'
    S.hClose h

    return ys'















