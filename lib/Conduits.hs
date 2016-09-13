{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A library of conduit functions
-- | Author  : Xiao Ling
-- | Date    : 8/12/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Conduits where

import Prelude hiding           (readFile, writeFile , 
                                 lines               )
import System.FilePath
import System.Directory

import Control.Monad.State  
import Control.Monad.Except    
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class   (MonadIO, liftIO     )
import Control.Exception.Base   (SomeException       )

import Codec.Compression.GZip   (decompress          )

import Data.Conduit 
import Data.Conduit.Text 
import Conduit hiding           (sourceDirectory     ,
                                 sourceFile          )
import Data.Conduit.Filesystem  (sourceDirectory     )
import Data.Conduit.Binary      (sourceFile, sinkFile)


import Data.Text hiding         (lines, chunksOf, foldr)
import Data.List.Split          (chunksOf              )
import Data.Text.Lazy.IO        (readFile , writeFile  )
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

{-----------------------------------------------------------------------------
   Types: Monad Transformer describing common functions needed
       to interact with file systems
------------------------------------------------------------------------------}

-- * A file operation monad transformer
-- * `FileOps`eration is a stateful computation keeping track of state `s`
type FileOpS m s = (MonadState s m , MonadIO m            , 
                    MonadResource m, MonadBaseControl IO m)

-- * A File Operation with trivial state `()`
type FileOp  m   = FileOpS m ()

-- * Result of `eval` a `FileOpS`
type Op m        = (MonadBaseControl IO m, MonadThrow m, MonadIO m)

{-----------------------------------------------------------------------------
   II. Operations over `FileOpS`
------------------------------------------------------------------------------}

-- * Run a FileOpS `m` with some user specified state `s`
eval :: (Op m, Monoid s)
      => ResourceT (StateT s m) a 
      -> m (a,s)
eval m = runStateT (runResourceT m) mempty

-- * Run a FileOp `m` with trivial state ()
-- * Use this when we do not need to keep a 
-- * meaninful state
run :: ( MonadBaseControl IO m ) 
    => ResourceT (StateT () m) a 
    -> m a
run m = evalStateT (runResourceT m) ()


{-----------------------------------------------------------------------------
  Conduit sources
------------------------------------------------------------------------------}

-- * Shallow traversal of all files in path `fs` with extension `e`
-- * If path invalid or extension invalid, pipe terminates
sourceDirectories :: FileOpS m s => [FilePath] -> String -> Source m FilePath
sourceDirectories fs e =  mapM_ sourceDirectory fs
               =$= filterC (\p -> takeExtension p == e)


-- * if no file exists at `FilePath` `f`, then
-- * output empty ByteString
sourceFileE :: FileOpS m s => FilePath -> Source m B.ByteString
sourceFileE f = catchC (sourceFile f) 
                (\(e :: SomeException) -> yield mempty)

{-----------------------------------------------------------------------------
   Conduit pipes
------------------------------------------------------------------------------}

-- * TODO : make exception handling here
-- * awaits a file path and opens as bytestring
openFile :: (Monad m, MonadIO m) 
         => Conduit FilePath m B.ByteString
openFile = awaitForever $ \p -> do
           f <- liftIO $ B.readFile p
           yield f


-- * Awaits bytestring and convert to list of text,
-- * splitting on token char
linesOn :: FileOpS m s => String -> Conduit B.ByteString m [Text]
linesOn tok =  decode utf8 
           =$= lines 
           =$= mapC (splitOn . pack $ tok)


-- * TODO: swap out the L.readFile for something more safe
-- *       Break this one down into source, pipe, and sink

-- * open .gz file with found at path `p`
-- * and untar it, save it in the same directory with extension `ext`
-- * yield the untared file `f` downstream with its filepath
untarSaveAs :: FileOpS m s
            => String 
            -> Conduit FilePath m (FilePath, L.ByteString)
untarSaveAs ext = awaitForever $ \p -> do

  let name = dropExtension . takeFileName $ p

  liftIO banner
  liftIO . print $ "untar and save file: " ++ name
  
  f <- liftIO $ decompress <$> L.readFile p   

  liftIO . flip L.writeFile f $ takeDirectory p ++ "/" ++ name ++ ext

  yield (p, f)


-- * Shard all file with extension `ext` found at path `p` into 
-- * `size`ed pieces and save in output directory `out`
shardFile :: FileOpS m s 
          => String 
          -> FilePath 
          -> Int 
          -> Conduit FilePath m ()
shardFile ext out size = awaitForever $ \p -> do
    
    let name = dropExtension . takeFileName $ p
    let dir  = takeExtension p

    f <- liftIO $ readFile p
    let ts  = LT.splitOn (LT.pack "\n") f
    let ts' = LT.unlines <$> chunksOf size ts

    liftIO $ foldM 
           (\n t -> do
                let name' = name ++ "-" ++ show n ++ ext
                let path  = out  ++ "/" ++ name'

                --banner 
                --print $ "saving file: " ++ path
                writeFile path t
                return $ n + 1

            ) 0 ts'

    return ()

-- * Log the current index of file `mxs` encountered
-- * for each new file encountered, increment the counter
logNum :: (Show i, FileOpS m Int) => Conduit i m i
logNum = awaitForever $ \xs -> do
            lift . modify $ succ
            n <- lift get
            liftIO banner
            log_ n
            yield xs
            logNum
              where 
                log_ n = liftIO . putStrLn 
                       $ "file number " ++ show n

-- * log input `i` to console
logi :: (Show i, FileOpS m s) => Conduit i m i
logi = awaitForever $ \xs -> do
          liftIO banner
          liftIO . putStrLn . show $ xs
          yield xs
          logi

-- * log message `xs` 
logm :: FileOpS m s => String -> Conduit i m i
logm xs = awaitForever $ \f -> do
  liftIO banner
  liftIO . print $ xs
  liftIO banner
  yield f


{-----------------------------------------------------------------------------
   Conduit sinks
------------------------------------------------------------------------------}

-- * `cap` a conduit pipeline 
cap :: MonadIO m => Consumer i m ()
cap = do
  mx <- await
  case mx of
    Nothing -> do
      liftIO banner
      liftIO $ putStrLn "pipe terminated"
      liftIO banner
      return ()
    _       -> cap



{-----------------------------------------------------------------------------
   III. Utility operations
------------------------------------------------------------------------------}

-- * When logging to console, `demark` the
-- * messages with "============="
banner :: IO ()
banner = putStrLn $ foldr (++) mempty 
                  $ (const "-") <$> [1..50] 
























