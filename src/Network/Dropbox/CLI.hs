{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonadComprehensions#-}

module Network.Dropbox.CLI (main) where

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Network.Dropbox.API hiding (mode)
import Network.Dropbox.Filesystem hiding (contentHash)
import Prelude
import Streamly
import qualified Streamly.Prelude as S
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Verbosity
import System.FilePath
import System.Posix

data Args = Args Verbosity Cmd
  deriving (Eq, Ord, Read, Show)

data Cmd = NoCmd
         | Ls { lsLong :: LsLong
              , lsRecursive :: LsRecursive
              , lsFiles :: [String]
              }
         | Mkdir { mkdirDirectories :: [String] }
         | Put { putFiles :: [String] }
         deriving (Eq, Ord, Read, Show)

data LsLong = LsShort | LsLong
  deriving (Enum, Eq, Ord, Read, Show)

data LsRecursive = LsFinal | LsRecursive
  deriving (Enum, Eq, Ord, Read, Show)

embedCmd :: Cmd -> Args
embedCmd cmd = Args Normal cmd

extractCmd :: Args -> (Cmd, Cmd -> Args)
extractCmd (Args verb cmd) = (cmd, \cmd' -> Args verb cmd')

remapArgs :: Remap m => m Cmd -> m Args
remapArgs = remap embedCmd extractCmd

makeUpdate :: Update Cmd -> Update Args
makeUpdate = remapUpdate embedCmd extractCmd

--------------------------------------------------------------------------------

argsLs :: Mode Cmd
argsLs = mode "ls" (Ls LsShort LsFinal []) "list directory entries"
         (flagArg addPath "path name")
         [ flagNone ["long", "l"] makeLong "show metadata"
         , flagNone ["recursive", "r"] makeRecursive
           "recursively list subdirectories"
         ]
  where addPath :: Update Cmd
        addPath fp ls = Right $ ls { lsFiles = lsFiles ls ++ [fp] }
        makeLong :: Cmd -> Cmd
        makeLong ls = ls { lsLong = LsLong }
        makeRecursive :: Cmd -> Cmd
        makeRecursive ls = ls { lsRecursive = LsRecursive }

argsMkdir :: Mode Cmd
argsMkdir = mode "mkdir" (Mkdir []) "create directories"
            (flagArg addPath "path name")
            []
  where addPath :: Update Cmd
        addPath fp mkdir =
          Right $ mkdir { mkdirDirectories = mkdirDirectories mkdir ++ [fp] }

argsPut :: Mode Cmd
argsPut = mode "put" (Put []) "upload file or directory"
          (flagArg addPath "path name")
          []
  where addPath :: Update Cmd
        addPath fp put = Right $ put { putFiles = putFiles put ++ [fp] }

args :: Mode Args
args = modes "dbxftp" (Args Normal NoCmd)
       "An ftp-like command line interface to DropBox"
       (remapArgs <$> [argsLs, argsMkdir, argsPut])
       -- (flagsVerbosity makeVerbose)
  where makeVerbose :: Verbosity -> Args -> Args
        makeVerbose verb (Args _ cmd) = Args verb cmd

main :: IO ()
main = do
  putStrLn "DBXFTP: Access DropBox via the command line"
  Args verbose cmd <- processArgs args
  runCmd cmd
  putStrLn "Done."

--------------------------------------------------------------------------------

runCmd :: Cmd -> IO ()
runCmd NoCmd = putStrLn "No command given."
runCmd (Ls long recursive fps) = ls long recursive $ fmap T.pack fps
runCmd (Mkdir fps) = mkdir $ fmap T.pack fps
runCmd (Put fps)
  | null fps = putStrLn "Need at least 1 argument"
  | otherwise = let (srcs, dst) = (init fps, T.pack $ last fps)
                in put srcs dst

--------------------------------------------------------------------------------

ls :: LsLong -> LsRecursive -> [Path] -> IO ()
ls long recursive fps = do
  mgr <- liftIO newManager
  (S.mapM_ T.putStrLn :: Serial T.Text -> IO ())
    $ (aheadly :: Ahead T.Text -> Serial T.Text)
    $ (S.concatMap (ls1 mgr) :: Ahead Path -> Ahead T.Text)
    |$ (S.fromList fps :: Ahead Path)
  where
    ls1 :: Manager -> Path -> Ahead T.Text
    ls1 mgr fp = do
      let arg = GetMetadataArg fp
      md <- liftIO
            $ handleJust (\case DbxNotFoundException{} -> Just ()
                                _ -> Nothing)
                         (\_ -> do putStrLn $ show fp ++ ": not found"
                                   return NoMetadata)
            $ getMetadata mgr arg
      case md of
        NoMetadata -> S.nil
        FolderMetadata{} ->
          let arg = ListFolderArg fp (recursive == LsRecursive)
          in format <$> serially (listFolder mgr arg)
        _ -> S.yield $ format md
    format :: Metadata -> T.Text
    format = case long of
      LsShort -> formatName
      LsLong -> formatInfo
    formatName :: Metadata -> T.Text
    formatName NoMetadata = "<not found>"
    formatName md = fromMaybe (name md) (pathDisplay md)
    formatInfo :: Metadata -> T.Text
    formatInfo md@FileMetadata{} =
      T.intercalate " " [ typeString md
                        , T.pack $ show $ size md
                        , formatName md
                        , symlinkTarget md]
    formatInfo md =
      T.intercalate " " [ typeString md
                        , "-"
                        , formatName md
                        ]
    typeString :: Metadata -> T.Text
    typeString md@FileMetadata{} =
      case symlinkInfo md of
        Nothing -> "-"
        Just _ -> "s"
    typeString FolderMetadata{} = "d"
    typeString DeletedMetadata{} = "D"
    typeString NoMetadata = "?"
    symlinkTarget :: Metadata -> T.Text
    symlinkTarget md@FileMetadata{} =
      case symlinkInfo md of
        Nothing -> ""
        Just sym -> T.append "-> " (target sym)
    symlinkTarget _ = ""

--------------------------------------------------------------------------------

mkdir :: [Path] -> IO ()
mkdir fps = do
  mgr <- liftIO newManager
  S.drain
    $ asyncly
    $ (createFolder mgr :: Async CreateFolderArg -> Async CreateFolderResult)
    |$ (S.map CreateFolderArg :: Async Path -> Async CreateFolderArg)
    |$ (S.fromList fps :: Async Path)

--------------------------------------------------------------------------------

data Counters = Counters { found :: !Int
                         , needUpload :: !Int
                         , uploaded :: !Int
                         }
  deriving (Eq, Ord, Read, Show)

showCounters :: Counters -> String
showCounters (Counters found needUpload uploaded) =
  let skipped = found - needUpload
  in "(" ++ show (uploaded + skipped) ++ "/" ++ show found ++ ")"

newCounters :: IO (IORef Counters)
newCounters = newIORef (Counters 0 0 0)

-- These want to be a lens
countFound :: IORef Counters -> IO ()
countFound counters =
  atomicModifyIORef' counters
  \counter -> (counter { found = found counter + 1}, ())

countNeedUpload :: IORef Counters -> IO ()
countNeedUpload counters =
  atomicModifyIORef' counters
  \counter -> (counter { needUpload = needUpload counter + 1}, ())

countUploaded :: IORef Counters -> IO ()
countUploaded counters =
  atomicModifyIORef' counters
  \counter -> (counter { uploaded = uploaded counter + 1}, ())

put :: [FilePath] -> Path -> IO ()
put fps dst = do
  counters <- newCounters
  fmgr <- liftIO newFileManager
  mgr <- newManager
  dstlist <- S.toList $ listFolder1 mgr (ListFolderArg dst True)
  let dstmap = H.fromList $ fmap makePair dstlist
  S.drain
    $ asyncly
    $ S.trace (\_ -> countUploaded counters)
    |$ uploadFiles fmgr mgr
    |$ S.map makeUploadFileArg
    |$ S.trace (\_ -> countNeedUpload counters)
    |$ S.filterM (needUploadFile fmgr mgr counters dstmap)
    |$ S.trace (\_ -> countFound counters)
    |$ listDirsRec fmgr dst
    |$ S.fromList fps
  where
    listFolder1 :: Manager -> ListFolderArg -> Serial Metadata
    listFolder1 mgr arg =
      S.handle (\case DbxNotFoundException {} -> S.nil
                      ex -> liftIO $ throw ex)
      $ listFolder mgr arg
    listDirsRec :: FileManager
                -> Path
                -> Async FilePath
                -> Async (FilePath, FileStatus, Path)
    listDirsRec fmgr dst = S.concatMap (listDirRec fmgr dst)
    listDirRec :: FileManager
               -> Path
               -> FilePath
               -> Async (FilePath, FileStatus, Path)
    listDirRec fmgr dst src = do
      fs <- liftIO $ fileStatus fmgr src
      if isDirectory fs
        then S.concatMap (listDirRec1 fmgr dst src) |$ listDir1 fmgr src
        else S.yield (src, fs, dst)
    listDir1 :: FileManager -> FilePath -> Async FilePath
    listDir1 fmgr fp = serially $ listDir fmgr fp
    listDirRec1 :: FileManager
                -> Path -> FilePath
                -> FilePath -> Async (FilePath, FileStatus, Path)
    listDirRec1 fmgr dst src fp =
      listDirRec fmgr (appendPath dst fp) (src </> fp)
    appendPath :: Path -> FilePath -> Path
    appendPath p fp = T.concat [p, "/", T.pack $ takeFileName fp]
    makePair :: Metadata -> (Path, Metadata)
    makePair NoMetadata = ("<not found>", NoMetadata)
    makePair md = (fromMaybe (name md) (pathDisplay md), md)
    needUploadFile :: FileManager -> Manager -> IORef Counters
                   -> H.HashMap Path Metadata
                   -> (FilePath, FileStatus, Path) -> IO Bool
    needUploadFile fmgr mgr counters dstmap (fp, fs, p) =
      if not (isRegularFile fs)
      then return False
      else do
        case H.lookup p dstmap of
          Nothing -> do
            ctrs <- readIORef counters
            putStrLn $ ("Uploading " ++ show p ++ " (remote does not exist) "
                        ++ showCounters ctrs)
            return True
          Just md ->
            if size md /= fromIntegral (fileSize fs)
            then do
              ctrs <- readIORef counters
              putStrLn $ ("Uploading " ++ show p ++ " (remote size differs)"
                          ++ showCounters ctrs)
              return True
            else do
              ContentHash hash <- fileContentHash fmgr fp
              let hashDiffers = T.encodeUtf8 (contentHash md) /= hash
              ctrs <- readIORef counters
              if hashDiffers
                then putStrLn $ ("Uploading " ++ show p ++ " (hash differs)"
                                 ++ showCounters ctrs)
                else putStrLn $ ("Skipping " ++ show p ++ " "
                                 ++ showCounters ctrs)
              return hashDiffers
    makeUploadFileArg :: (FilePath, FileStatus, Path) -> UploadFileArg
    makeUploadFileArg (fp, fs, p) = let mode = Overwrite
                                        autorename = False
                                        mute = False
                                    in UploadFileArg fp p mode autorename mute
