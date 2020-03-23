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
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Network.Dropbox.API hiding (mode)
import Network.Dropbox.Filesystem hiding (contentHash)
import Network.Dropbox.Progress
import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL (lmap)
import qualified Streamly.Prelude as S
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Verbosity
import System.FilePath
import System.Posix
import Text.Printf

data Args = Args Verbosity Cmd
  deriving (Eq, Ord, Read, Show)

data Cmd = NoCmd
         | Cp { cpFiles :: [String] }
         | Ls { lsLong :: LsLong
              , lsRecursive :: LsRecursive
              , lsFiles :: [String]
              }
         | Mkdir { mkdirDirectories :: [String] }
         | Mv { mvFiles :: [String] }
         | Put { putFiles :: [String] }
         | Rm { rmFiles :: [String] }
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

argsCp :: Mode Cmd
argsCp = mode "cp" (Cp []) "copy files or directories"
         (flagArg addPath "path name")
         []
  where addPath :: Update Cmd
        addPath fp cp = Right $ cp { cpFiles = cpFiles cp ++ [fp] }

argsLs :: Mode Cmd
argsLs = mode "ls" (Ls LsShort LsFinal []) "list files or directories"
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

argsMv :: Mode Cmd
argsMv = mode "mv" (Mv []) "move files or directories"
         (flagArg addPath "path name")
         []
  where addPath :: Update Cmd
        addPath fp mv = Right $ mv { mvFiles = mvFiles mv ++ [fp] }

argsPut :: Mode Cmd
argsPut = mode "put" (Put []) "upload files or directories"
          (flagArg addPath "path name")
          []
  where addPath :: Update Cmd
        addPath fp put = Right $ put { putFiles = putFiles put ++ [fp] }

argsRm :: Mode Cmd
argsRm = mode "rm" (Rm []) "delete files and directories"
         (flagArg addPath "path name")
         []
  where addPath :: Update Cmd
        addPath fp rm =
          Right $ rm { rmFiles = rmFiles rm ++ [fp] }

args :: Mode Args
args = modes "dbxftp" (Args Normal NoCmd)
       "An ftp-like command line interface to DropBox"
       (remapArgs <$> [argsCp, argsLs, argsMkdir, argsMv, argsRm, argsPut])
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
runCmd (Cp fps)
  | null fps = putStrLn "Need at least 1 argument"
  | otherwise = let (srcs, dst) = (T.pack <$> init fps, T.pack $ last fps)
                in cp srcs dst
runCmd (Ls long recursive fps) = ls long recursive $ fmap T.pack fps
runCmd (Mkdir fps) = mkdir $ fmap T.pack fps
runCmd (Mv fps)
  | null fps = putStrLn "Need at least 1 argument"
  | otherwise = let (srcs, dst) = (T.pack <$> init fps, T.pack $ last fps)
                in mv srcs dst
runCmd (Put fps)
  | null fps = putStrLn "Need at least 1 argument"
  | otherwise = let (srcs, dst) = (init fps, T.pack $ last fps)
                in put srcs dst
runCmd (Rm fps) = rm $ fmap T.pack fps

--------------------------------------------------------------------------------

cp :: [Path] -> Path -> IO ()
cp fps dst = runWithProgress \smgr -> do
  mgr <- liftIO newManager
  S.drain
    $ (copy smgr mgr :: Serial CopyArg -> Serial CopyResult)
    $ (S.map (\fp -> CopyArg fp dst) :: Serial Path -> Serial CopyArg)
    $ (S.fromList fps :: Serial Path)

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
    $ (createFolder mgr :: Serial CreateFolderArg -> Serial CreateFolderResult)
    $ (S.map CreateFolderArg :: Serial Path -> Serial CreateFolderArg)
    $ (S.fromList fps :: Serial Path)

--------------------------------------------------------------------------------

mv :: [Path] -> Path -> IO ()
mv fps dst = do
  mgr <- liftIO newManager
  S.drain
    $ (move mgr :: Serial MoveArg -> Serial MoveResult)
    $ (S.map (\fp -> MoveArg fp dst) :: Serial Path -> Serial MoveArg)
    $ (S.fromList fps :: Serial Path)

--------------------------------------------------------------------------------

data Destination = Upload    -- need upload
                 | Copy Path -- can copy from existing file
                 | Skip      -- already exists on remote
                 | Ignore    -- wrong file type; cannot upload
  deriving (Eq, Ord, Read, Show)

data Preparation = NoPreparation  -- no preparation necessary
                 | RemoveExisting -- need to remove existing entry
  deriving (Eq, Ord, Read, Show)

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
put fps dst = runWithProgress \smgr -> do
  fmgr <- liftIO newFileManager
  mgr <- newManager
  dstlist <- withActive smgr "[scanning remote files]"
             $ S.toList $ listFolder1 mgr (ListFolderArg dst True)
  addLog smgr $ T.pack $ "Found " ++ show dstlist
  let pathMap = makePathMap dstlist
  let hashMap = makeHashMap dstlist
  S.mapM_ (\srclist -> do
              addLog smgr "Starting batch"
              -- Calculate local content hashes
              addLog smgr "Within batch: Calculating content hashes"
              srclist <- S.toList
                $ S.mapM (addDestination smgr fmgr pathMap hashMap)
                $ (asyncly . maxThreads 10
                   . S.mapM (addFileContentHash smgr fmgr)
                   . serially)
                $ S.fromList srclist
              -- Remove directories that are in the way
              -- TODO: Save files that will be copied below
              addLog smgr "Within batch: Starting remote deletions"
              S.drain
                $ delete mgr
                $ mapMaybeA (\(fp, fs, fh, p, prep, dest) ->
                               case prep of
                                 RemoveExisting -> Just $ DeleteArg p
                                 _ -> Nothing)
                $ S.fromList srclist
              -- Copy files
              addLog smgr "Within batch: Starting remote copies"
              S.drain
                $ copy smgr mgr
                $ mapMaybeA (\(fp, fs, fh, p, prep, dest) ->
                                case dest of
                                  Copy src -> Just $ CopyArg src p
                                  _ -> Nothing)
                $ S.fromList srclist
              -- Upload files
              addLog smgr "Within batch: Starting uploads"
              S.drain
                $ upload smgr fmgr mgr
                $ mapMaybeA (\(fp, fs, fh, p, prep, dest) ->
                               case dest of
                                 Upload ->
                                   Just $ uploadArg fp (fileSize fs) p
                                 _ -> Nothing)
                $ S.fromList srclist
              addLog smgr "Finished batch"
          )
    $ groupFiles
    -- $ groupBy (\_ _ -> ()) () (\_ -> False) FL.toList
    -- $ S.chunksOf 100 FL.toList
    $ filterA (\(fp, fs, p) -> isRegularFile fs)
    -- $ S.trace (\(fp, fs, p) -> addLog smgr $ T.pack $ "Found " ++ fp)
    $ listDirsRec fmgr dst
    $ S.fromList fps
  where
    makePathMap :: [Metadata] -> H.HashMap Path Metadata
    makePathMap =
      H.fromList
      . fmap (\md -> (fromMaybe (T.toLower $ name md) (pathLower md) , md))
      . filter isMetadata
    isMetadata NoMetadata = False
    isMetadata _ = True
    makeHashMap :: [Metadata] -> H.HashMap T.Text Metadata
    makeHashMap =
      H.fromList
      . fmap (\md -> (contentHash md, md))
      . filter isFile
    isFile FileMetadata{} = True
    isFile _ = False
    batchCount = 100
    batchBytes = 128 * 1024 * 1024 -- 128 MByte
    groupFiles :: Serial (FilePath, FileStatus, Path)
               -> Serial [(FilePath, FileStatus, Path)]
    groupFiles =
      groupBy (\(count, bytes) (fp, fs, p) -> (count + 1, bytes + fileSize fs))
              (0, 0)
              (\(count, bytes) -> count >= batchCount || bytes >= batchBytes)
              FL.toList
    addFileContentHash :: ScreenManager
                       -> FileManager
                       -> (FilePath, FileStatus, Path)
                       -> IO (FilePath, FileStatus, T.Text, Path)
    addFileContentHash smgr fmgr (fp, fs, p) = do
      ContentHash fh <- fileContentHash smgr fmgr fp
      return (fp, fs, T.decodeUtf8 fh, p)
    addDestination :: ScreenManager
                   -> FileManager
                   -> H.HashMap Path Metadata
                   -> H.HashMap T.Text Metadata
                   -> (FilePath, FileStatus, T.Text, Path)
                   -> IO (FilePath, FileStatus, T.Text, Path,
                          Preparation, Destination)
    addDestination smgr fmgr pathMap hashMap (fp, fs, fh, p) = do
      (prep, dest) <-
        chooseDestination smgr fmgr pathMap hashMap (fp, fs, fh, p)
      return (fp, fs, fh, p, prep, dest)

filterA :: (IsStream t, MonadAsync m) => (a -> Bool) -> t m a -> t m a
filterA pred = filterMA (return . pred)

filterMA :: (IsStream t, MonadAsync m) => (a -> m Bool) -> t m a -> t m a
filterMA pred =
  S.mapMaybeM \x -> do p <- pred x
                       return if p then Just x else Nothing

mapMaybeA :: (IsStream t, MonadAsync m) => (a -> Maybe b) -> t m a -> t m b
mapMaybeA f = S.mapMaybeM (return . f)

groupBy :: (IsStream t, MonadAsync m)
        => (b -> a -> b) -> b -> (b -> Bool) -> FL.Fold m a c -> t m a -> t m c
groupBy step init pred fold =
  S.splitOnSuffix pred' fold' . S.postscanl' step' init'
  where step' (_, b, _) a = let b' = step b a
                                done = pred b'
                                b'' = if done then init else b'
                            in (Just a, b'', done)
        init' = (Nothing, init, True)
        pred' (_, _, done) = done
        fold' = FL.lmap (\(Just a, _, _) -> a) fold

listFolder1 :: Manager -> ListFolderArg -> Serial Metadata
listFolder1 mgr arg =
  S.handle (\case DbxNotFoundException {} -> S.nil
                  ex -> liftIO $ throw ex)
  $ listFolder mgr arg

listDirsRec :: FileManager
            -> Path
            -> Serial FilePath
            -> Serial (FilePath, FileStatus, Path)
listDirsRec fmgr dst = S.concatMap (listDirRec dst)
  where
    listDirRec :: Path -> FilePath
               -> Serial (FilePath, FileStatus, Path)
    listDirRec dst src = do
      fs <- S.yieldM $ fileStatus fmgr src
      if isDirectory fs
        then S.concatMap (listDirRec1 dst src) $ listDir fmgr src
        else S.yield (src, fs, dst)
    listDirRec1 :: Path -> FilePath -> FilePath
                -> Serial (FilePath, FileStatus, Path)
    listDirRec1 dst src fp =
      listDirRec (appendPath dst fp) (src </> fp)
    appendPath :: Path -> FilePath -> Path
    appendPath p fp = T.concat [p, "/", T.pack $ takeFileName fp]

chooseDestination :: ScreenManager -> FileManager
                  -> H.HashMap Path Metadata
                  -> H.HashMap T.Text Metadata
                  -> (FilePath, FileStatus, T.Text, Path)
                  -> IO (Preparation, Destination)
chooseDestination smgr fmgr pathMap hashMap arg@(fp, fs, fh, p) =
  case H.lookup (T.toLower p) pathMap of
    Nothing ->
      case H.lookup fh hashMap of
        Nothing -> do uploading "remote does not exist"
                      return (NoPreparation, Upload)
        Just md -> do copying
                      return (NoPreparation, Copy (identifier md))
    Just md -> case md of
      FileMetadata{} -> do
        let remoteHash = contentHash md
        if fh == remoteHash
          then do skipping
                  return (NoPreparation, Skip)
          else
          case H.lookup fh hashMap of
            Nothing -> do uploading "hash mismatch"
                          return (RemoveExisting, Upload)
            Just md -> do copying
                          return (RemoveExisting, Copy (identifier md))
      _ -> do
        case H.lookup fh hashMap of
          Nothing -> do uploading "hash mismatch"
                        return (RemoveExisting, Upload)
          Just md -> do copying
                        return (RemoveExisting, Copy (identifier md))
  where
    uploading reason =
      addLog smgr $ T.pack $ printf "Uploading %s to %s (%s)" fp p reason
    copying =
      addLog smgr $ T.pack $ printf "Copying %s from existing remote file" fp
    skipping =
      addLog smgr $ T.pack $ printf "Skipping %s" fp

--------------------------------------------------------------------------------

rm :: [Path] -> IO ()
rm fps = do
  mgr <- liftIO newManager
  S.drain
    $ (delete mgr :: Serial DeleteArg -> Serial DeleteResult)
    $ (S.map DeleteArg :: Serial Path -> Serial DeleteArg)
    $ (S.fromList fps :: Serial Path)
