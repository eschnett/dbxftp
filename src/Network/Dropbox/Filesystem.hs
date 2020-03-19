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

module Network.Dropbox.Filesystem
  ( FileManager(..)
  , newFileManager
  , waitOpenFile
  , signalOpenFile
  , FileType(..)
  , fileType
  , showFileStatus
  , fileStatus
  , listDir
  -- , listDirsRec
  , ContentHash(..)
  , contentHash
  , fileContentHash
  , addContentHashes
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Loops
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.Text.Encoding as T
import Data.Word
import Streamly
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as S
import System.IO
import System.Posix

--------------------------------------------------------------------------------

maxOpenFiles :: Int
maxOpenFiles = 100

newtype FileManager = FileManager { theOpenFiles :: QSem }

newFileManager :: IO FileManager
newFileManager = FileManager <$> newQSem maxOpenFiles

waitOpenFile :: FileManager -> IO ()
waitOpenFile fmgr = waitQSem (theOpenFiles fmgr)

signalOpenFile :: FileManager -> IO ()
signalOpenFile fmgr = signalQSem (theOpenFiles fmgr)

--------------------------------------------------------------------------------

data FileType = BlockDevice
              | CharacterDevice
              | NamedPipe
              | RegularFile
              | Directory
              | SymbolicLink
              | Socket
              | UnknownFileType
  deriving (Enum, Eq, Ord, Read, Show)

fileType:: FileStatus -> FileType
fileType fs | isBlockDevice fs     = BlockDevice
            | isCharacterDevice fs = CharacterDevice  
            | isNamedPipe fs       = NamedPipe
            | isRegularFile fs     = RegularFile
            | isDirectory fs       = Directory
            | isSymbolicLink fs    = SymbolicLink
            | isSocket fs          = Socket
            | otherwise            = UnknownFileType

showFileStatus :: FileStatus -> String
showFileStatus fs = "("
                    ++ intercalate ", " [ "type " ++ show (fileType fs)
                                        , "size " ++ show (fileSize fs)
                                        ]
                    ++ ")"

instance Show FileStatus where show = showFileStatus

fileStatus :: FileManager -> FilePath -> IO FileStatus
fileStatus fmgr fp =
  -- bracket_ (waitOpenFile fmgr) (signalOpenFile fmgr) $ getFileStatus fp
  getSymbolicLinkStatus fp

--------------------------------------------------------------------------------

listDir :: FileManager -> FilePath -> Serial FilePath
listDir fmgr dp = S.bracket open close readEntry
  where open :: IO DirStream
        open = do waitOpenFile fmgr
                  ds <- openDirStream dp
                  return ds
        close :: DirStream -> IO ()
        close ds = do closeDirStream ds
                      signalOpenFile fmgr
        readEntry :: DirStream -> Serial FilePath
        readEntry ds = S.map id -- (\fp -> dp </> fp)
                       $ S.filter (\fp -> fp /= "." && fp /= "..")
                       $ S.takeWhile (not . null)
                       $ S.repeatM (readDirStream ds)

listDir' :: FileManager -> FilePath -> IO [FilePath]
listDir' fmgr dp = bracket open close readEntries
  where open :: IO DirStream
        open = do waitOpenFile fmgr
                  ds <- openDirStream dp
                  return ds
        close :: DirStream -> IO ()
        close ds = do closeDirStream ds
                      signalOpenFile fmgr
        readEntries :: DirStream -> IO [FilePath]
        readEntries ds =
          do fps <- unfoldWhileM (not . null) $ readDirStream ds
             evaluate $ filter (\fp -> fp /= "." && fp /= "..") fps

-- might not be needed
listDirsRec :: FileManager -> Async FilePath -> Async (FilePath, FileStatus)
listDirsRec fmgr fps = S.concatMap expandDirs |$ S.concatMap listDir1 |$ fps
  where
    expandDirs :: (FilePath, FileStatus) -> Async (FilePath, FileStatus)
    expandDirs (fp, fs) | isDirectory fs = listDir1 fp
                        | otherwise = S.yield (fp, fs)
    listDir1 :: FilePath -> Async (FilePath, FileStatus)
    listDir1 fp = S.mapM addFileStatus $ serially $ listDir fmgr fp
    addFileStatus :: FilePath -> IO (FilePath, FileStatus)
    addFileStatus fp = do fs <- fileStatus fmgr fp
                          return (fp, fs)

--------------------------------------------------------------------------------

newtype ContentHash = ContentHash BS.ByteString
  deriving (Eq, Ord, Read, Show)

instance FromJSON ContentHash where
  parseJSON = withText "ContentHash"
    \v -> ContentHash . T.encodeUtf8 <$> parseJSON (String v)

instance ToJSON ContentHash where
  toJSON (ContentHash hash) = String $ T.decodeUtf8 hash

-- <https://www.dropbox.com/developers/reference/content-hash>
contentHash0 :: BL.ByteString -> ContentHash
contentHash0 content =
  ContentHash
  $ BL.toStrict
  $ BL.toLazyByteString
  $ BL.byteStringHex
  $ SHA256.hashlazy
  $ BL.concat
  $ fmap (BL.fromStrict . SHA256.hashlazy)
  $ unfoldr split
  $ content
  where
    split :: BL.ByteString -> Maybe (BL.ByteString, BL.ByteString)
    split bs | BL.null bs = Nothing
             | otherwise = Just $ BL.splitAt chunkSize bs
    chunkSize = 4 * 1024 * 1024 -- 4 MByte

-- might not be needed
fileContentHash0 :: FileManager -> FilePath -> IO ContentHash
fileContentHash0 fmgr fp = bracket_
                           (waitOpenFile fmgr)
                           (signalOpenFile fmgr)
                           do content <- BL.readFile fp
                              hash <- evaluate $ contentHash0 content
                              return hash

-- <https://www.dropbox.com/developers/reference/content-hash>
contentHash :: Serial Word8 -> IO ContentHash
contentHash =
  (fmap (ContentHash
          . BL.toStrict
          . BL.toLazyByteString
          . BL.byteStringHex
          . SHA256.hashlazy
          . BL.fromChunks) :: IO [BS.ByteString] -> IO ContentHash)
  . (S.toList :: Serial BS.ByteString -> IO [BS.ByteString])
  . (S.map (SHA256.hashlazy . BL.pack . A.toList)
      :: Serial (A.Array Word8) -> Serial BS.ByteString)
  . (S.chunksOf chunkSize (A.writeN chunkSize)
      :: Serial Word8 -> Serial (A.Array Word8))
  where
    chunkSize = 4 * 1024 * 1024 -- 4 MByte

fileContentHash :: FileManager -> FilePath -> IO ContentHash
fileContentHash fmgr fp =
  bracket_
  (waitOpenFile fmgr)
  (signalOpenFile fmgr)
  $ contentHash
  $ File.toBytes fp

-- might not be needed
addContentHashes :: FileManager
                 -> Async (FilePath, FileStatus)
                 -> Async (FilePath, FileStatus, ContentHash)
addContentHashes fmgr = S.mapM addContentHash
  where
    addContentHash (fp, fs) = do hash <- fileContentHash fmgr fp
                                 return (fp, fs, hash)
