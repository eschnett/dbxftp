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

{-# LANGUAGE DuplicateRecordFields #-}

module Network.Dropbox.API
  ( Manager
  , newManager
  , DbxException(..)
  , Path
  , Metadata(..)
  , SymlinkInfo(..)
  , CreateFolderArg(..)
  , CreateFolderResult(..)
  , createFolder
  , GetMetadataArg(..)
  , getMetadataArg
  , getMetadata
  , ListFolderArg(..)
  , listFolderArg
  , listFolder
  , UploadFileArg(..)
  , uploadFileArg
  , WriteMode(..)
  , writeMode
  , UploadFileResult(..)
  , uploadFiles
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Foldable hiding (toList)
import Data.Int
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import Network.Dropbox.API.Basic
import Network.Dropbox.Filesystem
import Network.Dropbox.Progress
import Prelude hiding (concat)
import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Prelude as S
import System.IO
import System.Posix
import Text.Printf

--------------------------------------------------------------------------------

type Path = T.Text

data Metadata = FileMetadata { name :: Path
                             , identifier :: Path
                             , size :: Int64
                             , pathLower :: Maybe Path
                             , pathDisplay :: Maybe Path
                             , symlinkInfo :: Maybe SymlinkInfo
                             , contentHash :: T.Text
                             }
              | FolderMetadata { name :: Path
                               , identifier :: Path
                               , pathLower :: Maybe Path
                               , pathDisplay :: Maybe Path
                               }
              | DeletedMetadata { name :: Path
                                , pathLower :: Maybe Path
                                , pathDisplay :: Maybe Path
                                }
              | NoMetadata
  deriving (Eq, Ord, Read, Show)

instance FromJSON Metadata where
  parseJSON = withObject "Metadata"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "file")
                    >> FileMetadata
                    <$> v .: "name"
                    <*> v .: "id"
                    <*> v .: "size"
                    <*> v .:? "path_lower"
                    <*> v .:? "path_display"
                    <*> v .:? "symlink_info"
                    <*> v .: "content_hash"
                  , guard (tag == String "folder")
                    >> FolderMetadata
                    <$> v .: "name"
                    <*> v .: "id"
                    <*> v .:? "path_lower"
                    <*> v .:? "path_display"
                  , guard (tag == String "deleted")
                    >> DeletedMetadata
                    <$> v .: "name"
                    <*> v .:? "path_lower"
                    <*> v .:? "path_display"
                  , return NoMetadata
                  ]

newtype SymlinkInfo = SymlinkInfo { target :: Path }
  deriving (Eq, Ord, Read, Show)

instance FromJSON SymlinkInfo where
  parseJSON = withObject "SymlinkInfo"
    \v -> SymlinkInfo <$> v .: "target"

--------------------------------------------------------------------------------

newtype CreateFolderArg = CreateFolderArg { path :: Path }
  deriving (Eq, Ord, Read, Show)

instance ToJSON CreateFolderArg where
  toJSON val = String $ path (val :: CreateFolderArg)

data CreateFolderFinishResult = CFComplete { entries :: [CreateFolderResult] }
                              | CFAsyncJobId { asyncJobId :: T.Text }
                              | CFInProgress
  deriving (Eq, Ord, Read, Show)

instance FromJSON CreateFolderFinishResult where
  parseJSON = withObject "CreateFolderFinishResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "complete")
                    >> CFComplete <$> v .: "entries"
                  , guard (tag == String "async_job_id")
                    >> CFAsyncJobId <$> v .: "async_job_id"
                  , guard (tag == String "in_progress")
                    >> return CFInProgress
                  ]

data CreateFolderResult = CreateFolderResult Metadata
                        | CreateFolderError T.Text
  deriving (Eq, Ord, Read, Show)

instance FromJSON CreateFolderResult where
  parseJSON = withObject "CreateFolderResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "success")
                    -- >> CreateFolderResult
                    -- <$> (FolderMetadata
                    --      <$> v .: "name"
                    --      <*> v .: "id"
                    --      <*> v .:? "path_lower"
                    --      <*> v .:? "path_display")
                    >> CreateFolderResult
                    <$> (do md <- v .: "metadata"
                            (FolderMetadata
                             <$> md .: "name"
                             <*> md .: "id"
                             <*> md .:? "path_lower"
                             <*> md .:? "path_display"))
                  , guard (tag == String "failure")
                    >> CreateFolderError <$> v .: "failure"
                  ]

createFolder :: Manager -> Async CreateFolderArg -> Async CreateFolderResult
createFolder mgr args = do
  S.concatMap S.fromList
  |$ S.mapM createFolders
  |$ S.chunksOf createFoldersBatchSize FL.toList
  |$ args
  where
    createFoldersBatchSize = 1000 :: Int
    createFolders :: [CreateFolderArg] -> IO [CreateFolderResult]
    createFolders paths
      | null paths = return []
      | otherwise = do
          let arg = object [ "paths" .= paths ]
          result <- apiCall mgr "/2/files/create_folder_batch" arg
          case result of
            CFComplete entries -> return entries
            CFAsyncJobId asyncJobId -> untilJust do
              let arg' = object [ "async_job_id" .= asyncJobId ]
              result' <- apiCall mgr "/2/files/create_folder_batch/check" arg'
              case result' of
                CFComplete entries -> return $ Just entries
                CFInProgress -> return Nothing
                CFAsyncJobId{} -> undefined
            CFInProgress -> undefined

--------------------------------------------------------------------------------

newtype GetMetadataArg = GetMetadataArg { path :: Path }
  deriving (Eq, Ord, Read, Show)

instance ToJSON GetMetadataArg where
  toJSON val = object [ "path" .= path (val :: GetMetadataArg)
                      ]

getMetadataArg :: GetMetadataArg 
getMetadataArg = GetMetadataArg { path = "" }

getMetadata :: Manager -> GetMetadataArg -> IO Metadata
getMetadata mgr arg =
  apiCall mgr "/2/files/get_metadata" arg

--------------------------------------------------------------------------------

data ListFolderArg = ListFolderArg { path :: Path
                                   , recursive :: Bool
                                   }
  deriving (Eq, Ord, Read, Show)

instance ToJSON ListFolderArg where
  toJSON val = object [ "path" .= path (val :: ListFolderArg)
                      , "recursive" .= recursive val
                      ]

listFolderArg :: ListFolderArg 
listFolderArg = ListFolderArg { path = "", recursive = False }

data ListFolderResult = ListFolderResult { entries :: V.Vector Metadata
                                         , cursor :: T.Text
                                         , hasMore :: Bool
                                         }
  deriving (Eq, Ord, Read, Show)

instance FromJSON ListFolderResult where
  parseJSON = withObject "ListFolderResult"
    \v -> ListFolderResult
    <$> v .: "entries"
    <*> v .: "cursor"
    <*> v .: "has_more"

listFolder :: Manager -> ListFolderArg -> Serial Metadata
listFolder mgr arg = S.concatMap S.fromFoldable listChunks
  where
    listChunks :: Serial (V.Vector Metadata)
    listChunks = do
      (mds, state) <- liftIO listInit
      S.yield mds <> S.unfoldrM listNext state
    listInit :: IO (V.Vector Metadata, (T.Text, Bool))
    listInit = do
      result <- apiCall mgr "/2/files/list_folder" arg
      return ( entries (result :: ListFolderResult)
             , (cursor result, hasMore result))
    listNext :: (T.Text, Bool) -> IO (Maybe (V.Vector Metadata, (T.Text, Bool)))
    listNext (_, False) = return Nothing
    listNext (prevCursor, True) = do
      let nextArg = object [ "cursor" .= prevCursor ]
      result <- apiCall mgr "/2/files/list_folder/continue" nextArg
      return $ Just ( entries (result :: ListFolderResult)
                    , (cursor result, hasMore result))

--------------------------------------------------------------------------------

data UploadFileArg = UploadFileArg { localPath :: FilePath
                                   , localSize :: FileOffset
                                   , path :: Path
                                   , mode :: WriteMode
                                   , autorename :: Bool
                                   , mute :: Bool
                                   }
  deriving (Eq, Ord, Read, Show)

instance ToJSON UploadFileArg where
  toJSON val = object [ "path" .= path (val :: UploadFileArg)
                      , "mode" .= mode val
                      , "autorename" .= autorename val
                      , "mute" .= mute val
                      ]

uploadFileArg :: FilePath -> FileOffset -> Path -> UploadFileArg
uploadFileArg fp fs path = UploadFileArg fp fs path writeMode False False

data WriteMode = Add | Overwrite | Update T.Text
  deriving (Eq, Ord, Read, Show, Generic, NFData)

instance ToJSON WriteMode where
  toJSON Add = String "add"
  toJSON Overwrite = String "overwrite"
  toJSON (Update rev) = object [ ".tag" .= String "udpate"
                               , "update" .= String rev
                               ]

writeMode :: WriteMode
writeMode = Add

data UploadCursor = UploadCursor { sessionId :: !T.Text
                                 , offset :: !Int64
                                 }
  deriving (Eq, Ord, Read, Show)

instance ToJSON UploadCursor where
  toJSON (UploadCursor sessionId offset) =
    object ["session_id" .= sessionId, "offset" .= offset]

data UploadFileResult = UploadFileResult Metadata
                      | UploadFileError T.Text
  deriving (Eq, Ord, Read, Show)

instance FromJSON UploadFileResult where
  parseJSON = withObject "UploadFileResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "success")
                    >> UploadFileResult
                    <$> (FileMetadata
                         <$> v .: "name"
                         <*> v .: "id"
                         <*> v .: "size"
                         <*> v .:? "path_lower"
                         <*> v .:? "path_display"
                         <*> v .:? "symlink_info"
                         <*> v .: "content_hash")
                  , guard (tag == String "failure")
                    >> UploadFileError <$> v .: "failure"
                  ]

uploadFiles :: ScreenManager -> FileManager -> Manager
            -> Serial UploadFileArg -> Serial UploadFileResult
uploadFiles smgr fmgr mgr args =
  S.concatMap S.fromList
  $ S.mapM (uploadFinish smgr mgr)
  $ groupFiles
  -- TODO: runs out of memory for parallel uploads; need ByteString.copy?
  $ asyncly . maxThreads 10 . S.mapM (uploadFile smgr fmgr mgr)
  -- $ serially . S.mapM (uploadFile smgr fmgr mgr)
  $ serially $ args
  where
    -- finishBatchCount = 1000 :: Int
    finishBatchCount = 100 :: Int
    finishBatchBytes = 100 * 1000 * 1000 -- 100 MByte
    groupFiles :: (IsStream t, MonadAsync m)
               => t m (UploadFileArg, UploadCursor)
               -> t m [(UploadFileArg, UploadCursor)]
    groupFiles files =
      S.map (fmap \(arg, cursor, _, _, _) -> (arg, cursor))
      $ S.splitOnSuffix (\(_, _, _, _, finish) -> finish) FL.toList
      $ S.postscanl' (\(_, _, count, size, _) (arg, cursor) ->
                        let count' = count + 1
                            size' = size + offset (cursor :: UploadCursor)
                            finish = count' >= finishBatchCount ||
                                     size' >= finishBatchBytes
                        in if finish
                           then (arg, cursor, 0, 0, True)
                           else (arg, cursor, count', size', False))
                     (undefined, undefined, 0, 0, True)
      $ files



newtype UploadResult = UploadResult { sessionId :: T.Text }
  deriving (Eq, Ord, Read, Show)

instance FromJSON UploadResult where
  parseJSON = withObject "UploadResult" \v -> UploadResult <$> v .: "session_id"

uploadFile :: ScreenManager -> FileManager -> Manager
           -> UploadFileArg -> IO (UploadFileArg, UploadCursor)
uploadFile smgr fmgr mgr arg =
  bracket_ (waitOpenFile fmgr) (signalOpenFile fmgr)
  $ withFile (localPath arg) ReadMode \handle -> do
  (sessionId, fileOffset, closed) <- uploadStart handle
  (fileOffset, closed) <-
    iterateUntilM snd (uploadAppend handle sessionId) (fileOffset, closed)
  let cursor = UploadCursor sessionId fileOffset
  return (arg, cursor)
  where
    chunkSize :: Int
    -- chunkSize = 150 * 1000 * 1000 -- 150 MByte
    -- Use a smaller chunk size to avoid timeouts
    chunkSize = 15 * 1000 * 1000 -- 150MByte
    uploadStart :: Handle -> IO (T.Text, Int64, Bool)
    uploadStart handle = do
      chunk <- BL.hGet handle chunkSize
      close <- hIsEOF handle
      let fileOffset = BL.length chunk
      let arg = object [ "close" .= close ]
      result <- withActive smgr (progressMsg fileOffset)
                $ sendContent mgr "/2/files/upload_session/start" arg
                $ chunk
      return (sessionId (result :: UploadResult), fileOffset, close)
    uploadAppend :: Handle -> T.Text
                 -> (Int64, Bool) -> IO (Int64, Bool)
    uploadAppend handle sessionId (fileOffset, _) = do
      chunk <- BL.hGet handle chunkSize
      close <- hIsEOF handle
      let fileOffset' = fileOffset + BL.length chunk
      let cursor = UploadCursor sessionId fileOffset
      let arg = object ["cursor" .= cursor, "close" .= close]
      value <- withActive smgr (progressMsg fileOffset')
               $ sendContent mgr "/2/files/upload_session/append_v2" arg
               $ chunk
      evaluate (value :: Value) -- wait for upload to complete
      return (fileOffset', close)
    progressMsg fileOffset =
      let off = fromIntegral fileOffset :: Int64
          sz = fromIntegral (localSize arg) :: Int64
      in T.pack $ printf "[uploading %d/%d (%.1f%%)]" off sz (percent off sz)
    percent n d = 100 * fromIntegral n / fromIntegral d :: Float



data UploadFinishResult = UFComplete { entries :: [UploadFileResult] }
                        | UFAsyncJobId { asyncJobId :: !T.Text }
                        | UFInProgress
  deriving (Eq, Ord, Read, Show)

instance FromJSON UploadFinishResult where
  parseJSON = withObject "UploadFinishResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "complete")
                    >> UFComplete <$> v .: "entries"
                  , guard (tag == String "async_job_id")
                    >> UFAsyncJobId <$> v .: "async_job_id"
                  , guard (tag == String "in_progress")
                    >> return UFInProgress
                  ]

uploadFinish :: ScreenManager -> Manager
             -> [(UploadFileArg, UploadCursor)] -> IO [UploadFileResult]
uploadFinish smgr mgr uploads
  | null uploads = return []
  | otherwise = do
      let arg = object [ "entries" .= [ object [ "cursor" .= cursor
                                               , "commit" .= fileArg
                                               ]
                                      | (fileArg, cursor) <- uploads
                                      ]
                       ]
      result <- bracket_ (waitUploadFinish mgr) (signalUploadFinish mgr)
                $ withActive smgr "[finalizing upload]"
                $ apiCall mgr "/2/files/upload_session/finish_batch" arg
      case result of
        UFComplete entries -> return entries
        UFAsyncJobId asyncJobId -> untilJust do
          let arg' = object [ "async_job_id" .= asyncJobId ]
          result' <-
            apiCall mgr "/2/files/upload_session/finish_batch/check" arg'
          case result' of
            UFComplete entries -> return $ Just entries
            UFInProgress -> return Nothing
            UFAsyncJobId{} -> undefined
        UFInProgress -> undefined



-- import Streamly
-- import qualified Streamly.Internal.Prelude as S
-- import qualified Streamly.Internal.Data.Fold as FL
-- import qualified Streamly.Internal.Data.Parse as PR
-- import qualified Streamly.Internal.FileSystem.Handle as FH
-- import qualified Streamly.Internal.Data.Unicode.Stream as U
-- 
-- {-# INLINE chunksOfTimeout #-}
-- chunksOfTimeout :: (IsStream t, MonadAsync m)
--     => Int -> Double -> FL.Fold m a b -> t m a -> t m b
-- chunksOfTimeout n t f =
--       S.splitWithSuffix isNothing (FL.lcatMaybes f)
--     . S.interjectSuffix t (return Nothing)
--     . S.intersperseSuffixBySpan n (return Nothing)
--     . S.map Just
