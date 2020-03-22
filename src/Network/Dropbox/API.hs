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
                                   , path :: Path
                                   , mode :: WriteMode
                                   , autorename :: Bool
                                   , mute :: Bool
                                   }
  deriving (Eq, Ord, Read, Show, Generic, NFData)

instance ToJSON UploadFileArg where
  toJSON val = object [ "path" .= path (val :: UploadFileArg)
                      , "mode" .= mode val
                      , "autorename" .= autorename val
                      , "mute" .= mute val
                      ]

uploadFileArg :: FilePath -> Path -> UploadFileArg
uploadFileArg fp path = UploadFileArg fp path writeMode False False

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

data UploadState = UploadState { content :: BL.ByteString
                               , offset :: !Int64
                               , count :: !Int
                               }
  deriving (Eq, Ord, Read, Show, Generic, NFData)

splitUploadState :: Int64 -> UploadState -> (UploadState, UploadState)
splitUploadState size (UploadState content coffset count) =
  let (hd, tl) = BL.splitAt size content
  in (UploadState hd coffset count,
      UploadState tl (coffset + BL.length hd) (count + 1))

uploadStateDone :: UploadState -> Bool
uploadStateDone (UploadState content coffset count) = BL.null content

newtype UploadResult = UploadResult { sessionId :: T.Text }
  deriving (Eq, Ord, Read, Show)

instance FromJSON UploadResult where
  parseJSON = withObject "UploadResult" \v -> UploadResult <$> v .: "session_id"

data UploadCursor = UploadCursor { sessionId :: !T.Text
                                 , offset :: !Int64
                                 }
  deriving (Eq, Ord, Read, Show, Generic, NFData)

instance ToJSON UploadCursor where
  toJSON (UploadCursor sessionId offset) = object [ "session_id" .= sessionId
                                                  , "offset" .= offset
                                                  ]

data UploadCount = UploadCount { fileCount :: !Int
                               , fileSize :: !Int64
                               , finishGroup :: !Bool
                               }
  deriving (Eq, Ord, Read, Show, Generic, NFData)

initUploadCount :: UploadCount
initUploadCount = UploadCount 0 0 True

foldUploadCount :: UploadCount -> (UploadFileArg, UploadCursor) -> UploadCount
foldUploadCount (UploadCount count size _) (_, cursor) =
  let count' = count + 1
      size' = size + offset (cursor :: UploadCursor)
      finish = count' >= finishBatchSize || size' >= finishBatchFileSize
  in if finish then initUploadCount else UploadCount count' size' False
  where
    -- finishBatchSize = 1000 :: Int
    finishBatchSize = 100 :: Int
    finishBatchFileSize = 150 * 1000 * 1000

finishUpload :: ((UploadFileArg, UploadCursor), UploadCount) -> Bool
finishUpload (_, UploadCount _ _ finish) = finish

data UploadFinishResult = UFComplete { entries :: [UploadFileResult] }
                        | UFAsyncJobId { asyncJobId :: T.Text }
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

uploadFiles :: ScreenManager -> FileManager -> Manager
            -> Serial UploadFileArg -> Serial UploadFileResult
uploadFiles smgr fmgr mgr args =
  S.concatMap S.fromList
  $ S.mapM uploadFinish
  $ groupFiles
  -- TODO: runs out of memory for parallel uploads; need ByteString.copy?
  -- $ asyncly . maxThreads 10 . S.mapM uploadFile
  $ serially . S.mapM uploadFile
  $ serially $ args
  where
    -- Large requests take a long time to process, and the default
    -- request timeout is only 30 seconds. A timed-out request might
    -- still be processed correctly, and it's then difficult to find
    -- out whether this is the case.
    -- requestSize = 150 * 1024 * 1024 :: Int64 -- 150 MByte
    requestSize = 15 * 1024 * 1024 :: Int64 -- 15 MByte
    -- finishBatchSize = 1000 :: Int
    finishBatchSize = 100 :: Int
    finishBatchFileSize = 150 * 1000 * 1000
    percent n d = 100 * fromIntegral n / fromIntegral d :: Float
    uploadFile :: UploadFileArg -> IO (UploadFileArg, UploadCursor)
    uploadFile arg =
      bracket_
      (waitOpenFile fmgr)
      (signalOpenFile fmgr)
      do file <- BL.readFile (localPath arg)
         let uploadState0 = UploadState file 0 0
         (sessionId, uploadState1) <- uploadStart uploadState0
         uploadState2 <-
           iterateUntilM uploadStateDone (uploadAppend sessionId) uploadState1
         let cursor =
               UploadCursor sessionId (offset (uploadState2 :: UploadState))
         evaluate $ force (arg, cursor)
    uploadStart :: UploadState -> IO (T.Text, UploadState)
    uploadStart uploadState = do
      let (headUploadState, tailUploadState) =
            splitUploadState requestSize uploadState
      assert (offset (headUploadState :: UploadState) == 0) $ return ()
      let arg = object [ "close" .= uploadStateDone tailUploadState ]
      let off = 0
      let sz = off + BL.length (content uploadState)
      let msg = printf "[uploading %d/%d (%.1f%%)]" off sz (percent off sz)
      result <- withActive smgr (T.pack msg)
                $ sendContent mgr "/2/files/upload_session/start" arg
                $ content headUploadState
      evaluate $ force (sessionId (result :: UploadResult), tailUploadState)
    uploadAppend :: T.Text -> UploadState -> IO UploadState
    uploadAppend sessionId uploadState = do
      let (headUploadState, tailUploadState) =
            splitUploadState requestSize uploadState
      let cursor =
            UploadCursor sessionId (offset (headUploadState :: UploadState))
      let arg = object [ "cursor" .= cursor
                       , "close" .= uploadStateDone tailUploadState
                       ]
      let off = offset (cursor :: UploadCursor)
      let sz = off + BL.length (content uploadState)
      let msg = printf "[uploading %d/%d (%.1f%%)]" off sz (percent off sz)
      value <- withActive smgr (T.pack msg)
               $ sendContent mgr "/2/files/upload_session/append_v2" arg
               $ content headUploadState
      evaluate (value :: Value) -- wait for upload to complete
      evaluate $ force tailUploadState
    groupFiles :: (IsStream t, MonadAsync m)
               => t m (UploadFileArg, UploadCursor)
               -> t m [(UploadFileArg, UploadCursor)]
    groupFiles files =
      S.map (fmap \(arg, cursor, _, _, _) -> (arg, cursor))
      $ S.splitOnSuffix (\(_, _, _, _, finish) -> finish) FL.toList
      $ S.postscanl' (\(_, _, count, size, _) (arg, cursor) ->
                        let count' = count + 1
                            size' = size + offset (cursor :: UploadCursor)
                            finish = count' >= finishBatchSize ||
                                     size' >= finishBatchFileSize
                        in if finish
                           then (arg, cursor, 0, 0, True)
                           else (arg, cursor, count', size', False))
                     (undefined, undefined, 0, 0, True)
      $ files
    uploadFinish :: [(UploadFileArg, UploadCursor)] -> IO [UploadFileResult]
    uploadFinish uploads
      | null uploads = return []
      | otherwise = do
          assert (not (null uploads)) $ return ()
          let arg = object [ "entries" .= [ object [ "cursor" .= cursor
                                                   , "commit" .= fileArg
                                                   ]
                                          | (fileArg, cursor) <- uploads
                                          ]
                           ]
          result <- bracket_
                    (waitUploadFinish mgr)
                    (signalUploadFinish mgr)
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
