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
  , Timestamp(..)
  , Metadata(..)
  , SymlinkInfo(..)
  , CopyArg(..)
  , CopyResult(..)
  , copy
  , CreateFolderArg(..)
  , CreateFolderResult(..)
  , createFolder
  , DeleteArg(..)
  , DeleteResult(..)
  , delete
  , GetMetadataArg(..)
  , getMetadataArg
  , getMetadata
  , ListFolderArg(..)
  , listFolderArg
  , listFolder
  , MoveArg(..)
  , MoveResult(..)
  , move
  , UploadArg(..)
  , uploadArg
  , WriteMode(..)
  , writeMode
  , UploadResult(..)
  , upload
  ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Fixed
import Data.Foldable hiding (toList)
import Data.Int
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
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

newtype Timestamp = Timestamp UTCTime
  deriving (Eq, Ord, Read, Show)

timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M:%SZ"

instance FromJSON Timestamp where
  parseJSON = withText "Timestamp"
    \str -> return
            $ Timestamp
            $ parseTimeOrError False defaultTimeLocale timeFormat (T.unpack str)

instance ToJSON Timestamp where
  toJSON (Timestamp time) =
    String $ T.pack $ formatTime defaultTimeLocale timeFormat time



data Metadata = FileMetadata { name :: Path
                             , identifier :: Path
                             , clientModified :: Timestamp
                             , serverModified :: Timestamp
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
                    <*> v .: "client_modified"
                    <*> v .: "server_modified"
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

data CopyArg = CopyArg { fromPath :: !Path
                       , toPath :: !Path
                       }
  deriving (Eq, Ord, Read, Show)

instance ToJSON CopyArg where
  toJSON val = object [ "from_path" .= (String $ fromPath (val :: CopyArg))
                      , "to_path" .= (String $ toPath (val :: CopyArg))
                      ]

data CopyCheckResult = CComplete { entries :: [CopyResult] }
                     | CAsyncJobId { asyncJobId :: !T.Text }
                     | CInProgress
  deriving (Eq, Ord, Read, Show)

instance FromJSON CopyCheckResult where
  parseJSON = withObject "CopyCheckResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "complete")
                    >> CComplete <$> v .: "entries"
                  , guard (tag == String "async_job_id")
                    >> CAsyncJobId <$> v .: "async_job_id"
                  , guard (tag == String "in_progress")
                    >> return CInProgress
                  ]

data CopyResult = CopyResult !Metadata
                | CopyError T.Text
  deriving (Eq, Ord, Read, Show)

instance FromJSON CopyResult where
  parseJSON = withObject "CopyResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "success")
                    >> CopyResult <$> v .: "success"
                  , guard (tag == String "failure")
                    >> CopyError <$> v .: "failure"
                  ]

copy :: ScreenManager -> Manager -> Serial CopyArg -> Serial CopyResult
copy smgr mgr args =
  S.concatMap S.fromList
  $ S.mapM copyBatch
  $ S.chunksOf copyBatchSize FL.toList
  $ args
  where
    copyBatchSize = 1000 :: Int
    copyBatch :: [CopyArg] -> IO [CopyResult]
    copyBatch pairs
      | null pairs = return []
      | otherwise = do
          t0 <- liftIO getCurrentTime
          let arg = object [ "entries" .= pairs
                           , "autorename" .= False
                           ]
          result <- withActive smgr (progressMsg pairs t0 t0)
                    $ apiCall mgr "/2/files/copy_batch_v2" arg
          case result of
            CComplete entries -> return entries
            CAsyncJobId asyncJobId -> untilJust do
              t <- liftIO getCurrentTime
              withActive smgr (progressMsg pairs t0 t) do
                let arg' = object [ "async_job_id" .= asyncJobId ]
                result' <- apiCall mgr "/2/files/copy_batch/check_v2" arg'
                case result' of
                  CComplete entries -> return $ Just entries
                  CInProgress -> do threadDelay (1000 * 1000)
                                    return Nothing
                  CAsyncJobId{} -> undefined
            CInProgress -> undefined
    progressMsg pairs t0 t =
      let dt :: Float =
            realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime t t0
      in ( T.pack (printf "[coyping %d files (waiting %.1f s)]"
                   (length pairs) dt)
         , "")

--------------------------------------------------------------------------------

newtype CreateFolderArg = CreateFolderArg { path :: Path }
  deriving (Eq, Ord, Read, Show)

instance ToJSON CreateFolderArg where
  toJSON val = String $ path (val :: CreateFolderArg)

data CreateFolderCheckResult = CFComplete { entries :: [CreateFolderResult] }
                             | CFAsyncJobId { asyncJobId :: !T.Text }
                             | CFInProgress
  deriving (Eq, Ord, Read, Show)

instance FromJSON CreateFolderCheckResult where
  parseJSON = withObject "CreateFolderCheckResult"
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

createFolder :: ScreenManager -> Manager
             -> Serial CreateFolderArg -> Serial CreateFolderResult
createFolder smgr mgr args =
  S.concatMap S.fromList
  $ S.mapM createFolders
  $ S.chunksOf createFoldersBatchSize FL.toList
  $ args
  where
    createFoldersBatchSize = 1000 :: Int
    createFolders :: [CreateFolderArg] -> IO [CreateFolderResult]
    createFolders paths
      | null paths = return []
      | otherwise = do
          t0 <- liftIO getCurrentTime
          let arg = object [ "paths" .= paths ]
          result <- withActive smgr (progressMsg paths t0 t0)
                    $ apiCall mgr "/2/files/create_folder_batch" arg
          case result of
            CFComplete entries -> return entries
            CFAsyncJobId asyncJobId -> untilJust do
              t <- liftIO getCurrentTime
              withActive smgr (progressMsg paths t0 t) do
                let arg' = object [ "async_job_id" .= asyncJobId ]
                result' <- apiCall mgr "/2/files/create_folder_batch/check" arg'
                case result' of
                  CFComplete entries -> return $ Just entries
                  CFInProgress -> do threadDelay (100 * 1000)
                                     return Nothing
                  CFAsyncJobId{} -> undefined
            CFInProgress -> undefined
    progressMsg paths t0 t =
      let dt :: Float =
            realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime t t0
      in ( T.pack
           $ printf "[creating %d folders (waiting %.1f s)]" (length paths) dt
         , "")

--------------------------------------------------------------------------------

newtype DeleteArg = DeleteArg { path :: Path }
  deriving (Eq, Ord, Read, Show)

instance ToJSON DeleteArg where
  toJSON val = object ["path" .= (String $ path (val :: DeleteArg))]

data DeleteCheckResult = DComplete { entries :: [DeleteResult] }
                       | DAsyncJobId { asyncJobId :: !T.Text }
                       | DInProgress
  deriving (Eq, Ord, Read, Show)

instance FromJSON DeleteCheckResult where
  parseJSON = withObject "DeleteCheckResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "complete")
                    >> DComplete <$> v .: "entries"
                  , guard (tag == String "async_job_id")
                    >> DAsyncJobId <$> v .: "async_job_id"
                  , guard (tag == String "in_progress")
                    >> return DInProgress
                  ]

data DeleteResult = DeleteResult Metadata
                  | DeleteError T.Text
  deriving (Eq, Ord, Read, Show)

instance FromJSON DeleteResult where
  parseJSON = withObject "DeleteResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "success")
                    >> DeleteResult <$> v .: "metadata"
                  , guard (tag == String "failure")
                    >> DeleteError <$> v .: "failure"
                  ]

delete :: ScreenManager -> Manager -> Serial DeleteArg -> Serial DeleteResult
delete smgr mgr args =
  S.concatMap S.fromList
  $ S.mapM deleteBatch
  $ S.chunksOf deleteBatchSize FL.toList
  $ args
  where
    deleteBatchSize = 1000 :: Int
    deleteBatch :: [DeleteArg] -> IO [DeleteResult]
    deleteBatch paths
      | null paths = return []
      | otherwise = do
          t0 <- liftIO getCurrentTime
          let arg = object [ "entries" .= paths ]
          result <- withActive smgr (progressMsg paths t0 t0)
                    $ apiCall mgr "/2/files/delete_batch" arg
          case result of
            DComplete entries -> return entries
            DAsyncJobId asyncJobId -> untilJust do
              t <- liftIO getCurrentTime
              withActive smgr (progressMsg paths t0 t) do
                let arg' = object [ "async_job_id" .= asyncJobId ]
                result' <- apiCall mgr "/2/files/delete_batch/check" arg'
                case result' of
                  DComplete entries -> return $ Just entries
                  DInProgress -> do threadDelay (100 * 1000)
                                    return Nothing
                  DAsyncJobId{} -> undefined
            DInProgress -> undefined
    progressMsg paths t0 t =
      let dt :: Float =
            realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime t t0
      in ( T.pack (printf "[deleting %d files/folders (waiting %.1f s)]"
                   (length paths) dt)
         , "")

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

data ListFolderArg = ListFolderArg { path :: !Path
                                   , recursive :: !Bool
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

listFolder :: ScreenManager -> Manager -> ListFolderArg -> Serial Metadata
listFolder smgr mgr arg = S.concatMap S.fromFoldable listChunks
  where
    listChunks :: Serial (V.Vector Metadata)
    listChunks = do
      t0 <- liftIO getCurrentTime
      (mds, state) <- liftIO (listInit t0)
      S.yield mds <> S.unfoldrM (listNext t0) state
    listInit :: UTCTime -> IO (V.Vector Metadata, (T.Text, Int, Bool))
    listInit t0 = do
      let count = 0 :: Int
      result <- withActive smgr (progressMsg count t0 t0)
                $ apiCall mgr "/2/files/list_folder" arg
      let es = entries (result :: ListFolderResult)
      return (es, (cursor result, count + V.length es, hasMore result))
    listNext :: UTCTime
             -> (T.Text, Int, Bool)
             -> IO (Maybe (V.Vector Metadata, (T.Text, Int, Bool)))
    listNext _ (_, _, False) = return Nothing
    listNext t0 (prevCursor, count, True) = do
      t <- liftIO getCurrentTime
      let nextArg = object [ "cursor" .= prevCursor ]
      result <- withActive smgr (progressMsg count t0 t)
                $ apiCall mgr "/2/files/list_folder/continue" nextArg
      let es = entries (result :: ListFolderResult)
      return $ Just (es, (cursor result, count + length es, hasMore result))
    progressMsg c t0 t =
      let dt :: Float =
            realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime t t0
      in (T.pack $ printf "[listing folder (%d, waiting %.1f s)]" c dt, "")

--------------------------------------------------------------------------------

data MoveArg = MoveArg { fromPath :: !Path
                       , toPath :: !Path
                       }
  deriving (Eq, Ord, Read, Show)

instance ToJSON MoveArg where
  toJSON val = object [ "from_path" .= (String $ fromPath (val :: MoveArg))
                      , "to_path" .= (String $ toPath (val :: MoveArg))
                      ]

data MoveCheckResult = MComplete { entries :: [MoveResult] }
                     | MAsyncJobId { asyncJobId :: !T.Text }
                     | MInProgress
  deriving (Eq, Ord, Read, Show)

instance FromJSON MoveCheckResult where
  parseJSON = withObject "MoveCheckResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "complete")
                    >> MComplete <$> v .: "entries"
                  , guard (tag == String "async_job_id")
                    >> MAsyncJobId <$> v .: "async_job_id"
                  , guard (tag == String "in_progress")
                    >> return MInProgress
                  ]

data MoveResult = MoveResult !Metadata
                | MoveError T.Text
  deriving (Eq, Ord, Read, Show)

instance FromJSON MoveResult where
  parseJSON = withObject "MoveResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "success")
                    >> MoveResult <$> v .: "success"
                  , guard (tag == String "failure")
                    >> MoveError <$> v .: "failure"
                  ]

move :: ScreenManager -> Manager -> Serial MoveArg -> Serial MoveResult
move smgr mgr args =
  S.concatMap S.fromList
  $ S.mapM moveBatch
  $ S.chunksOf moveBatchSize FL.toList
  $ args
  where
    moveBatchSize = 1000 :: Int
    moveBatch :: [MoveArg] -> IO [MoveResult]
    moveBatch pairs
      | null pairs = return []
      | otherwise = do
          t0 <- liftIO getCurrentTime
          let arg = object [ "entries" .= pairs
                           , "autorename" .= False
                           ]
          result <- withActive smgr (progressMsg pairs t0 t0)
                    $ apiCall mgr "/2/files/move_batch_v2" arg
          case result of
            MComplete entries -> return entries
            MAsyncJobId asyncJobId -> untilJust do
              t <- liftIO getCurrentTime
              withActive smgr (progressMsg pairs t0 t) do
                let arg' = object [ "async_job_id" .= asyncJobId ]
                result' <- apiCall mgr "/2/files/move_batch/check_v2" arg'
                case result' of
                  MComplete entries -> return $ Just entries
                  MInProgress -> do threadDelay (100 * 1000)
                                    return Nothing
                  MAsyncJobId{} -> undefined
            MInProgress -> undefined
    progressMsg pairs t0 t =
      let dt :: Float =
            realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime t t0
      in ( T.pack (printf "[moving %d files (waiting %.1f s)]"
                   (length pairs) dt)
         , "")

--------------------------------------------------------------------------------

data UploadArg = UploadArg { localPath :: !FilePath
                           , localSize :: !FileOffset
                           , path :: !Path
                           , mode :: !WriteMode
                           , autorename :: !Bool
                           , clientModified :: !Timestamp
                           , mute :: !Bool
                           }
  deriving (Eq, Ord, Read, Show)

instance ToJSON UploadArg where
  toJSON val = object $
    [ "path" .= path (val :: UploadArg)
    , "mode" .= mode val
    , "autorename" .= autorename val
    , "client_modified" .= clientModified (val :: UploadArg)
    , "mute" .= mute val
    ]

uploadArg :: FilePath -> FileStatus -> Path -> UploadArg
uploadArg fp fs path =
  let size = fileSize fs
      mtime = Timestamp $ posixSecondsToUTCTime $ modificationTimeHiRes fs
  in UploadArg fp size path writeMode False mtime False

data WriteMode = Add | Overwrite | Update !T.Text
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

data UploadResult = UploadResult Metadata
                      | UploadError T.Text
  deriving (Eq, Ord, Read, Show)

instance FromJSON UploadResult where
  parseJSON = withObject "UploadResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "success")
                    >> UploadResult
                    <$> (FileMetadata
                         <$> v .: "name"
                         <*> v .: "id"
                         <*> v .: "client_modified"
                         <*> v .: "server_modified"
                         <*> v .: "size"
                         <*> v .:? "path_lower"
                         <*> v .:? "path_display"
                         <*> v .:? "symlink_info"
                         <*> v .: "content_hash")
                  , guard (tag == String "failure")
                    >> UploadError <$> v .: "failure"
                  ]

upload :: ScreenManager -> FileManager -> Manager
       -> Serial UploadArg -> Serial UploadResult
upload smgr fmgr mgr args =
  S.concatMap S.fromList
  $ S.mapM (uploadFinish smgr mgr)
  $ groupFiles
  $ asyncly . maxThreads 10 . S.mapM (uploadContent smgr fmgr mgr) . serially
  $ args
  where
    finishBatchCount = 1000 :: Int
    finishBatchBytes = 128 * 1024 * 1024 -- 128 MByte
    groupFiles :: (IsStream t, MonadAsync m)
               => t m (UploadArg, UploadCursor)
               -> t m [(UploadArg, UploadCursor)]
    groupFiles =
      S.map (fmap \(arg, cursor, _, _, _) -> (arg, cursor))
      . S.splitOnSuffix (\(_, _, _, _, finish) -> finish) FL.toList
      . S.postscanl' (\(_, _, count, size, _) (arg, cursor) ->
                        let count' = count + 1
                            size' = size + offset (cursor :: UploadCursor)
                            finish = count' >= finishBatchCount ||
                                     size' >= finishBatchBytes
                        in if finish
                           then (arg, cursor, 0, 0, True)
                           else (arg, cursor, count', size', False))
                     (undefined, undefined, 0, 0, True)



newtype UploadContentResult = UploadContentResult { sessionId :: T.Text }
  deriving (Eq, Ord, Read, Show)

instance FromJSON UploadContentResult where
  parseJSON = withObject "UploadContentResult"
    \v -> UploadContentResult <$> v .: "session_id"

uploadContent :: ScreenManager -> FileManager -> Manager
              -> UploadArg -> IO (UploadArg, UploadCursor)
uploadContent smgr fmgr mgr arg =
  bracket_ (waitOpenFile fmgr) (signalOpenFile fmgr)
  $ withBinaryFile (localPath arg) ReadMode \handle -> do
  (sessionId, fileOffset, closed) <- uploadStart handle
  (fileOffset, closed) <-
    iterateUntilM snd (uploadAppend handle sessionId) (fileOffset, closed)
  let cursor = UploadCursor sessionId fileOffset
  return (arg, cursor)
  where
    chunkSize :: Int
    -- chunkSize = 150 * 1024 * 1024 -- 150 MByte
    -- Use a smaller chunk size to avoid timeouts
    chunkSize = 16 * 1024 * 1024 -- 16 MByte
    uploadStart :: Handle -> IO (T.Text, Int64, Bool)
    uploadStart handle = do
      chunk <- BL.hGet handle chunkSize
      close <- hIsEOF handle
      let fileOffset = BL.length chunk
      let arg = object [ "close" .= close ]
      result <- withActive smgr (progressMsg fileOffset)
                $ sendContent mgr "/2/files/upload_session/start" arg
                $ chunk
      return (sessionId (result :: UploadContentResult), fileOffset, close)
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
      in ( T.pack $ printf "[uploading "
         , T.pack $ printf "%s (%.1f%%)]" (localPath arg) (percent off sz))
    percent n d = if n == 0
                  then 0
                  else 100 * fromIntegral n / fromIntegral d :: Float



data UploadFinishCheckResult = UFComplete { entries :: [UploadResult] }
                             | UFAsyncJobId { asyncJobId :: !T.Text }
                             | UFInProgress
  deriving (Eq, Ord, Read, Show)

instance FromJSON UploadFinishCheckResult where
  parseJSON = withObject "UploadFinishCheckResult"
    \v -> do tag <- v .: ".tag"
             asum [ guard (tag == String "complete")
                    >> UFComplete <$> v .: "entries"
                  , guard (tag == String "async_job_id")
                    >> UFAsyncJobId <$> v .: "async_job_id"
                  , guard (tag == String "in_progress")
                    >> return UFInProgress
                  ]

uploadFinish :: ScreenManager -> Manager
             -> [(UploadArg, UploadCursor)] -> IO [UploadResult]
uploadFinish smgr mgr uploads
  | null uploads = return []
  | otherwise = do
      t0 <- liftIO getCurrentTime
      let arg = object [ "entries" .= [ object [ "cursor" .= cursor
                                               , "commit" .= fileArg
                                               ]
                                      | (fileArg, cursor) <- uploads
                                      ]
                       ]
      result <- bracket_ (waitUploadFinish mgr) (signalUploadFinish mgr)
                $ withActive smgr (progressMsg uploads t0 t0)
                $ apiCall mgr "/2/files/upload_session/finish_batch" arg
      case result of
        UFComplete entries -> return entries
        UFAsyncJobId asyncJobId -> untilJust do
          t <- liftIO getCurrentTime
          withActive smgr (progressMsg uploads t t0) do
            let arg' = object [ "async_job_id" .= asyncJobId ]
            result' <-
              apiCall mgr "/2/files/upload_session/finish_batch/check" arg'
            case result' of
              UFComplete entries -> return $ Just entries
              UFInProgress -> do threadDelay (100 * 1000)
                                 return Nothing
              UFAsyncJobId{} -> undefined
        UFInProgress -> undefined
  where
    progressMsg uploads t0 t =
      let dt :: Float =
            realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime t t0
      in ( T.pack (printf "[finalizing upload of %d files (waiting %.1f s)]"
                   (length uploads) dt)
         , "")



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
