{-# LANGUAGE DuplicateRecordFields #-}

module Network.Dropbox.API
  ( Manager
  , newManager
  , Path
  , Metadata(..)
  , SymlinkInfo(..)
  , GetMetadataArg(..)
  , getMetadataArg
  , getMetadata
  , ListFolderArg(..)
  , listFolderArg
  , listFolder
  , UploadFileArg(..)
  , uploadFileArg
  -- , uploadFiles
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Conduit
import Data.Conduit.Combinators (concat)
import Data.Conduit.List (unfoldM)
import qualified Data.Foldable as F
import Data.Int
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.Dropbox.API.Basic
import Prelude hiding (concat)

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
             F.asum [ guard (tag == String "file")
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

listFolder :: Manager -> ListFolderArg -> ConduitT () Metadata IO ()
listFolder mgr arg = listChunks .| concat
  where
    listChunks :: ConduitT () (V.Vector Metadata) IO ()
    listChunks = do
      (mds, state) <- liftIO listInit
      yield mds
      unfoldM listNext state
    listInit :: IO (V.Vector Metadata, (T.Text, Bool))
    listInit = do
      result <- apiCall mgr "/2/files/list_folder" arg
      return (entries result, (cursor result, hasMore result))
    listNext :: (T.Text, Bool) -> IO (Maybe (V.Vector Metadata, (T.Text, Bool)))
    listNext (_, False) = return Nothing
    listNext (prevCursor, True) = do
      let nextArg = object [ "cursor" .= prevCursor ]
      result <- apiCall mgr "/2/files/list_folder/continue" nextArg
      return $ Just (entries result, (cursor result, hasMore result))

--------------------------------------------------------------------------------

data UploadFileArg = UploadFileArg { localPath :: FilePath
                                   , path :: Path
                                   , mode :: WriteMode
                                   , autorename :: Bool
                                   }
  deriving (Eq, Ord, Read, Show)

instance ToJSON UploadFileArg where
  toJSON val = object [ "path" .= path (val :: UploadFileArg)
                      , "mode" .= mode val
                      , "autorename" .= autorename val
                      ]

uploadFileArg :: FilePath -> Path -> UploadFileArg
uploadFileArg fp path = UploadFileArg fp path writeMode False

data WriteMode = Add | Overwrite | Update T.Text
  deriving (Eq, Ord, Read, Show)

instance ToJSON WriteMode where
  toJSON Add = String "add"
  toJSON Overwrite = String "overwrite"
  toJSON (Update rev) = object [ ".tag" .= String "udpate"
                               , "update" .= String rev
                               ]

writeMode :: WriteMode
writeMode = Add

-- uploadFiles :: Manager -> [UploadFileArg] -> IO [Maybe Metadata]
-- uploadFiles mgr arg =
--   do _
