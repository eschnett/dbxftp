{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- <https://www.dropbox.com/developers/documentation/http/overview>

module DBXFTP
  ( AppState
  , newAppState
  , ls
  , put
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.QSemN
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (iterateUntilM, untilJust)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Aeson.Types (listValue)
import Data.Function ((&))
import qualified Data.HashMap.Strict as H
import Data.Int
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
import qualified Data.Vector as V
import Debug.Trace
import Network.HTTP.Client hiding (fileSize)
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import Streamly
import Streamly.Data.Fold (toList)
import qualified Streamly.Prelude as S
import System.FilePath.Posix
import System.IO.Unsafe
import System.Posix hiding (Null)



maxOpenFiles :: Int
maxOpenFiles = 100

maxOpenConnections :: Int
maxOpenConnections = 20



-- Don't open more than 100 files simultaneously

openFile :: IO ()
openFile = waitQSemN theOpenFiles 1

closeFile :: IO ()
closeFile = signalQSemN theOpenFiles 1

theOpenFiles :: QSemN
{-# NOINLINE theOpenFiles #-}
theOpenFiles = unsafePerformIO $ newQSemN maxOpenFiles



-- Don't exceed DropBox's connection rate limit

openConnection :: IO ()
openConnection = do waitQSemN theOpenConnections 1
                    readMVar theAvailableConnection

closeConnection :: IO ()
closeConnection = signalQSemN theOpenConnections 1

delayConnection :: Int64 -> IO ()
delayConnection d =
  do t0 <- systemToPOSIXTime <$> getSystemTime
     putStrLn $ "*** waiting for " ++ show d ++ " s"
     putStrLn "*** waiting for lock"
     () <- takeMVar theAvailableConnection
     putStrLn "*** took lock"
     let t1 = t0 + fromIntegral d
     t <- systemToPOSIXTime <$> getSystemTime
     let delay = round $ 1e6 * (t1 - t)
     putStrLn $ "*** waiting for " ++ show delay ++ " us"
     when (t1 > t) $ threadDelay delay
     putStrLn "*** releasing lock"
     putMVar theAvailableConnection ()

theAvailableConnection :: MVar ()
{-# NOINLINE theAvailableConnection #-}
theAvailableConnection = unsafePerformIO do newMVar ()

theOpenConnections :: QSemN
{-# NOINLINE theOpenConnections #-}
theOpenConnections = unsafePerformIO $ newQSemN maxOpenConnections



openUploadFinishConnection :: IO ()
openUploadFinishConnection = waitQSemN theOpenUploadFinishConnections 1

closeUploadFinishConnection :: IO ()
closeUploadFinishConnection = signalQSemN theOpenUploadFinishConnections 1

theOpenUploadFinishConnections :: QSemN
{-# NOINLINE theOpenUploadFinishConnections #-}
theOpenUploadFinishConnections = unsafePerformIO $ newQSemN 1



apiCall :: AppState -> S8.ByteString -> Value -> IO Value
apiCall app path args =
  let request =
        defaultRequest
        & setRequestManager (manager app)
        & setRequestSecure True
        & setRequestHost "api.dropboxapi.com"
        & setRequestPort 443
        & setRequestMethod "POST"
        & setRequestPath path
        & (addRequestHeader "Authorization"
           (S8.append "Bearer " (authToken app)))
        & addRequestHeader "Content-Type" "application/json"
        & setRequestBodyJSON args
        & setRequestIgnoreStatus
  in untilJust
     do resp <- bracket_ openConnection closeConnection $ httpLBS request
        let st = getResponseStatusCode resp
        let body = getResponseBody resp
        if | st == 429
             -> do putStrLn $ "Exceeded rate limit, waiting..."
                   putStrLn "body:"
                   putStrLn $ show body
                   putStrLn $ show (eitherDecode body :: Either String Value)
                   let Object obj = fromRight' (eitherDecode body)
                   putStrLn "obj:"
                   putStrLn $ show obj
                   let Object err = obj H.! "error"
                   let Object reason = err H.! "reason"
                   assertM $ reason H.! ".tag" == String "too_many_requests"
                   let Number retryAfter' = err H.! "retry_after"
                   putStrLn $ "retry after': " ++ show retryAfter'
                   let Just retryAfter = toBoundedInteger retryAfter'
                   putStrLn $ "retry after: " ++ show retryAfter
                   delayConnection retryAfter
                   return Nothing
           | st `div` 100 == 5 -- retry
             -> do putStrLn $ ("Received status code " ++ show st
                               ++ ", retrying...")
                   putStrLn $ show resp
                   return Nothing
           | st `div` 100 /= 2 -- abort
             -> do putStrLn $ ("Received status code " ++ show st
                               ++ ", aborting")
                   putStrLn $ show resp
                   let ex = StatusCodeException (void resp) (BL.toStrict body)
                   throw $ HttpExceptionRequest request ex
           | True
             -> do let retval = eitherDecode body
                   case retval of
                     Left err -> do putStrLn "JSON parse error"
                                    putStrLn $ show err
                                    -- throw $ JSONParseException request (void resp) err
                                    -- throw $ JSONConversionException request resp err
                                    throw $ AssertionFailed err
                     Right val -> return $ Just val

sendContent :: AppState -> S8.ByteString -> Value -> BL.ByteString -> IO Value
sendContent app path args content =
  let request =
        defaultRequest
        & setRequestManager (manager app)
        & setRequestSecure True
        & setRequestHost "content.dropboxapi.com"
        & setRequestPort 443
        & setRequestMethod "POST"
        & setRequestPath path
        & (addRequestHeader "Authorization"
           (S8.append "Bearer " (authToken app)))
        & addRequestHeader "Dropbox-API-Arg" (BL.toStrict (encode args))
        & addRequestHeader "Content-Type" "application/octet-stream"
        & setRequestBodyLBS content
        & setRequestIgnoreStatus
  in untilJust
     do resp <- bracket_ openConnection closeConnection $ httpLBS request
        let st = getResponseStatusCode resp
        let body = getResponseBody resp
        if | st == 429
             -> do putStrLn $ "Exceeded rate limit, waiting..."
                   putStrLn "body:"
                   putStrLn $ show body
                   putStrLn $ show (eitherDecode body :: Either String Value)
                   let Object obj = fromRight' (eitherDecode body)
                   putStrLn "obj:"
                   putStrLn $ show obj
                   let Object err = obj H.! "error"
                   let Object reason = err H.! "reason"
                   assertM $ reason H.! ".tag" == String "too_many_requests"
                   let Number retryAfter' = err H.! "retry_after"
                   putStrLn $ "retry after': " ++ show retryAfter'
                   let Just retryAfter = toBoundedInteger retryAfter'
                   putStrLn $ "retry after: " ++ show retryAfter
                   delayConnection retryAfter
                   return Nothing
           | st `div` 100 == 5 -- retry
             -> do putStrLn $ ("Received status code " ++ show st
                               ++ ", retrying...")
                   putStrLn $ show resp
                   return Nothing
           | st `div` 100 /= 2 -- abort
             -> do putStrLn $ ("Received status code " ++ show st
                               ++ ", aborting")
                   putStrLn $ show resp
                   let ex = StatusCodeException (void resp) (BL.toStrict body)
                   throw $ HttpExceptionRequest request ex
           | True
             -> do let retval = eitherDecode body
                   case retval of
                     Left err -> do putStrLn "JSON parse error"
                                    putStrLn $ show err
                                    -- throw $ JSONParseException request (void resp) err
                                    -- throw $ JSONConversionException request resp err
                                    throw $ AssertionFailed err
                     Right val -> return $ Just val

-- recvContent :: 

tryHttp :: forall a.
           (Int -> Value -> Maybe a) -> IO Value -> IO (Either a Value)
tryHttp test val = tryJust test' val
  where test' :: HttpException -> Maybe a
        test' (HttpExceptionRequest _req ex) =
          case ex of
            StatusCodeException resp body ->
              let st = getResponseStatusCode resp
                  mobj = eitherDecode (BL.fromStrict body)
              in case mobj of
                Left _err -> Nothing
                Right obj -> test st obj
            _ -> Nothing



data AppState = AppState { authToken :: S8.ByteString
                         , manager :: Manager
                         }
newAppState :: IO AppState
newAppState = do authToken <- getAuthToken
                 manager <- newManager tlsManagerSettings
                 return $ AppState authToken manager

getAuthToken :: IO S8.ByteString
getAuthToken = do home <- getEnvDefault "HOME" ""
                  let fp = home </> ".dbxftp.http"
                  token <- bracket_ openFile closeFile $ readFile fp
                  return $ parseToken $ S8.pack token
  where parseToken :: S8.ByteString -> S8.ByteString
        parseToken s = chomp $ S8.drop (S8.length "access_token:") s



ls :: AppState -> [FilePath] -> IO [FilePath]
ls app fps =
  S.toList
  $ asyncly
  $ maxThreads 10
  |$ S.concatMap (\fp -> do files <- liftIO (ls' app fp)
                            S.fromList files)
  |$ S.fromList fps

ls' :: AppState -> FilePath -> IO [FilePath]
ls' app fp =
  do let args = object [ "path" .= String (T.pack fp)
                       , "recursive" .= Bool False
                       ]
     Object resp <- apiCall app "/2/files/list_folder" args 
     let Array entries = resp H.! "entries"
     return $ fmap parseEntry (V.toList entries)
  where parseEntry :: Value -> FilePath
        parseEntry (Object entry) = let String name = entry H.! "name"
                                    in T.unpack name



put :: AppState -> [FilePath] -> FilePath -> IO ()
put app fps op =
  S.drain
  $ asyncly
  -- $ maxBuffer 1000
  $ maxThreads 10
  |$ uploadMetadata app
  |$ uploadFiles app
  |$ filterOutExistingFiles app
  |$ traverseDirectories
  |$ S.fromList [uploadState fp op | fp <- fps]



data UploadState = UploadState { sourcePath :: FilePath
                               , destPath :: FilePath
                               , sourceSize :: Maybe Int64
                               , sessionId :: Maybe T.Text
                               , offset :: Maybe Int64
                               , success :: Maybe Bool
                               }
  deriving (Eq, Ord, Read, Show)

uploadState :: FilePath -> FilePath -> UploadState
uploadState src dst = UploadState src dst Nothing Nothing Nothing Nothing



-- | Recursively expand directories into a stream of file names
traverseDirectories :: Async UploadState -> Async UploadState
traverseDirectories = S.concatMap traverseEntry

traverseEntry :: UploadState -> Async UploadState
traverseEntry upload =
  do fs <- liftIO (getSymbolicLinkStatus (sourcePath upload))
     if | isRegularFile fs -> let sz = Just $ fromIntegral $ fileSize fs
                              in S.yield $ upload { sourceSize = sz }
        | isDirectory fs -> traverseDirectories (readDir upload)
        | True -> S.nil

readDir :: UploadState -> Async UploadState
readDir upload = S.map prependPath $ serially (listDir src)
  where src = sourcePath upload
        dst = destPath upload
        prependPath :: FilePath -> UploadState
        prependPath fp = uploadState (src </> fp) (dst </> fp)

listDir :: FilePath -> Serial FilePath
listDir dp = S.bracket open close read
  where open :: IO DirStream
        open = do putStrLn $ "[reading directory " ++ quoteString dp ++ "...]"
                  openFile
                  ds <- openDirStream dp
                  return ds
        close :: DirStream -> IO ()
        close ds = do closeDirStream ds
                      closeFile
        read :: DirStream -> Serial FilePath
        read ds = S.filter (\fn -> fn /= "." && fn /= "..")
                  $ S.takeWhile (not . null)
                  $ S.repeatM (readDirStream ds)



filterOutExistingFiles :: AppState -> Async UploadState -> Async UploadState
filterOutExistingFiles app upload = upload >>= filterOutExistingFile app

filterOutExistingFile :: AppState -> UploadState -> Async UploadState
filterOutExistingFile app upload =
  do let fp = destPath upload
     mmd <- getMetadata app fp
     case mmd of
       Nothing -> S.yield upload
       Just _ -> do liftIO $ putStrLn ("[skipping existing file "
                                       ++ quoteString fp ++ "]")
                    S.nil



data Metadata = FileMetadata { name :: T.Text
                             , identifier :: T.Text
                             , size :: Int64
                             , pathLower :: Maybe T.Text
                             , pathDisplay :: Maybe T.Text
                             , symlinkInfo :: Maybe SymlinkInfo
                             , contentHash :: T.Text
                             }
              | FolderMetadata { name :: T.Text
                               , identifier :: T.Text
                               , pathLower :: Maybe T.Text
                               , pathDisplay :: Maybe T.Text
                               }
              | DeletedMetadata { name :: T.Text
                                , pathLower :: Maybe T.Text
                                , pathDisplay :: Maybe T.Text
                                }
  deriving (Eq, Ord, Read, Show)

instance FromJSON Metadata where
  parseJSON = withObject "Metadata"
    \v -> do let String tag = v H.! ".tag"
             case tag of
               "file" -> FileMetadata
                         <$> v .: "name"
                         <*> v .: "id"
                         <*> v .: "size"
                         <*> v .:? "path_lower"
                         <*> v .:? "path_display"
                         <*> v .:? "symlink_info"
                         <*> v .: "content_hash"
               "folder" -> FolderMetadata
                           <$> v .: "name"
                           <*> v .: "id"
                           <*> v .:? "path_lower"
                           <*> v .:? "path_display"
               "deleted" -> DeletedMetadata
                            <$> v .: "name"
                            <*> v .:? "path_lower"
                            <*> v .:? "path_display"

data SymlinkInfo = SymlinkInfo { target :: T.Text }
  deriving (Eq, Ord, Read, Show)

instance FromJSON SymlinkInfo where
  parseJSON = withObject "SymlinkInfo"
    \v -> SymlinkInfo <$> v .: "target"

getMetadata :: AppState -> FilePath -> Async (Maybe Metadata)
getMetadata app fp =
  do let args = object [ "path" .= String (T.pack fp) ]
     mresp <- liftIO
              $ tryHttp handleNotFound
              $ apiCall app "/2/files/get_metadata" args
     case mresp of
       Left _ -> S.yield Nothing
       Right resp ->
         do let mmd = fromJSON resp :: Result Metadata
            case mmd of
              Success md -> S.yield $ Just md
  where handleNotFound :: Int -> Value -> Maybe ()
        handleNotFound st val =
          do traceShow "aaa" $ return $ Just ()
             guard $ st == 409
             traceShow "bbb" $ return $ Just ()
             obj <- fromObject val
             traceShow "ccc" $ return $ Just ()
             err <- H.lookup "error" obj
             traceShow "ddd" $ return $ Just ()
             err <- fromObject err
             traceShow "eee" $ return $ Just ()
             tag <- H.lookup ".tag" err
             traceShow "fff" $ return $ Just ()
             guard $ tag == String "path"
             traceShow "ggg" $ return $ Just ()
             path <- H.lookup "path" err
             traceShow "hhh" $ return $ Just ()
             path <- fromObject path
             traceShow "iii" $ return $ Just ()
             tag <- H.lookup ".tag" path
             traceShow "jjj" $ return $ Just ()
             guard $ tag == String "not_found"
             traceShow "kkk" $ return $ Just ()
             return ()
        fromObject :: Value -> Maybe Object
        fromObject (Object obj) = Just obj
        fromObject _ = Nothing



uploadFiles :: AppState -> Async UploadState -> Async UploadState
uploadFiles app uploads = uploads >>= S.yieldM . uploadFile app

data Cursor = Cursor { content :: BL.ByteString
                     , coffset :: Int64
                     , count :: Int
                     }
              deriving (Eq, Ord, Read, Show)

makeCursor :: BL.ByteString -> Cursor
makeCursor content = Cursor content 0 0

splitCursor :: Int64 -> Cursor -> (Cursor, Cursor)
splitCursor sz (Cursor content coffset count) =
  let (hd, tl) = BL.splitAt sz content
  in (Cursor hd coffset count, Cursor tl (coffset + BL.length hd) (count + 1))

cursorDone :: Cursor -> Bool
cursorDone cursor = BL.null (content cursor)

uploadFile :: AppState -> UploadState -> IO UploadState
uploadFile app upload =
  do openFile
     file <- BL.readFile fp
     -- TODO: Should we be running upload sessions in parallel?
     let cursor = makeCursor file
     (sessionId, cursor) <- upload_session_start cursor
     cursor <- iterateUntilM cursorDone (upload_session_append sessionId) cursor
     closeFile
     evaluate $ upload { sessionId = Just sessionId
                       , offset = Just (coffset cursor)
                       }
  where
    fp = sourcePath upload
    op = destPath upload
    fs = fromJust (sourceSize upload)
    divup i j = (i + j - 1) `div` j
    requestSize = 150000000 :: Int64 -- 150 MByte
    nrequests = fs `divup` fromIntegral requestSize
    upload_session_start :: Cursor -> IO (T.Text, Cursor)
    upload_session_start cursor =
      do let (headCursor, tailCursor) = splitCursor requestSize cursor
         putStrLn $ ("[uploading " ++ quoteString fp ++ " to " ++ quoteString op
                     ++ " (" ++ show (count headCursor + 1) ++ "/"
                     ++ show nrequests ++ ")...]")
         let args = object ["close" .= cursorDone tailCursor]
         assertM $ coffset headCursor == 0
         Object resp <- sendContent app "/2/files/upload_session/start" args
                        (content headCursor)
         let String sessionId = resp H.! "session_id"
         return (sessionId, tailCursor)
    upload_session_append :: T.Text -> Cursor -> IO Cursor
    upload_session_append sessionId cursor =
      do let (headCursor, tailCursor) = splitCursor requestSize cursor
         putStrLn $ ("[uploading " ++ quoteString fp ++ " to " ++ quoteString op
                     ++ " (" ++ show (count headCursor + 1) ++ "/"
                     ++ show nrequests ++ ")...]")
         let args =
               object [ "cursor" .=
                        object [ "session_id" .= String sessionId
                               , "offset" .=
                                 Number (fromIntegral $ coffset headCursor) ]
                      , "close" .= cursorDone tailCursor ]
         Object resp <- sendContent app "/2/files/upload_session/append_v2" args
                        (content headCursor)
         return tailCursor



uploadMetadata :: AppState -> Async UploadState -> Async UploadState
uploadMetadata app uploads =
  let chunks = S.chunksOf 1000 toList uploads
  in uploadMetadata' app =<< chunks

uploadMetadata' :: AppState -> [UploadState] -> Async UploadState
uploadMetadata' app uploads =
  do res <- liftIO $ bracket_ openUploadFinishConnection
                              closeUploadFinishConnection
                              do putStrLn "[finalizing upload...]"
                                 mres <- finish_batch
                                 res <- case mres of
                                   Left asyncJobId ->
                                     untilJust do putStrLn "[waiting...]"
                                                  threadDelay 100000 -- 100 ms
                                                  finish_batch_check asyncJobId
                                   Right res -> return res
                                 putStrLn "[done finalizing upload]"
                                 return res
     let done = all (fromJust . success) res
     assertM done
     -- S.fromList uploads
     -- res <- liftIO $ evaluate res
     -- liftIO $ mapM_ (putStrLn . show) res
     S.fromList res
  where
    finish_batch :: IO (Either T.Text [UploadState])
    finish_batch =
      do assertM $ length uploads <= 1000
         let args =
               object [ "entries" .= array
                        [ object [ "cursor" .= object
                                   [ "session_id" .=
                                     String (fromJust $ sessionId upload)
                                   , "offset" .= fromJust (offset upload)
                                   ]
                                 , "commit" .= object
                                   [ "path" .=
                                     String (T.pack $ destPath upload)
                                   , "mode" .= String "overwrite"
                                   -- , "mute" .= Bool True
                                   ]
                                 ]
                        | upload <- uploads
                        ]
                      ]
         Object resp <- apiCall app "/2/files/upload_session/finish_batch" args
         let String tag = resp H.! ".tag"
         if | tag == "complete"
              -> let Array entries = resp H.! "entries"
                     res = zipWith parseEntry (V.toList entries) uploads
                 in return $ Right res
            | tag == "async_job_id"
              -> do let String asyncJobId = resp H.! "async_job_id"
                    return $ Left asyncJobId
    finish_batch_check :: T.Text -> IO (Maybe [UploadState])
    finish_batch_check asyncJobId =
      do let args = object [ "async_job_id" .= String asyncJobId ]
         Object resp <- apiCall app "/2/files/upload_session/finish_batch/check"
                        args
         let String tag = resp H.! ".tag"
         if | tag == "complete"
              -> let Array entries = resp H.! "entries"
                     res = zipWith parseEntry (V.toList entries) uploads
                 in return $ Just res
            | tag == "in_progress"
              -> return Nothing
    parseEntry :: Value -> UploadState -> UploadState
    parseEntry (Object entry) upload =
      let String tag = entry H.! ".tag"
          String name = entry H.! "name"
      in (if tag /= "success" then traceShow entry else id)
           upload { success = Just (tag == "success") }

 

array :: [Value] -> Value
array = listValue id

assertM :: Monad m => Bool -> m ()
assertM c = assert c $ return ()

chomp :: S8.ByteString -> S8.ByteString
chomp s = if S8.null s || S8.last s /= '\n'
          then s
          else S8.init s

-- fromList :: (IsStream t, Monad m) => [a] -> t m a
-- fromList = fromStreamList . S.yield

-- fromListM :: (IsStream t, Monad m) => m [a] -> t m a
-- fromListM = fromStreamList . S.yieldM

fromRight' :: Either a b -> b
fromRight' (Right x) = x

-- fromStreamList :: (IsStream t, Monad m) => t m [a] -> t m a
-- fromStreamList = S.concatMap S.fromList

quoteString :: String -> String
quoteString s = "\"" ++ s ++ "\""
