{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- <https://www.dropbox.com/developers/documentation/http/overview>

module DBXFTP
  ( AppState
  , newAppState
  , ls
  , put
  ) where

import Control.Concurrent (threadDelay)
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
import qualified Data.Text as T
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



openFile :: IO ()
openFile = waitQSemN theOpenFiles 1

closeFile :: IO ()
closeFile = signalQSemN theOpenFiles 1

theOpenFiles :: QSemN
{-# NOINLINE theOpenFiles #-}
theOpenFiles = unsafePerformIO $ newQSemN 100



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



ls :: AppState -> FilePath -> IO [FilePath]
ls app fp =
  do let args = object [ "path" .= String (T.pack fp)
                       , "recursive" .= Bool False
                       ]
     let request =
           defaultRequest
           & setRequestManager (manager app)
           & setRequestSecure True
           & setRequestHost "api.dropboxapi.com"
           & setRequestPort 443
           & setRequestMethod "POST"
           & setRequestPath "/2/files/list_folder"
           & (setRequestHeader "Authorization"
              [S8.append "Bearer " (authToken app)])
           & setRequestHeader "Content-Type" ["application/json"]
           & setRequestBodyJSON args
     response <- untilJust
       do jhresp <- try @JSONException $ try @HttpException
                    $ httpJSON request
          case jhresp of
            Left ex -> do putStrLn $ show ex
                          return Nothing
            Right hresp -> case hresp of
              Left ex -> do putStrLn $ show ex
                            threadDelay 100000 -- 100 ms
                            return Nothing
              Right resp -> return $ Just resp
     let st = getResponseStatusCode response
     when (st /= 200) do putStrLn $ "Received statusCode " ++ show st
                         putStrLn $ show response
                         assert False $ return ()
     let Object json = getResponseBody response
     let Array entries = json H.! "entries"
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
  do fs <- liftIO (getFileStatus (sourcePath upload))
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
uploadFile (AppState authToken manager) upload =
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
         let request =
               defaultRequest
               & setRequestManager manager
               & setRequestSecure True
               & setRequestHost "content.dropboxapi.com"
               & setRequestPort 443
               & setRequestMethod "POST"
               & setRequestPath "/2/files/upload_session/start"
               & (setRequestHeader "Authorization"
                  [S8.append "Bearer " authToken])
               & setRequestHeader "Dropbox-API-Arg" [BL.toStrict (encode args)]
               & setRequestHeader "Content-Type" ["application/octet-stream"]
               & setRequestBodyLBS (content headCursor)
         response <- untilJust
           do jhresp <- try @JSONException $ try @HttpException
                        $ httpJSON request
              case jhresp of
                Left ex -> do putStrLn $ show ex
                              return Nothing
                Right hresp -> case hresp of
                  Left ex -> do putStrLn $ show ex
                                threadDelay 100000 -- 100 ms
                                return Nothing
                  Right resp -> return $ Just resp
         let st = getResponseStatusCode response
         when (st /= 200) do putStrLn $ "Received statusCode " ++ show st
                             putStrLn $ show response
                             assert False $ return ()
         let Object json = getResponseBody response
         let String sessionId = json H.! "session_id"
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
                                 (Number $ fromIntegral $ coffset headCursor) ]
                      , "close" .= cursorDone tailCursor ]
         let request =
               defaultRequest
               & setRequestManager manager
               & setRequestSecure True
               & setRequestHost "content.dropboxapi.com"
               & setRequestPort 443
               & setRequestMethod "POST"
               & setRequestPath "/2/files/upload_session/append_v2"
               & (setRequestHeader "Authorization"
                  [S8.append "Bearer " authToken])
               & setRequestHeader "Dropbox-API-Arg" [BL.toStrict (encode args)]
               & setRequestHeader "Content-Type" ["application/octet-stream"]
               & setRequestBodyLBS (content headCursor)
         response <- untilJust
           do jhresp <- try @JSONException $ try @HttpException
                        $ httpJSON request
              case jhresp of
                Left ex -> do putStrLn $ show ex
                              return Nothing
                Right hresp -> case hresp of
                  Left ex -> do putStrLn $ show ex
                                threadDelay 100000 -- 100 ms
                                return Nothing
                  Right resp -> return $ Just resp
         let _ = response :: Response Value
         let st = getResponseStatusCode response
         when (st /= 200) do putStrLn $ "Received statusCode " ++ show st
                             putStrLn $ show response
                             assert False $ return ()
         return tailCursor



uploadMetadata :: AppState -> Async UploadState -> Async UploadState
uploadMetadata app uploads =
  let chunks = asyncly (S.chunksOf 1000 toList uploads)
  in serially (uploadMetadata' app =<< chunks)

uploadMetadata' :: AppState -> [UploadState] -> Serial UploadState
uploadMetadata' (AppState authToken manager) uploads =
  do mres <- liftIO finish_batch
     res <- liftIO case mres of
                     Left asyncJobId ->
                       untilJust do threadDelay 100000 -- 100 ms
                                    finish_batch_check asyncJobId
                     Right res -> return res
     liftIO $ putStrLn $ "[done]"
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
                                     (String $ fromJust $ sessionId upload)
                                   , "offset" .= (fromJust $ offset upload)
                                   ]
                                 , "commit" .= object
                                   [ "path" .=
                                     (String $ T.pack $ destPath upload)
                                   , "mode" .= String "overwrite"
                                   -- , "mute" .= Bool True
                                   ]
                                 ]
                        | upload <- uploads
                        ]
                      ]
         let request =
               defaultRequest
               & setRequestManager manager
               & setRequestSecure True
               & setRequestHost "api.dropboxapi.com"
               & setRequestPort 443
               & setRequestMethod "POST"
               & setRequestPath "/2/files/upload_session/finish_batch"
               & (setRequestHeader "Authorization"
                  [S8.append "Bearer " authToken])
               & setRequestHeader "Content-Type" ["application/json"]
               & setRequestBodyLBS (encode args)
         liftIO $ putStrLn $ "[finalizing...]"
         response <- untilJust
           do jhresp <- try @JSONException $ try @HttpException
                        $ httpJSON request
              case jhresp of
                Left ex -> do putStrLn $ show ex
                              return Nothing
                Right hresp -> case hresp of
                  Left ex -> do putStrLn $ show ex
                                threadDelay 100000 -- 100 ms
                                return Nothing
                  Right resp -> return $ Just resp
         let st = getResponseStatusCode response
         when (st /= 200) do putStrLn $ "Received statusCode " ++ show st
                             putStrLn $ show response
                             assert False $ return ()
         let Object json = getResponseBody response
         let String tag = json H.! ".tag"
         if | tag == "complete"
              -> let Array entries = json H.! "entries"
                     res = zipWith parseEntry (V.toList entries) uploads
                 in return $ Right res
            | tag == "async_job_id"
              -> do let String asyncJobId = json H.! "async_job_id"
                    return $ Left asyncJobId
    finish_batch_check :: T.Text -> IO (Maybe [UploadState])
    finish_batch_check asyncJobId =
      do let args = object [ "async_job_id" .= String asyncJobId ]
         let request =
               defaultRequest
               & setRequestManager manager
               & setRequestSecure True
               & setRequestHost "api.dropboxapi.com"
               & setRequestPort 443
               & setRequestMethod "POST"
               & setRequestPath "/2/files/upload_session/finish_batch/check"
               & (setRequestHeader "Authorization"
                  [S8.append "Bearer " authToken])
               & setRequestHeader "Content-Type" ["application/json"]
               & setRequestBodyJSON args
         liftIO $ putStrLn $ "[waiting...]"
         response <- untilJust
           do jhresp <- try @JSONException $ try @HttpException
                        $ httpJSON request
              case jhresp of
                Left ex -> do putStrLn $ show ex
                              return Nothing
                Right hresp -> case hresp of
                  Left ex -> do putStrLn $ show ex
                                threadDelay 100000 -- 100 ms
                                return Nothing
                  Right resp -> return $ Just resp
         let st = getResponseStatusCode response
         when (st /= 200) do putStrLn $ "Received statusCode " ++ show st
                             putStrLn $ show response
                             assert False $ return ()
         let Object json = getResponseBody response
         let String tag = json H.! ".tag"
         if | tag == "complete"
              -> let Array entries = json H.! "entries"
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
