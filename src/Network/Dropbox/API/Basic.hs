module Network.Dropbox.API.Basic
  ( Manager
  , newManager
  , apiCall
  , sendContent
  , recvContent
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops (untilJust)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import qualified Data.HashMap.Strict as H
import Data.Int
import Data.Scientific
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
import Debug.Trace
import Network.HTTP.Client hiding (Manager, newManager)
import qualified Network.HTTP.Client as NC
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import System.FilePath.Posix
import System.Posix

--------------------------------------------------------------------------------

maxOpenConnections :: Int
maxOpenConnections = 100

data Manager = Manager { accessToken :: BS.ByteString
                       , manager :: NC.Manager
                       , theOpenConnections :: QSem
                       , theAvailableConnection :: MVar ()
                       }

newManager :: IO Manager
newManager = Manager
             <$> getAccessToken
             <*> NC.newManager tlsManagerSettings
             <*> newQSem maxOpenConnections
             <*> newMVar ()

-- no Read/Show instance to prevent accidental disclosure
newtype AccessTokenFile = AccessTokenFile BS.ByteString
  deriving (Eq, Ord)

instance FromJSON AccessTokenFile where
  parseJSON = withObject "AccessTokenFile"
              \v -> AccessTokenFile . T.encodeUtf8 <$> v .: "access_token"

getAccessToken :: IO BS.ByteString
getAccessToken =
  do home <- getEnvDefault "HOME" ""
     let fp = home </> ".dbxftp.json"
     Just (AccessTokenFile token) <- decodeFileStrict fp
     return token

waitOpenConnection :: Manager -> IO ()
waitOpenConnection mgr = do waitQSem (theOpenConnections mgr)
                            readMVar (theAvailableConnection mgr)

signalOpenConnection :: Manager -> IO ()
signalOpenConnection mgr = signalQSem (theOpenConnections mgr)

delayConnection :: Manager -> Int64 -> IO ()
delayConnection mgr delay =
  do tnow <- systemToPOSIXTime <$> getSystemTime
     let tuntil = tnow + fromIntegral delay
     let mvar = theAvailableConnection mgr
     () <- takeMVar mvar
     tnow' <- systemToPOSIXTime <$> getSystemTime
     let delay' = tuntil - tnow'
     when (delay' > 0) $ threadDelay $ round $ 1e6 * delay'
     putMVar mvar ()

--------------------------------------------------------------------------------

data ArgLoc = ArgHeader | ArgBody
  deriving (Enum, Eq, Ord, Read, Show)
data ResLoc = ResHeader | ResBody
  deriving (Enum, Eq, Ord, Read, Show)

dbxCall :: (ToJSON arg, FromJSON result)
        => BS.ByteString -> ArgLoc -> ResLoc
        -> Manager -> BS.ByteString -> arg
        -> BL.ByteString -> IO (result, BL.ByteString)
dbxCall host argLoc resLoc mgr path arg input =
  do let request =
           basicRequest
           & (if argLoc == ArgHeader
              then addRequestHeader
                   "Dropbox-API-Arg" (BL.toStrict (encode arg))
              else id)
           & addRequestHeader "Content-Type" "application/octet-stream"
           & (if argLoc == ArgHeader
              then setRequestBodyLBS input
              else assert (BL.null input) $ setRequestBodyJSON arg)
     response <- untilJust $ checkedHttp request
     let body = getResponseBody response :: BL.ByteString
     let result = if resLoc == ResHeader
                  then BL.fromStrict $ head
                       $ getResponseHeader "Dropbox-API-Result" response
                  else body
     result <- case eitherDecode result of
                   Left err -> do putStrLn "JSON parse error"
                                  putStrLn $ show err
                                  throw $ AssertionFailed err
                   Right result -> return result
     let output = if resLoc == ResHeader then body else BL.empty
     return (result, output)
  where
    checkedHttp :: Request -> IO (Maybe (Response BL.ByteString))
    checkedHttp request =
      do response <-
           bracket_ (waitOpenConnection mgr) (signalOpenConnection mgr)
           $ httpLBS request
         let st = getResponseStatusCode response
         let body = getResponseBody response
         if | st == 429
              -> do putStrLn "Exceeded rate limit"
                    flagRateLimit body
                    return Nothing
            | st `div` 100 == 5 -- retry
              -> do putStrLn $ ("Received status code " ++ show st
                                ++ ", retrying...")
                    return Nothing
            | st `div` 100 /= 2 -- abort
              -> do putStrLn $ ("Received status code " ++ show st
                                ++ ", aborting")
                    let ex = StatusCodeException
                             (void response) (BL.toStrict body)
                    throw $ HttpExceptionRequest request ex
            | True -> return $ Just response
    basicRequest :: Request
    basicRequest =
      defaultRequest
      & setRequestManager (manager mgr)
      & setRequestSecure True
      & setRequestHost host
      & setRequestPort 443
      & setRequestMethod "POST"
      & setRequestPath path
      & addRequestHeader "Authorization" (BS.append "Bearer " (accessToken mgr))
      & setRequestIgnoreStatus
    flagRateLimit :: BL.ByteString -> IO ()
    flagRateLimit body =
      do let delay = case decodeRateLimit body of
               Nothing -> 60
               Just retryAfter -> retryAfter
         delayConnection mgr delay
    decodeRateLimit :: BL.ByteString -> Maybe Int64
    decodeRateLimit body =
      do obj <- decode body
         obj <- fromObject obj
         error <- H.lookup "error" obj
         error <- fromObject error
         reason <- H.lookup "reason" error
         reason <- fromObject reason
         tag <- H.lookup ".tag" reason
         guard $ tag == String "too_many_requests"
         retryAfter <- H.lookup "retry_after" error
         retryAfter <- fromNumber retryAfter
         retryAfter <- toBoundedInteger retryAfter
         return retryAfter
    fromObject :: Value -> Maybe Object
    fromObject (Object obj) = Just obj
    fromObject _ = Nothing
    fromNumber :: Value -> Maybe Scientific
    fromNumber (Number num) = Just num
    fromNumber _ = Nothing

--------------------------------------------------------------------------------

apiCall :: (ToJSON arg, FromJSON result)
        => Manager -> BS.ByteString -> arg -> IO result
apiCall conn path arg =
  do (result, _) <-
       dbxCall "api.dropboxapi.com" ArgBody ResBody conn path arg BL.empty
     return result

sendContent :: (ToJSON arg, FromJSON result)
            => Manager -> BS.ByteString -> arg -> BL.ByteString -> IO result
sendContent conn path arg input =
  do (result, _) <-
       dbxCall "content.dropboxapi.com" ArgHeader ResBody conn path arg input
     return result

recvContent :: (ToJSON arg, FromJSON result)
            => Manager -> BS.ByteString -> arg -> IO (result, BL.ByteString)
recvContent conn path arg =
  dbxCall "content.dropboxapi.com" ArgHeader ResHeader conn path arg BL.empty
