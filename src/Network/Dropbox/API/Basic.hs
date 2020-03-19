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

module Network.Dropbox.API.Basic
  ( Manager
  , newManager
  , waitUploadFinish
  , signalUploadFinish
  , DbxException(..)
  , apiCall
  , sendContent
  , recvContent
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
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
import Data.Typeable
import Network.HTTP.Client hiding (Manager, newManager)
import qualified Network.HTTP.Client as NC
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import System.FilePath.Posix
import System.Posix

--------------------------------------------------------------------------------

maxOpenConnections :: Int
maxOpenConnections = 10

maxUploadFinishes :: Int
maxUploadFinishes = 1

data Manager = Manager { accessToken :: BS.ByteString
                       , manager :: NC.Manager
                       , theOpenConnections :: QSem
                       , theUploadFinishes :: QSem
                       , theAvailableConnection :: MVar ()
                       }

newManager :: IO Manager
newManager = Manager
             <$> getAccessToken
             <*> NC.newManager tlsManagerSettings
             <*> newQSem maxOpenConnections
             <*> newQSem maxUploadFinishes
             <*> newMVar ()

-- no Read/Show instances to prevent accidental disclosure of token
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

waitUploadFinish :: Manager -> IO ()
waitUploadFinish mgr = waitQSem (theUploadFinishes mgr)

signalUploadFinish :: Manager -> IO ()
signalUploadFinish mgr = signalQSem (theUploadFinishes mgr)

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

data DbxException = DbxStatusCodeException Request Int BL.ByteString
                  | DbxJSONException Request BL.ByteString String
                  | DbxNotFoundException Request
  deriving (Show, Typeable)

instance Exception DbxException

dbxCall :: (ToJSON arg, FromJSON result)
        => BS.ByteString -> ArgLoc -> ResLoc
        -> Manager -> BS.ByteString -> arg
        -> BL.ByteString -> IO (result, BL.ByteString)
dbxCall host argLoc resLoc mgr path arg input = do
  let request =
        basicRequest
        & (if argLoc == ArgHeader
           then addRequestHeader "Dropbox-API-Arg" (BL.toStrict (encode arg))
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
                   putStrLn $ show result
                   putStrLn $ show err
                   throw $ DbxJSONException request body err
    Right result -> return result
  let output = if resLoc == ResHeader then body else BL.empty
  return (result, output)
  where
    checkedHttp :: Request -> IO (Maybe (Response BL.ByteString))
    checkedHttp request = do
      mresponse <-
        tryJust (\case
                    HttpExceptionRequest _ ex ->
                      case ex of
                        ResponseTimeout -> Just ResponseTimeout
                        _ -> Nothing
                    _ -> Nothing)
        $ bracket_ (waitOpenConnection mgr) (signalOpenConnection mgr)
        $ httpLBS request
      case mresponse of
        Left ResponseTimeout -> do
          putStrLn "Received ResponseTimeout, retrying..."
          return Nothing -- retry
        Right response -> do
          let st = getResponseStatusCode response
          let body = getResponseBody response
          if | st == 409 -> do
                 let ex = decodeDbxException request st body
                 case ex of
                   DbxNotFoundException{} -> return ()
                   _ -> putStrLn
                        $ "Received status code " ++ show st ++ ", aborting"
                 throw ex
             | st == 429 -> do
                 putStrLn "Exceeded rate limit"
                 flagRateLimit body
                 return Nothing
             | st `div` 100 == 5 -> do -- retry
                 putStrLn $ ("Received status code " ++ show st
                             ++ ", retrying...")
                 return Nothing
             | st `div` 100 /= 2 -> do -- abort
                 putStrLn $ "Received status code " ++ show st ++ ", aborting"
                 throw $ DbxStatusCodeException request st body
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
    decodeDbxException :: Request -> Int -> BL.ByteString -> DbxException
    decodeDbxException request st body = do
      let notFound = handleNotFound body
      case notFound of
        Nothing -> DbxStatusCodeException request st body
        Just () -> DbxNotFoundException request
    handleNotFound :: BL.ByteString -> Maybe ()
    handleNotFound body =
      do obj <- decode body
         obj <- fromObject obj
         err <- H.lookup "error" obj
         err <- fromObject err
         tag <- H.lookup ".tag" err
         guard $ tag == String "path"
         path <- H.lookup "path" err
         path <- fromObject path
         tag <- H.lookup ".tag" path
         guard $ tag == String "not_found"
         return ()
    flagRateLimit :: BL.ByteString -> IO ()
    flagRateLimit body = do
      let delay = case decodeRateLimit body of
            Nothing -> 60
            Just retryAfter -> retryAfter
      delayConnection mgr delay
    decodeRateLimit :: BL.ByteString -> Maybe Int64
    decodeRateLimit body = do
      obj <- decode body
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
apiCall mgr path arg = do
  (result, _) <-
    dbxCall "api.dropboxapi.com" ArgBody ResBody mgr path arg BL.empty
  return result

sendContent :: (ToJSON arg, FromJSON result)
            => Manager -> BS.ByteString -> arg -> BL.ByteString -> IO result
sendContent mgr path arg input = do
  (result, _) <-
    dbxCall "content.dropboxapi.com" ArgHeader ResBody mgr path arg input
  return result

recvContent :: (ToJSON arg, FromJSON result)
            => Manager -> BS.ByteString -> arg -> IO (result, BL.ByteString)
recvContent mgr path arg =
  dbxCall "content.dropboxapi.com" ArgHeader ResHeader mgr path arg BL.empty
