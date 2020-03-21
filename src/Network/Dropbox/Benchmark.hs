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

module Network.Dropbox.Benchmark (main) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Maybe
import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Prelude as S

main :: IO ()
main = do
  counters <- newIORef (0, 0, 0)
  let files = [show i | i <- [0 .. 999]]
  results <- S.toList $ benchmark counters $ S.fromList files
  return ()

benchmark :: IORef (Int, Int, Int) -> Serial FilePath -> Serial (Either () ())
benchmark counters files =
  S.concatMap S.fromList
  $ S.mapM finalizeUploads
  $ S.chunksOf 10 FL.toList
  $ asyncly . maxThreads 10 . S.mapM uploadFile
  $ asyncly . maxThreads 10 . S.mapMaybeM needUpload
  $ serially $ files
  where
    -- at most about 10 in parallel (filesystem bound)
    needUpload :: FilePath -> IO (Maybe FilePath)
    needUpload fp = do
      tid <- myThreadId
      nu <-
        atomicModifyIORef' counters \(nu, uf, fu) -> ((nu + 1, uf, fu), nu + 1)
      putStrLn $ "checking " ++ fp ++ " " ++ show tid ++ " " ++ show nu
      threadDelay (100 * 1000)
      atomicModifyIORef' counters \(nu, uf, fu) -> ((nu - 1, uf, fu), ())
      return (Just fp)
    -- at most about 10 in parallel (network bound)
    uploadFile :: FilePath -> IO FilePath
    uploadFile fp = do
      tid <- myThreadId
      uf <-
        atomicModifyIORef' counters \(nu, uf, fu) -> ((nu, uf + 1, fu), uf + 1)
      putStrLn $ "uploading " ++ fp ++ " " ++ show tid ++ " " ++ show uf
      threadDelay (100 * 1000)
      atomicModifyIORef' counters \(nu, uf, fu) -> ((nu, uf - 1, fu), ())
      return fp
    -- at most one at a time (must be called serially)
    finalizeUploads :: [FilePath] -> IO [Either () ()]
    finalizeUploads fps = do
      tid <- myThreadId
      fu <-
        atomicModifyIORef' counters \(nu, uf, fu) -> ((nu, uf, fu + 1), fu + 1)
      putStrLn $ "finalizing " ++ show fps ++ " " ++ show tid ++ " " ++ show fu
      threadDelay (1000 * 1000)
      atomicModifyIORef' counters \(nu, uf, fu) -> ((nu, uf, fu - 1), ())
      return $ fmap (\_ -> Right ()) fps
