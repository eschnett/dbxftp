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

module Network.Dropbox.Progress
  ( ScreenManager(..)
  , addLog
  -- , addActive
  -- , removeActive
  , withActive
  -- , displayActive
  , runWithProgress
  ) where

import Control.Exception
import Data.IORef
import qualified Data.Text as T
import Graphics.Vty

-- TODO: Use Data.Sequence from "containers" instead of []

data ScreenManager = ScreenManager { vty :: Vty
                                   , logged :: IORef [T.Text]
                                   , current :: IORef [T.Text]
                                   }

addLog :: ScreenManager -> T.Text -> IO ()
addLog smgr msg = do
  atomicModifyIORef' (logged smgr) \msgs -> (take 1000 (msg:msgs), ())
  displayActive smgr

addActive :: ScreenManager -> T.Text -> IO ()
addActive smgr msg = do
  atomicModifyIORef' (current smgr) \msgs -> (msg:msgs, ())
  displayActive smgr

removeActive :: ScreenManager -> T.Text -> IO ()
removeActive smgr msg = do
  atomicModifyIORef' (current smgr) \msgs -> (filter (/= msg) msgs, ())
  displayActive smgr

withActive :: ScreenManager -> T.Text -> IO a -> IO a
withActive smgr msg =
  bracket_ (addActive smgr msg) (removeActive smgr msg)

displayActive :: ScreenManager -> IO ()
displayActive smgr = do
  logged <- readIORef (logged smgr)
  current <- readIORef (current smgr)
  let title = text (defAttr `withForeColor` green) "DBXFTP: put"
  let img = vertCat ([title]
                     ++ fmap (\msg -> text' defAttr msg) current
                     ++ [text defAttr ""]
                     ++ fmap (\msg -> text' defAttr msg) logged
                    )
  let pic = picForImage img
  update (vty smgr) pic

runWithProgress :: (ScreenManager -> IO ()) -> IO ()
runWithProgress f = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  logged <- newIORef []
  current <- newIORef []
  let smgr = ScreenManager vty logged current
  displayActive smgr
  f smgr
  shutdown vty

-- USE Data.ByteString.copy to free data!
