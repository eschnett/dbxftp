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

import Control.Concurrent
import Control.Exception
import Control.Monad.Loops
import Data.IORef
import Data.List
import qualified Data.Text as T
import Graphics.Vty

-- TODO: Use Data.Sequence from "containers" instead of []

data ScreenManager = ScreenManager { vty :: Vty
                                   , title :: T.Text
                                   , logged :: IORef [(T.Text, T.Text)]
                                   , current :: IORef [(T.Text, T.Text)]
                                   }

addLog :: ScreenManager -> (T.Text, T.Text) -> IO ()
addLog smgr msg = do
  atomicModifyIORef' (logged smgr) \msgs -> (take 20 (msg:msgs), ())
  -- displayActive smgr

addActive :: ScreenManager -> (T.Text, T.Text) -> IO ()
addActive smgr msg = do
  atomicModifyIORef' (current smgr) \msgs -> (msg:msgs, ())
  -- displayActive smgr

removeActive :: ScreenManager -> (T.Text, T.Text) -> IO ()
removeActive smgr msg = do
  atomicModifyIORef' (current smgr) \msgs -> (filter (/= msg) msgs, ())
  -- displayActive smgr

withActive :: ScreenManager -> (T.Text, T.Text) -> IO a -> IO a
withActive smgr msg =
  bracket_ (addActive smgr msg) (removeActive smgr msg)

displayActive :: ScreenManager -> IO ()
displayActive smgr = do
  let out = outputIface (vty smgr)
  reg <- displayBounds out
  logged <- readIORef (logged smgr)
  current <- readIORef (current smgr)
  let headline = text' (defAttr `withForeColor` green)
        $ "DBXFTP: " `T.append` title smgr
  let img = vertCat ([headline]
                     ++ fmap (textImage reg) (reverse logged)
                     ++ [text defAttr ""]
                     ++ fmap (textImage reg) (sort current)
                    )
  let pic = picForImage img
  update (vty smgr) pic

textImage :: DisplayRegion -> (T.Text, T.Text) -> Image
textImage reg (l, r) = joinImages reg (text' defAttr l) (text' defAttr r)

joinImages :: DisplayRegion -> Image -> Image -> Image
joinImages reg l r =
  let w = regionWidth reg
      wl = imageWidth l
      wr = imageWidth r
  in if wl + wr <= w
     then l <|> r
     else let ell = text' defAttr "..."
              r' = ell <|> cropLeft (w - wl - imageWidth ell) r
          in l <|> r'

runWithProgress :: T.Text -> (ScreenManager -> IO a) -> IO a
runWithProgress title f = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  logged <- newIORef []
  current <- newIORef []
  let smgr = ScreenManager vty title logged current
  flag <- newEmptyMVar
  painter <- forkIO do
    whileM_ (isEmptyMVar flag) do
      displayActive smgr
      threadDelay (1000 * 1000)
    takeMVar flag
  result <- f smgr
  putMVar flag ()
  putMVar flag ()
  displayActive smgr
  -- threadDelay (1000 * 1000)
  shutdown vty
  return result
