{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       $HEADER$
-- Description:  Tests for Data.Streaming.NamedPipes module.
-- Copyright:    (c) 2016, Ixperta Solutions s.r.o.
-- License:      BSD3
--
-- Maintainer:   Ixcom Core Team <ixcom-core@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC-specific language extensions.
--
-- Tests for "Data.Streaming.NamedPipes" module.
module TestCase.Data.Streaming.NamedPipes (tests)
  where

import Control.Applicative ((*>), pure)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, tryPutMVar, takeMVar)
import Data.Function (($), const)
import Data.Functor ((<$))
import System.IO (IO)

import Control.Concurrent.Async (async, waitAnyCancel)

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure)

import System.Win32.NamedPipes (PipeName, PipePath(LocalPipe))
import Data.Streaming.NamedPipes
    ( AppDataPipe
    , clientSettingsPipe
    , runPipeClient
    , runPipeServer
    , serverSettingsPipe
    , setAfterBindPipe
    )


tests :: [Test]
tests =
    [ testCase "Empty server and client" testConnectDisconnect
    ]

withServer
    :: PipeName
    -> (AppDataPipe -> IO ())
    -> (((AppDataPipe -> IO a) -> IO a) -> IO ())
    -> IO ()
withServer name serverApp k = do
    (signalReady, waitReady) <- newWait
    let serverSettings = setAfterBindPipe signalReady $
            serverSettingsPipe name
    let clientSettings = clientSettingsPipe (LocalPipe name)
    server <- async $ runPipeServer serverSettings serverApp
    clients <- async $ waitReady *> k (runPipeClient clientSettings)
    timeOut <- async $ threadDelay 500000 *> assertFailure "timeout"  -- 500 ms
    () <$ waitAnyCancel [server, clients, timeOut]
  where
    newWait = do
        ready <- newEmptyMVar
        let signalReady _ = void $ tryPutMVar ready ()
        let waitReady = takeMVar ready
        pure (signalReady, waitReady)

testConnectDisconnect :: Assertion
testConnectDisconnect =
    withServer pipeName (const $ pure ()) ($ const $ pure ())

pipeName :: PipeName
pipeName = "win-named-pipes-test"
