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
import Control.Concurrent (myThreadId, threadDelay, throwTo)
import Control.Concurrent.MVar (newEmptyMVar, tryPutMVar, takeMVar)
import Control.Exception (catch)
import Control.Monad ((>>=), replicateM_, void)
import Data.Function (($), (.), const)
import Data.Functor ((<$))
import Data.Int (Int)
import Data.List (replicate)
import Data.Monoid ((<>))
import Data.String (fromString)
import System.IO (IO)
import Text.Show (show)

import Control.Concurrent.Async (async, mapConcurrently, waitAnyCancel)
import System.Win32.Process (getProcessId)
import System.Win32.Types (iNVALID_HANDLE_VALUE)

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), assertFailure)
import Test.HUnit.Lang (HUnitFailure)

import System.Win32.NamedPipes (PipeName, PipePath(LocalPipe))
import Data.Streaming.NamedPipes
    ( AppDataPipe
    , appRead
    , appWrite
    , clientSettingsPipe
    , runPipeClient
    , runPipeServer
    , serverSettingsPipe
    , setAfterBindPipe
    )


tests :: [Test]
tests =
    [ testCase "Empty server and client" testConnectDisconnect
    , testCase "Server pings, client waits" testServerPing
    , testCase "Client pings, server waits" testClientPing
    , testCase "Multiple empty clients (consecutive)" testConsecutiveClients
    , testCase "Multiple empty clients (concurrent)" testConcurrentClients
    ]

withServer
    :: (AppDataPipe -> IO ())
    -> (((AppDataPipe -> IO a) -> IO a) -> IO ())
    -> IO ()
withServer serverApp k = do
    name <- genPipeName

    (signalReady, waitReady) <- newWait
    let serverSettings = setAfterBindPipe signalReady $
            serverSettingsPipe name
    let clientSettings = clientSettingsPipe (LocalPipe name)

    server <- async $ do
        thread <- myThreadId
        let throwToServer = throwTo thread :: HUnitFailure -> IO ()
        runPipeServer serverSettings $ \a -> catch (serverApp a) throwToServer
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
    withServer (const $ pure ()) ($ const $ pure ())

testServerPing :: Assertion
testServerPing = withServer server ($ client)
  where
    server appData = do
        appWrite appData "ping"
        appRead appData >>= \m -> m @?= "pong"
        appWrite appData "end"
    client appData = do
        appRead appData >>= \m -> m @?= "ping"
        appWrite appData "pong"
        appRead appData >>= \m -> m @?= "end"

testClientPing :: Assertion
testClientPing = withServer server ($ client)
  where
    server appData = do
        appRead appData >>= \m -> m @?= "ping"
        appWrite appData "pong"
    client appData = do
        appWrite appData "ping"
        appRead appData >>= \m -> m @?= "pong"

testConsecutiveClients :: Assertion
testConsecutiveClients = withServer server $ \runClient ->
    replicateM_ numClients (runClient client)
  where
    server _ = pure ()
    client _ = pure ()

testConcurrentClients :: Assertion
testConcurrentClients = withServer server $ \runClient ->
    void . mapConcurrently runClient $ replicate numClients client
  where
    server _ = pure ()
    client _ = pure ()

numClients :: Int
numClients = 10

genPipeName :: IO PipeName
genPipeName = do
    processId <- getProcessId iNVALID_HANDLE_VALUE
    threadId <- myThreadId
    pure . fromString $ show processId <> "-" <> show threadId
