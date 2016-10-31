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
import Data.Function (($), (.), const, id)
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
    , ClientSettingsPipe
    , ServerSettingsPipe
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

-- | Run a server-clients interaction. HUnit assertions may be used in both
-- server and clients, although do note that exceptions thrown in the server
-- after all clients have finished are very likely to be ignored.
withServer
    :: ((ServerSettingsPipe, ClientSettingsPipe)
        -> (ServerSettingsPipe, ClientSettingsPipe))
    -> (AppDataPipe -> IO ())
    -> (((AppDataPipe -> IO a) -> IO a) -> IO ())
    -> IO ()
withServer conf serverApp k = do
    name <- genPipeName

    (signalReady, waitReady) <- newWait
    let (serverSettings, clientSettings) = conf
            ( setAfterBindPipe signalReady $ serverSettingsPipe name
            , clientSettingsPipe (LocalPipe name)
            )

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

-- | Simple test with a single client and no data being sent either way.
testConnectDisconnect :: Assertion
testConnectDisconnect =
    withServer id (const $ pure ()) ($ const $ pure ())

-- | Test that a server may start sending data first.
testServerPing :: Assertion
testServerPing = withServer id server ($ client)
  where
    server appData = do
        appWrite appData "ping"
        appRead appData >>= \m -> m @?= "pong"
        appWrite appData "end"
    client appData = do
        appRead appData >>= \m -> m @?= "ping"
        appWrite appData "pong"
        appRead appData >>= \m -> m @?= "end"

-- | Test that a client may start sending data first.
testClientPing :: Assertion
testClientPing = withServer id server ($ client)
  where
    server appData = do
        appRead appData >>= \m -> m @?= "ping"
        appWrite appData "pong"
    client appData = do
        appWrite appData "ping"
        appRead appData >>= \m -> m @?= "pong"

-- | Test that multiple clients can connect one after the other. Both server
-- and clients end immediately, increasing the likelihood of race conditions
-- in connection logic being triggered.
testConsecutiveClients :: Assertion
testConsecutiveClients = withServer id server $ \runClient ->
    replicateM_ numClients (runClient client)
  where
    server _ = pure ()
    client _ = pure ()

-- | Test that multiple clients can connect simultaneously. Both server and
-- clients end immediately, increasing the likelihood of race conditions in
-- connection logic being triggered.
testConcurrentClients :: Assertion
testConcurrentClients = withServer id server $ \runClient ->
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
