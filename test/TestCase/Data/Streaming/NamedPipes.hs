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

import Prelude (mod, succ)

import Control.Applicative ((*>), pure)
import Control.Arrow (first)
import Control.Concurrent (myThreadId, threadDelay, throwTo)
import Control.Concurrent.MVar (newEmptyMVar, tryPutMVar, takeMVar)
import Control.Exception (catch)
import Control.Monad ((>>=), replicateM_, void)
import Data.Bool (otherwise)
import Data.Function (($), (.), const, id)
import Data.Functor ((<$), (<$>))
import Data.Int (Int)
import Data.List (replicate)
import Data.Maybe (Maybe(Just))
import Data.Monoid ((<>))
import Data.Ord ((>=))
import Data.String (fromString)
import Data.Tuple (fst)
import System.IO (IO)
import Text.Show (show)

import Control.Concurrent.Async (async, mapConcurrently, waitAnyCancel)
import System.Win32.Process (getProcessId)
import System.Win32.Types (iNVALID_HANDLE_VALUE)
import qualified Data.ByteString as BS (length, unfoldrN)

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), assertFailure)
import Test.HUnit.Lang (HUnitFailure)

import System.Win32.NamedPipes
    ( PipeMode(MessageMode, StreamMode)
    , PipeName
    , PipePath(LocalPipe)
    , defaultReadBufferSize
    )
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
    , setPipeMode
    )


tests :: [Test]
tests =
    [ testCase "Empty server and client" testConnectDisconnect
    , testCase "Server pings, client waits" testServerPing
    , testCase "Client pings, server waits" testClientPing
    , testCase "Multiple empty clients (consecutive)" testConsecutiveClients
    , testCase "Multiple empty clients (concurrent)" testConcurrentClients
    , testCase "Message mode" testMessageMode
    , testCase "Stream mode" testStreamMode
    , testCase "Long data exchange in stream mode" testLongStreams
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
            ( setAfterBindPipe (const signalReady) $ serverSettingsPipe name
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
    genPipeName :: IO PipeName
    genPipeName = do
        processId <- getProcessId iNVALID_HANDLE_VALUE
        threadId <- myThreadId
        pure . fromString $ show processId <> "-" <> show threadId

newWait :: IO (IO (), IO ())
newWait = do
    ready <- newEmptyMVar
    let signalReady = void $ tryPutMVar ready ()
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
        appRead appData >>= (@?= "pong")
        appWrite appData "end"
    client appData = do
        appRead appData >>= (@?= "ping")
        appWrite appData "pong"
        appRead appData >>= (@?= "end")

-- | Test that a client may start sending data first.
testClientPing :: Assertion
testClientPing = withServer id server ($ client)
  where
    server appData = do
        appRead appData >>= (@?= "ping")
        appWrite appData "pong"
    client appData = do
        appWrite appData "ping"
        appRead appData >>= (@?= "pong")

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

-- | Test that 'MessageMode' works as expected, not joining two messages
-- together as one might expect in a stream mode.
testMessageMode :: Assertion
testMessageMode = withServer conf server ($ client)
  where
    conf = first $ setPipeMode MessageMode
    server appData = do
        appRead appData >>= (@?= "ping1")
        appRead appData >>= (@?= "ping2")
        appWrite appData "end"
    client appData = do
        appWrite appData "ping1"
        appWrite appData "ping2"
        appRead appData >>= (@?= "end")

-- | Test that 'StreamMode' works as expected, joining messages together if
-- a read occurs after multiple writes.
testStreamMode :: Assertion
testStreamMode = do
    (signal, wait) <- newWait
    let conf = first $ setPipeMode StreamMode
    let server appData = do
            wait
            appRead appData >>= (@?= "ping1ping2")
            appWrite appData "end"
    let client appData = do
            appWrite appData "ping1"
            appWrite appData "ping2"
            signal
            appRead appData >>= (@?= "end")
    withServer conf server ($ client)

-- | Test sending larger (than buffer size) amounts of data through pipes.
--
-- This unfortunately doesn't work in 'MessageMode' due to
-- 'System.Win32.NamedPipes.Internal.readFile' not handling @ERROR_MORE_DATA@.
testLongStreams :: Assertion
testLongStreams = withServer conf server ($ client)
  where
    conf = first $ setPipeMode StreamMode
    server appData = do
        appReadN appData (BS.length long1) >>= (@?= long1)
        appWrite appData "ack1"
        appReadN appData (BS.length long2) >>= (@?= long2)
        appWrite appData "ack2"
    client appData = do
        appWrite appData long1
        appRead appData >>= (@?= "ack1")
        appWrite appData long2
        appRead appData >>= (@?= "ack2")

    appReadN appData n = go ""
      where
        go readSoFar
          | BS.length readSoFar >= n = pure readSoFar
          | otherwise = (readSoFar <>) <$> appRead appData >>= go

    -- Don't cycle when reading in chunks.
    prime = 251
    gen n = Just (n `mod` prime, succ n `mod` prime)

    long1 = fst $ BS.unfoldrN (2 * defaultReadBufferSize) gen 0
        -- Explicitly longer than defaultReadBufferSize.

    long2 = fst $ BS.unfoldrN (16 * defaultReadBufferSize) gen 0
        -- Quite long indeed.
