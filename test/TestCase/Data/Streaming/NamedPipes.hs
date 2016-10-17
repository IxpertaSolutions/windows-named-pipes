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
-- License:      AllRightsReserved
--
-- Maintainer:   Ixcom Core Team <ixcom-core@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC-specific language extensions.
--
-- Tests for "Data.Streaming.NamedPipes" module.
module TestCase.Data.Streaming.NamedPipes (tests)
  where

import Control.Applicative (pure)
import Control.Concurrent (threadDelay)
import Data.Function (($), const)
import Data.Functor ((<$))
import System.IO (IO)

import Control.Concurrent.Async (Async, async, waitAnyCancel)

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import System.Win32.NamedPipes (PipeName, PipePath(LocalPipe))
import Data.Streaming.NamedPipes
    ( AppDataPipe
    , ClientSettingsPipe
    , clientSettingsPipe
    , runPipeClient
    , runPipeServer
    , serverSettingsPipe
    )


tests :: [Test]
tests =
    [ testCase "Empty server and client" testConnectDisconnect
    ]

withServer
    :: PipeName
    -> (AppDataPipe -> IO ())
    -> (ClientSettingsPipe -> IO (Async ())) -> IO ()
withServer name serverApp k = do
    server <- async $ runPipeServer (serverSettingsPipe name) serverApp
    client <- k $ clientSettingsPipe (LocalPipe name)
    timeOut <- async $ threadDelay 500000   -- 500 ms
    () <$ waitAnyCancel [server, client, timeOut]

testConnectDisconnect :: Assertion
testConnectDisconnect = withServer pipeName (const $ pure ()) $ \cfg ->
    async $ runPipeClient cfg (const $ pure ())

pipeName :: PipeName
pipeName = "win-named-pipes-test"
