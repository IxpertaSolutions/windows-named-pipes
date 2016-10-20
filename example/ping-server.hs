{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       $HEADER$
-- Description:  Example Named Pipes server.
-- Copyright:    (c) 2016, Ixperta Solutions s.r.o.
-- License:      BSD3
--
-- Maintainer:   Ixcom Core Team <ixcom-core@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC-specific language extensions.
--
-- Example Named Pipes server.
module Main (main)
  where

import Prelude (Num((+)), error)

import Control.Applicative ((<*>), pure)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (AssertionFailed(AssertionFailed), throwIO)
import Control.Monad ((>>=))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Monoid ((<>))
import Data.String (String)
import System.Environment (getArgs)
import System.IO
    ( BufferMode(LineBuffering)
    , IO
    , IOMode(AppendMode)
    , hPutStrLn
    , hSetBuffering
    , withBinaryFile
    )
import Text.Show (show)

import System.Win32.NamedPipes (PipeName)
import Data.Streaming.NamedPipes
    ( appRead
    , appWrite
    , runPipeServer
    , serverSettingsPipe
    )


data Counters = Counters
    { incServerReceivedPing :: IO ()
    , incServerSendPong :: IO ()
    , getCounters :: IO (Int, Int)
    }

newCounters :: IO Counters
newCounters = do
    rx <- newIORef 0
    tx <- newIORef 0
    pure Counters
        { incServerReceivedPing = increment rx
        , incServerSendPong = increment tx
        , getCounters = (,) <$> readIORef rx <*> readIORef tx
        }
  where
    increment ref = atomicModifyIORef' ref $ \a -> (a + 1, ())

main :: IO ()
main = getArgs >>= \case
    [] -> error "Usage: LOG_FILE"
    fp : _ -> withBinaryFile fp AppendMode $ \h -> do
        hSetBuffering h LineBuffering
        hPutStrLn h "Starting the example..."
        counters <- newCounters
        sid <- forkIO $ server counters
        threadDelay 50000000
        getCounters counters >>= \(rx, tx) -> do
            hPutStrLn h $ "Server received " <> show rx <> " message(s)."
            hPutStrLn h $ "Server sent " <> show tx <> " message(s)."
        hPutStrLn h "Time-out reched; ending the example..."
        killThread sid

assertFailure :: String -> IO ()
assertFailure = throwIO . AssertionFailed

server :: Counters -> IO ()
server counters = runPipeServer (serverSettingsPipe pipeName) serverApp
  where
    serverApp appData = appRead appData >>= \case
        "Ping" -> do
            incServerReceivedPing counters
            appWrite appData "Pong"
            incServerSendPong counters
            serverApp appData
        rq -> do
            assertFailure $ "Server received unexpected request: " <> show rq

pipeName :: PipeName
pipeName = "win-named-pipes-test"
