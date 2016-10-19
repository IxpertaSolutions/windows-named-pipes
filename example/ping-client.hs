{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       $HEADER$
-- Description:  Example Named Pipes client.
-- Copyright:    (c) 2016, Ixperta Solutions s.r.o.
-- License:      BSD3
--
-- Maintainer:   Ixcom Core Team <ixcom-core@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC-specific language extensions.
--
-- Example Named Pipes client.
module Main (main)
  where

import Prelude (Num((-), (+)), error)

import Control.Applicative ((<*>), pure)
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

import System.Win32.NamedPipes (PipeName, PipePath(LocalPipe))
import Data.Streaming.NamedPipes
    ( AppDataPipe
    , appRead
    , appWrite
    , clientSettingsPipe
    , runPipeClient
    )


data Counters = Counters
    { incClientSendPing :: IO ()
    , incClientReceivedPong :: IO ()
    , getCounters :: IO (Int, Int)
    }

newCounters :: IO Counters
newCounters = do
    rx <- newIORef 0
    tx <- newIORef 0
    pure Counters
        { incClientSendPing = increment rx
        , incClientReceivedPong = increment tx
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
        client counters
        getCounters counters >>= \(tx, rx) -> do
            hPutStrLn h $ "Client sent " <> show tx <> " message(s)."
            hPutStrLn h $ "Client received " <> show rx <> " message(s)."
        hPutStrLn h "Ending the example..."

assertFailure :: String -> IO ()
assertFailure = throwIO . AssertionFailed

client :: Counters -> IO ()
client counters =
    runPipeClient (clientSettingsPipe (LocalPipe pipeName)) (loop 10)
  where
    loop :: Int -> AppDataPipe -> IO ()
    loop 0 _       = pure ()
    loop n appData = do
        appWrite appData "Ping"
        incClientSendPing counters
        appRead appData >>= \case
            "Pong" -> do
                incClientReceivedPong counters
                loop (n - 1) appData
            rsp -> do
                assertFailure $ "Client received unexpected response: " <> show rsp

pipeName :: PipeName
pipeName = "win-named-pipes-test"
