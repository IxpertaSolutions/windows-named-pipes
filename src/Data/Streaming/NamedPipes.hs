{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Windows Named Pipes streaming API.
-- Copyright:    (c) 2016, Ixperta Solutions s.r.o.
-- License:      AllRightsReserved
--
-- Maintainer:   Ixcom Core Team <ixcom-core@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Windows Named Pipes streaming API.
module Data.Streaming.NamedPipes
    (
    -- * Common
      AppDataPipe
    , defaultReadBufferSize

    -- * Server
    , HasPipeName(pipeNameLens)
    , ServerSettingsPipe
    , serverSettingsPipe
    , runPipeServer

    -- * Client
    , HasPipePath(pipePathLens)
    , ClientSettingsPipe
    , clientSettingsPipe
    , runPipeClient

    -- * Re-exported
    , appRead
    , appWrite
    , getReadBufferSize
    , setReadBufferSize
    )
  where

import System.IO (IO)

import Data.Streaming.Network
    ( appRead
    , appWrite
    , getReadBufferSize
    , setReadBufferSize
    )
import Data.Streaming.NamedPipes.Internal
    ( AppDataPipe
    , ClientSettingsPipe
    , HasPipeName(pipeNameLens)
    , HasPipePath(pipePathLens)
    , ServerSettingsPipe
    , clientSettingsPipe
    , defaultReadBufferSize
    , serverSettingsPipe
    )


runPipeServer :: ServerSettingsPipe -> (AppDataPipe -> IO ()) -> IO a
runPipeServer = runPipeServer   -- TODO

runPipeClient :: ClientSettingsPipe -> (AppDataPipe -> IO a) -> IO a
runPipeClient = runPipeClient   -- TODO
