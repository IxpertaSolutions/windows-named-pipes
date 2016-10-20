{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       $HEADER$
-- Description:  Windows Named Pipes streaming API.
-- Copyright:    (c) 2016, Ixperta Solutions s.r.o.
-- License:      BSD3
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
    , getPipeName
    , setPipeName
    , ServerSettingsPipe
    , serverSettingsPipe
    , runPipeServer

    -- * Client
    , HasPipePath(pipePathLens)
    , getPipePath
    , setPipePath
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

import Control.Applicative ((*>))
import Control.Exception (bracket, finally, mask, onException)
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void, when)
import Data.Function (($), (.))
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
        ( ClientSettingsPipe
        , clientPipePath
        )
    , HasPipeName(pipeNameLens)
    , HasPipePath(pipePathLens)
    , ServerSettingsPipe
        ( ServerSettingsPipe
        , serverAfterBindPipe
        , serverPipeMode
        , serverPipeName
        , serverReadBufferSizePipe
        )
    , clientSettingsPipe
    , defaultReadBufferSize
    , getPipeName
    , getPipePath
    , mkAppDataPipe
    , serverSettingsPipe
    , setPipeName
    , setPipePath
    )
import System.Win32.NamedPipes
    ( PipeHandle
    , bindPipe
    , closePipe
    , connectPipe
    , disconnectPipe
    , getPipe
    , readPipe
    , writePipe
    )


-- | Run an server application function with the given settings. This function
-- will accept connections on a Named Pipe, and spawn a new thread for each
-- connection.
--
-- Example:
--
-- @
-- 'runPipeServer' ('serverSettingsPipe' pipeName) $ \appData ->
--     -- -->8-- Server code.
-- @
runPipeServer :: ServerSettingsPipe -> (AppDataPipe -> IO ()) -> IO a
runPipeServer cfg@ServerSettingsPipe{..} app = forever . withPipe $ \pipe -> do
    haveClient <- connectPipe pipe
    when haveClient $ serve pipe
  where
    withPipe :: (PipeHandle -> IO ()) -> IO ()
    withPipe k = do
        pipe <- bindPipe'
        (serverAfterBindPipe pipe *> k pipe) `onException` closePipe pipe
            -- It is important to close pipe only when exception is detected,
            -- otherwise we would close an active handle.
            --
            -- On this level we can use plain closePipe without disconnectPipe.
            -- If we have detected that client is connected then we are
            -- necessarily inside connection handling thread (see "serve"
            -- function).

    -- | We are assuming that it is optimal to use same size of input/output
    -- buffer as the read size when calling readPipe.
    bindPipe' :: IO PipeHandle
    bindPipe' = bindPipe serverReadBufferSizePipe serverPipeMode serverPipeName

    -- We need to use closePipe' instead of closePipe, since AppDataPipe are
    -- living in "app", which means that client is connected to a Named Pipe.
    -- See closePipe' for details.
    mkAppData :: PipeHandle -> AppDataPipe
    mkAppData = mkAppDataPipe cfg readPipe writePipe closePipe'

    -- Implementation of serve is based on how streaming-commons does it,
    -- i.e. we have masked asynchronous exception during forkIO, and defered
    -- their apperence to "app" evaluation.
    serve :: PipeHandle -> IO ()
    serve pipe = mask $ \restore ->
        void . forkIO
            $ restore (app (mkAppData pipe))
                `finally` closePipe' pipe

    -- When client is already connected to a named pipe, server has to
    -- disconnect it, which forces client end of the named pipe to be closed,
    -- before closing the server side end of a named pipe.
    --
    -- More can be found in MSDN documentation of DisconnectNamedPipe function:
    -- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365166(v=vs.85).aspx
    closePipe' :: PipeHandle -> IO ()
    closePipe' pipe = do
        isHandleValid <- disconnectPipe pipe
        unless isHandleValid $ void (closePipe pipe)

    -- Implementation of runPipeServer is inspired by streamings-common, and
    -- Multithreaded Pipe Server example from MSDN:
    -- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365588(v=vs.85).aspx

-- | Run a client application function by connecting to the specified server
-- via Named Pipe. Client function is evaluated in current thread.
--
-- Example:
--
-- @
-- 'runPipeClient' ('clientSettingsPipe' pipePath) $ \appData ->
--     -- -->8-- Client code.
-- @
runPipeClient :: ClientSettingsPipe -> (AppDataPipe -> IO a) -> IO a
runPipeClient cfg@ClientSettingsPipe{..} app = withPipe $ app . mkAppData
  where
    withPipe = getPipe clientPipePath `bracket` closePipe
    mkAppData = mkAppDataPipe cfg readPipe writePipe (void . closePipe)

    -- Implementation of runPipeServer is inspired by streamings-common, and
    -- Multithreaded Pipe Server example from MSDN:
    -- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365592(v=vs.85).aspx
