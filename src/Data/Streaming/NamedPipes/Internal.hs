{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Windows Named Pipes streaming API (internals).
-- Copyright:    (c) 2016, Ixperta Solutions s.r.o.
-- License:      BSD3
--
-- Maintainer:   Ixcom Core Team <ixcom-core@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Windows Named Pipes streaming API (internals).
module Data.Streaming.NamedPipes.Internal
    (
    -- * AppDataPipe
      AppDataPipe(..)
    , mkAppDataPipe

    -- * ServerSettingsPipe
    , HasPipeName(..)
--  , getPipeName
--  , setPipeName
    , ServerSettingsPipe(..)
    , serverSettingsPipe

    -- * ClientSettingsPipe
    , HasPipePath(..)
--  , getPipePath
--  , setPipePath
    , ClientSettingsPipe(..)
    , clientSettingsPipe

    -- * Utilities
    , defaultReadBufferSize
    )
  where

import Control.Applicative (pure)
import Data.Function (const)
import Data.Functor (Functor, fmap)
import Data.Int (Int)
import Data.Maybe (Maybe)
import System.IO (IO)

import Data.ByteString (ByteString)

import System.Win32.NamedPipes (PipeHandle, PipeName, PipePath)


-- | Currently set to 32 KiB, i.e. 32768 B. Same value is used by
-- <https://hackage.haskell.org/package/streaming-commons streaming-commons>
-- library.
defaultReadBufferSize :: Int
defaultReadBufferSize = 32768

-- {{{ AppDataPipe ------------------------------------------------------------

-- | The data passed to an @Application@.
data AppDataPipe = AppDataPipe
    { appReadPipe' :: !(IO ByteString)
    , appWritePipe' :: !(ByteString -> IO ())
    , appClosePipe' :: !(IO ())
    , appRawPipe' :: PipeHandle
    }

instance HasReadWrite AppData where
    readLens f s@AppDataPipe{appReadPipe' = a} =
        f a <$$> \b -> s{appReadPipe' = b}

    writeLens f s@AppDataPipe{appWritePipe' = a} =
        f a <$$> \b -> s{appWritePipe' = b}

-- | Smart constructor for 'AppDataPipe'.
mkAppDataPipe :: HasReadBufferSize cfg => cfg -> PipeHandle -> AppDataPipe
mkAppDataPipe cfg h = AppDataPipe
    { appReadPipe' = read (getReadBufferSize cfg) h
    , appWritePipe' = write h
    , appClosePipe' = close h
    , appRawPipe' = h
    }
  where
    read :: Int -> PipeHandle -> IO ByteString
    read = bufferedRead     -- TODO

    write :: PipeHandle -> ByteString -> IO ()
    write = bufferedWrite   -- TODO

    close :: PipeHandle -> IO ()
    close = close           -- TODO

-- }}} AppDataPipe ------------------------------------------------------------

-- {{{ ServerSettingsPipe -----------------------------------------------------

-- | Type class for accessing 'PipeName' in a data type it's usually a type
-- that represents server settings.
class HasPipeName s where
    pipeNameLens :: Functor f => (PipeName -> f PipeName) -> s -> f s

-- | Settings of a server that listens on a Windows Named Pipe.
data ServerSettingsPipe = ServerSettingsPipe
    { serverPipeName :: !PipeName
    , serverAfterBindPipe :: !(PipeHandle -> IO ())
    , serverReadBufferSizePipe :: !Int
    }

instance HasPipeName ServerSettingsPipe where
    pipeNameLens f s@ServerSettingsPipe{serverPipeName = a} =
        f a <$$> \b -> s{serverPipeName = b}

instance HasAfterBind ServerSettingsPipe where
    serverAfterBindPipe f s@ServerSettingsPipe{serverAfterBindPipe = a} =
        f a <$$> \b -> s{serverAfterBindPipe = b}

instance HasReadBufferSize ServerSettingsPipe where
    readBufferSizeLens f s@ServerSettingsPipe{serverReadBufferSizePipe = a}
        f a <$$> \b -> s{serverReadBufferSizePipe = b}

-- | Smart constructor for 'ServerSettingsPipe'.
--
-- It takes just 'PipeName', instead of 'PipePath', because Named Pipe server
-- can bind only local pipe; therefore, the server name part of the path is
-- fixed.
serverSettingsPipe :: PipeName -> ServerSettingsPipe
serverSettingsPipe name = ServerSettingsPipe
    { serverPipeName = name
    , serverAfterBindPipe = const (pure ())
    , serverReadBufferSizePipe = defaultReadBufferSize
    }

-- }}} ServerSettingsPipe -----------------------------------------------------

-- {{{ ClientSettingsPipe -----------------------------------------------------

-- | Type class for accessing 'PipePath' in a data type it's usually a type
-- that represents client settings.
class HasPipePath s where
    -- | Lens for accessing 'PipePath' stored in some type @s@.
    pipePathLens :: Functor f => (PipePath -> f PipePath) -> s -> f s

-- | Settings of a client that connects to a Windows Named Pipe.
data ClientSettingsPipe = ClientSettingsPipe
    { clientPipePath :: !PipePath
    , clientReadBufferSizePipe :: !Int
    }

instance HasPipePath ClientSettingsPipe where
    pipePathLens f s@ClientSettingsPipe{clientPipePath = a} =
        f a <$$> \b -> s{clientPipePath = b}

instance HasReadBufferSize ClientSettingsPipe where
    readBufferSizeLens f s@ClientSettingsPipe{clientReadBufferSizePipe = a} =
        f a <$$> \b -> s{clientReadBufferSizePipe = b}

-- | Smart constructor for 'ClientSettingsPipe'.
clientSettingsPipe :: PipePath -> ClientSettingsPipe
clientSettingsPipe path = ClientSettingsPipe
    { clientPipePath = path
    , clientReadBufferSizePipe = defaultReadBufferSize
    }

-- }}} ClientSettingsPipe -----------------------------------------------------

-- {{{ Utility functions ------------------------------------------------------
--
-- DO NOT EXPORT THESE!

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap

-- }}} Utility functions ------------------------------------------------------
