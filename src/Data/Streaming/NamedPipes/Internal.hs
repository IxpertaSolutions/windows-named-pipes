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
    , getPipeName
    , setPipeName
    , ServerSettingsPipe(..)
    , serverSettingsPipe

    -- * ClientSettingsPipe
    , HasPipePath(..)
    , getPipePath
    , setPipePath
    , ClientSettingsPipe(..)
    , clientSettingsPipe

    -- * Utilities
    , defaultReadBufferSize
    )
  where

import Control.Applicative (Const(Const, getConst), pure)
import Data.Function ((.), const, flip)
import Data.Functor (Functor, fmap)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Int (Int)
import System.IO (IO)

import Data.ByteString (ByteString)
import Data.Streaming.Network
    ( HasReadBufferSize(readBufferSizeLens)
    , HasReadWrite(readLens, writeLens)
    , getReadBufferSize
    )

import System.Win32.NamedPipes
    ( PipeHandle
    , PipeMode(MessageMode)
    , PipeName
    , PipePath
    )


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

instance HasReadWrite AppDataPipe where
    readLens f s@AppDataPipe{appReadPipe' = a} =
        f a <$$> \b -> s{appReadPipe' = b}

    writeLens f s@AppDataPipe{appWritePipe' = a} =
        f a <$$> \b -> s{appWritePipe' = b}

-- | Smart constructor for 'AppDataPipe'.
mkAppDataPipe
    :: HasReadBufferSize cfg
    => cfg
    -- ^ Server or client configuration.
    -> (Int -> PipeHandle -> IO ByteString)
    -- ^ Read data from a Named Pipe.
    -> (PipeHandle -> ByteString -> IO ())
    -- ^ Write data into a Named Pipe.
    -> (PipeHandle -> IO ())
    -- ^ Close a Named Pipe handle.
    -> PipeHandle
    -> AppDataPipe
mkAppDataPipe cfg read write close h = AppDataPipe
    { appReadPipe' = read (getReadBufferSize cfg) h
    , appWritePipe' = write h
    , appClosePipe' = close h
    , appRawPipe' = h
    }
{-# INLINE mkAppDataPipe #-}

-- }}} AppDataPipe ------------------------------------------------------------

-- {{{ ServerSettingsPipe -----------------------------------------------------

-- | Type class for accessing 'PipeName' in a data type it's usually a type
-- that represents server settings.
class HasPipeName s where
    -- | Lens for accessing Named Pipe name in a data type, usually server
    -- settings.
    pipeNameLens :: Functor f => (PipeName -> f PipeName) -> s -> f s

-- | Get Named Pipe name from server settings.
getPipeName :: HasPipeName s => s -> PipeName
getPipeName = getConst . pipeNameLens Const

-- | Set Named Pipe name in server settings.
setPipeName :: HasPipeName s => PipeName -> s -> s
setPipeName n = runIdentity . pipeNameLens (const (Identity n))

-- | Settings of a server that listens on a Windows Named Pipe.
data ServerSettingsPipe = ServerSettingsPipe
    { serverPipeName :: !PipeName
    , serverAfterBindPipe :: !(PipeHandle -> IO ())
    , serverReadBufferSizePipe :: !Int
    , serverPipeMode :: !PipeMode
    }

instance HasPipeName ServerSettingsPipe where
    pipeNameLens f s@ServerSettingsPipe{serverPipeName = a} =
        f a <$$> \b -> s{serverPipeName = b}

instance HasReadBufferSize ServerSettingsPipe where
    readBufferSizeLens f s@ServerSettingsPipe{serverReadBufferSizePipe = a} =
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
    , serverPipeMode = MessageMode
    }

-- }}} ServerSettingsPipe -----------------------------------------------------

-- {{{ ClientSettingsPipe -----------------------------------------------------

-- | Type class for accessing 'PipePath' in a data type it's usually a type
-- that represents client settings.
class HasPipePath s where
    -- | Lens for accessing Named Pipe path in a data type, usually client
    -- settings.
    pipePathLens :: Functor f => (PipePath -> f PipePath) -> s -> f s

-- | Get Named Pipe path from client settings.
getPipePath :: HasPipePath s => s -> PipePath
getPipePath = getConst . pipePathLens Const

-- | Set Named Pipe path in client settings.
setPipePath :: HasPipePath s => PipePath -> s -> s
setPipePath p = runIdentity . pipePathLens (const (Identity p))

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
