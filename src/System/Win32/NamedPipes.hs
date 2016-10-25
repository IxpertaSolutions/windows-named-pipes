{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Example Haskell module.
-- Copyright:    (c) 2016, Ixperta Solutions s.r.o.
-- License:      BSD3
--
-- Maintainer:   Ixcom Core Team <ixcom-core@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Example Haskell module.
module System.Win32.NamedPipes
    (
    -- * Handle
      PipeHandle

    -- * Named Pipe Path
    , PipeName(..)
    , pipeName

    , PipePath(..)
    , pipePath

    -- * Operations on Named Pipes
    , PipeMode(..)
    , bindPipe
    , getPipe
    , readPipe
    , writePipe
    , closePipe
    , disconnectPipe
    , connectPipe
    )
  where

import Prelude (error, fromIntegral)

import Control.Monad (return, unless, void)
import Data.Bits ((.|.))
import Data.Bool (Bool, otherwise)
import Data.Eq (Eq((==)))
import Data.Function (($), (.), on)
import Data.Functor (fmap)
import Data.Int (Int)
import qualified Data.List as List (notElem)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.Ord (Ord(compare))
import Data.String (IsString(fromString), String)
import GHC.Generics (Generic)
import System.IO (FilePath, IO)
import Text.Read (Read)
import Text.Show (Show(showsPrec), showString)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as ByteString (createAndTrim)
import qualified Data.ByteString.Unsafe as ByteString (unsafeUseAsCStringLen)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI (mk)
import System.Win32.Types (HANDLE, failIf)
import System.Win32.File
    ( createFile
    , fILE_SHARE_NONE
    , gENERIC_READ
    , gENERIC_WRITE
    , oPEN_EXISTING
    , sECURITY_ANONYMOUS
    , win32_ReadFile
    , win32_WriteFile
    )

import System.Win32.NamedPipes.Internal
    ( closeHandleCheckInvalidHandle
    , connectNamedPipe
    , createNamedPipe
    , disconnectNamedPipe
    , flushFileBuffersCheckInvalidHandle
    , pIPE_ACCESS_DUPLEX
    , pIPE_READMODE_BYTE
    , pIPE_READMODE_MESSAGE
    , pIPE_TYPE_BYTE
    , pIPE_TYPE_MESSAGE
    , pIPE_UNLIMITED_INSTANCES
    , pIPE_WAIT
    )


type PipeHandle = HANDLE

-- {{{ PipeName ---------------------------------------------------------------

-- | Name of the Named Pipe without the path prefix. For obvious reasons it
-- can not contain backslash (@\'\\\\\'@), which is a path separator on
-- Windows.
newtype PipeName = PipeName String
  deriving Generic

-- | Smart constructor for 'PipeName' that returns 'Nothing', when the string
-- contains backslash (@\'\\\'@).
pipeName :: String -> Maybe PipeName
pipeName str
  | isValid = Just $ PipeName str
  | otherwise = Nothing
  where
    -- MSDN documentation specifies that backslash is the only character that
    -- is the only invalid character in pipe name.
    isValid = '\\' `List.notElem` str

-- | Utility function that simplifies implementation of 'Eq', and 'Ord'
-- instances.
onCiPipeName :: (CI String -> CI String -> a) -> PipeName -> PipeName -> a
onCiPipeName f (PipeName n1) (PipeName n2) = (f `on` CI.mk) n1 n2
{-# INLINE onCiPipeName #-}

instance Show PipeName where
    showsPrec _ (PipeName n) = showString n

-- | Warning: Use with caution! Throws error if string contains backslash. Use
-- 'pipeName' smart constructor to enforce safety.
instance IsString PipeName where
    fromString = fromMaybe parseError . pipeName
      where
        parseError =
            "fromString: PipeName: Backslash ('\\') is an invalid character."

-- | Equality is case-insensitive.
instance Eq PipeName where
    (==) = onCiPipeName (==)

-- | Ordering is case-insensitive.
instance Ord PipeName where
    compare = onCiPipeName compare

-- }}} PipeName ---------------------------------------------------------------

-- {{{ PipePath ---------------------------------------------------------------

-- | Represents path to a Named Pipe. Named pipes have special paths in the
-- form:
--
-- @
-- \\\\%ServerName%\\pipe\\%PipeName%
-- @
--
-- 'LocalPipe' is interpreted as:
--
-- @
-- \\\\.\\pipe\\%PipeName%
-- @
data PipePath
    = LocalPipe PipeName
    | RemotePipe String PipeName
  deriving Generic

-- | Smart constructor for 'PipePath'. It analyses server name, and based on
-- its value it creates either 'LocalPipe' or 'RemotePipe'. The former
-- ('LocalPipe') is created when server name is equal to @\".\"@.
pipePath
    :: String
    -- ^ Server Name. WARNING: Currently we aren't checking validity of this
    -- string.
    -> PipeName
    -- ^ Name of the pipe relative to Server Name.
    -> PipePath
pipePath "." = LocalPipe
pipePath srv = RemotePipe srv

-- | Utility function that simplifies implementation of 'Eq', and 'Ord'
-- instances.
onCiPipePath
    :: ((Maybe (CI String), CI String) -> (Maybe (CI String), CI String) -> a)
    -> PipePath
    -> PipePath
    -> a
onCiPipePath = (`on` toCI)
  where
    toCI = \case
        LocalPipe (PipeName n) -> (Nothing, CI.mk n)
        RemotePipe srv (PipeName n) -> (Just $ CI.mk srv, CI.mk n)
{-# INLINE onCiPipePath #-}

instance Show PipePath where
    showsPrec _ = showString . pipePathToFilePath

-- | Equality is case-insensitive.
instance Eq PipePath where
    (==) = onCiPipePath (==)

-- | Ordering is case-insensitive.
instance Ord PipePath where
    compare = onCiPipePath compare

-- | Convert 'PipePath' to a valid Windows 'FilePath' referencing a Named Pipe.
-- Such paths are in form:
--
-- @
-- \\\\%ServerName%\\pipe\\%PipeName%
-- @
pipePathToFilePath :: PipePath -> FilePath
pipePathToFilePath = \case
    LocalPipe n -> format "." n
    RemotePipe srv n -> format srv n
  where
    format serverName (PipeName name) =
        "\\\\" <> serverName <> "\\pipe\\" <> name

-- }}} PipePath ---------------------------------------------------------------

-- {{{ Operations on Named Pipes ----------------------------------------------

-- {{{ bindPipe, connectPipe --------------------------------------------------

data PipeMode = MessageMode | StreamMode
  deriving (Eq, Generic, Ord, Show, Read)

-- | Create Named Pipe, specified by its name relative to the current machine,
-- and return its handle for further use.
bindPipe
    :: Int
    -- ^ Input and output buffer size.
    -> PipeMode
    -- ^ Mode in which the pipe should be created.
    -> PipeName
    -> IO PipeHandle
bindPipe bufSize mode name =
    createNamedPipe strName openMode pipeMode maxInstances inBufSize outBufSize
        timeOut securityAttrs
  where
    strName = pipePathToFilePath $ LocalPipe name

    openMode = pIPE_ACCESS_DUPLEX

    pipeMode = (pIPE_WAIT .|.) $ case mode of
        MessageMode -> pIPE_TYPE_MESSAGE .|. pIPE_READMODE_MESSAGE
        StreamMode -> pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE

    maxInstances = pIPE_UNLIMITED_INSTANCES

    inBufSize = bufSize'
    outBufSize = bufSize'
    bufSize' = fromIntegral bufSize

    -- Zero means default value of 50 ms.
    -- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365150(v=vs.85).aspx
    timeOut = 0

    -- Nothing means that default security descriptor will be used, and the
    -- PipeHandle cannot be inherited.
    securityAttrs = Nothing

-- | Enables a named pipe server process to wait for a client process to
-- connect to an instance of a named pipe. A client process connects by calling
-- the 'getPipe' function.
connectPipe :: PipeHandle -> IO Bool
connectPipe = connectNamedPipe
{-# INLINE connectPipe #-}

-- }}} bindPipe, connectPipe --------------------------------------------------

-- {{{ getPipe, closePipe, disconnectPipe -------------------------------------

-- | Open client side of a Named Pipe. Note that client can not choose the mode
-- in which it will be communicating via the pipe ('pIPE_TYPE_BYTE', or
-- 'pIPE_TYPE_MESSAGE'), that is up to the server.
getPipe :: PipePath -> IO PipeHandle
getPipe path = createFile fileName accessMode shareMode securityAttrs
    createMode fileAttrOrFlag templateFile
  where
    fileName = pipePathToFilePath path

    -- Following values are based on Named Pipe Client example from MSDN:
    -- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365592(v=vs.85).aspx
    accessMode = gENERIC_READ .|. gENERIC_WRITE
    shareMode = fILE_SHARE_NONE
    securityAttrs = Nothing
    createMode = oPEN_EXISTING
    fileAttrOrFlag = sECURITY_ANONYMOUS
    templateFile = Nothing

-- | Close a Named Pipe handle.
--
-- Function returns 'Data.Bool.False' when operation was successful, and
-- 'Data.Bool.True' when @ERROR_INVALID_HANDLE@ was returned by underlying
-- low-level call. This function enables us to ignore already closed handles.
closePipe :: PipeHandle -> IO Bool
closePipe = closeHandleCheckInvalidHandle
{-# INLINE closePipe #-}

-- | Disconnects the server end of a named pipe instance from a client process.
--
-- When the server process disconnects a pipe instance, any unread data in the
-- pipe is discarded. Before calling low-level 'disconnectNamedPipe', this
-- function calls 'flushFileBuffers' to prevent such data loss.
--
-- Function returns 'Data.Bool.False' when operation was successful, and
-- 'Data.Bool.True' when @ERROR_INVALID_HANDLE@ was returned by underlying
-- low-level call. This function enables us to ignore already closed handles.
disconnectPipe :: PipeHandle -> IO Bool
disconnectPipe pipe = do
    isInvalidHandle <- flushFileBuffersCheckInvalidHandle pipe
    unless isInvalidHandle $ disconnectNamedPipe pipe
    return isInvalidHandle

-- }}} getPipe, closePipe, disconnectPipe -------------------------------------

-- {{{ readPipe, writePipe ----------------------------------------------------

-- | Read data up to specified size from a Named Pipe.
readPipe
    :: Int
    -- ^ Maximum number of bytes to read from a pipe.
    -> PipeHandle
    -> IO ByteString
readPipe bufSize h =
    ByteString.createAndTrim bufSize $ fmap fromIntegral . readPipe'
  where
    -- We are assuming that zero return value is an error, but that may not be
    -- an error in case of overlapped I/O. We aren't using that, so we have cut
    -- the corners here.
    --
    -- See MSDN documentation on details about Win32 ReadFile function:
    -- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365467(v=vs.85).aspx
    readPipe' ptr = failIf (== 0) "readPipe"
         $ win32_ReadFile h ptr (fromIntegral bufSize) overlapped

    -- Not using overlapped (asynchronous) I/O.
    overlapped = Nothing

-- | Write data to a Named Pipe.
writePipe :: PipeHandle -> ByteString -> IO ()
writePipe h bs = ByteString.unsafeUseAsCStringLen bs $ void . writePipe'
  where
    -- We are assuming that zero return value is an error, but that may not be
    -- an error in case of overlapped I/O. We aren't using that, so we have cut
    -- the corners here.
    --
    -- See MSDN documentation on details about Win32 WriteFile function:
    -- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365747(v=vs.85).aspx
    writePipe' (ptr, len) = failIf (== 0) "writePipe"
        $ win32_WriteFile h ptr (fromIntegral len) overlapped

    -- Not using overlapped (asynchronous) I/O.
    overlapped = Nothing

-- }}} readPipe, writePipe ----------------------------------------------------
