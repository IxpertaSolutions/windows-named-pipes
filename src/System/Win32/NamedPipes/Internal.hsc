{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Bindings to Named Pipes Win32 API (internals).
-- Copyright:    (c) 2016, Ixperta Solutions s.r.o.
-- License:      BSD3
--
-- Maintainer:   Ixcom Core Team <ixcom-core@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Bindings to Named Pipes Win32 API.
module System.Win32.NamedPipes.Internal
    (
    -- * Win32 Style Wrappers
      createNamedPipe
    , connectNamedPipe
    , disconnectNamedPipe
    , waitNamedPipe

    -- * Parameters
    , OutBufferSize
    , InBufferSize
    , DefaultTimeOut

    -- ** MaxInstances
    , MaxInstances
    , pIPE_UNLIMITED_INSTANCES

    -- ** PipeOpenMode
    , PipeOpenMode
    , pIPE_ACCESS_DUPLEX
    , pIPE_ACCESS_INBOUND
    , pIPE_ACCESS_OUTBOUND
    , fILE_FLAG_FIRST_PIPE_INSTANCE
    , fILE_FLAG_WRITE_THROUGH

    -- ** PipeMode
    , PipeMode
    , pIPE_ACCEPT_REMOTE_CLIENTS
    , pIPE_NOWAIT
    , pIPE_READMODE_BYTE
    , pIPE_READMODE_MESSAGE
    , pIPE_REJECT_REMOTE_CLIENTS
    , pIPE_TYPE_BYTE
    , pIPE_TYPE_MESSAGE
    , pIPE_WAIT

    -- ** TimeOut
    , TimeOut
    , nMPWAIT_USE_DEFAULT_WAIT
    , nMPWAIT_WAIT_FOREVER

    -- * Utility functions
    , catchPipeBusy
    , closeHandleCheckInvalidHandle
    , flushFileBuffersCheckInvalidHandle

    -- * Low-level FFI Calls
    , c_CreateNamedPipe
    , c_ConnectNamedPipe
    , c_DisconnectNamedPipe
    , c_WaitNamedPipe
    )
  where

import Control.Monad ((>>=), return)
import Data.Bool (Bool)
import Data.Eq (Eq((==)))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Monoid ((<>))
import Data.String (String)
import System.IO (FilePath, IO)
import System.IO.Error (IOError, catchIOError, ioError)
import Text.Show (show)

import System.Win32.File
    ( LPOVERLAPPED
    , LPSECURITY_ATTRIBUTES
    , c_CloseHandle
    , c_FlushFileBuffers
    )
import System.Win32.Types
    ( DWORD
    , ErrCode
    , HANDLE
    , LPCTSTR
    , failIf
    , failIfFalse_
    , failUnlessSuccessOr
    , getLastError
    , iNVALID_HANDLE_VALUE
    , maybePtr
    , nullPtr
    , withTString
    )

#include <windows.h>


-- {{{ Windows Calling Convention ---------------------------------------------
--
-- This was inspired by Win32 library.

##if defined(i386_HOST_ARCH)

##define WINDOWS_CCONV stdcall

##elif defined(x86_64_HOST_ARCH)

##define WINDOWS_CCONV ccall

##else

##error Unknown mingw32 arch.

##endif

-- }}} Windows Calling Convention ---------------------------------------------

type OutBufferSize = DWORD
type InBufferSize = DWORD
type DefaultTimeOut = DWORD

-- {{{ MaxInstances -----------------------------------------------------------

type MaxInstances = DWORD

pIPE_UNLIMITED_INSTANCES :: MaxInstances
pIPE_UNLIMITED_INSTANCES = #{const PIPE_UNLIMITED_INSTANCES}

-- }}} MaxInstances -----------------------------------------------------------

-- {{{ PipeOpenMode -----------------------------------------------------------

type PipeOpenMode = DWORD

-- | The pipe is bi-directional; both server and client processes can read from
-- and write to the pipe. This mode gives the server the equivalent of
-- 'gENERIC_READ' and 'gENERIC_WRITE' access to the pipe. The client can
-- specify 'gENERIC_READ' or 'gENERIC_WRITE', or both, when it connects to the
-- pipe using the 'System.Win32.Files.createFile' function.
pIPE_ACCESS_DUPLEX :: PipeOpenMode
pIPE_ACCESS_DUPLEX = #{const PIPE_ACCESS_DUPLEX}

-- | The flow of data in the pipe goes from client to server only. This mode
-- gives the server the equivalent of GENERIC_READ access to the pipe. The
-- client must specify 'gENERIC_WRITE' access when connecting to the pipe. If
-- the client must read pipe settings by calling the @GetNamedPipeInfo@ or
-- @GetNamedPipeHandleState@ functions, the client must specify 'gENERIC_WRITE'
-- and 'fILE_READ_ATTRIBUTES' access when connecting to the pipe.
pIPE_ACCESS_INBOUND :: PipeOpenMode
pIPE_ACCESS_INBOUND = #{const PIPE_ACCESS_INBOUND}

-- | The flow of data in the pipe goes from server to client only. This mode
-- gives the server the equivalent of GENERIC_WRITE access to the pipe. The
-- client must specify 'gENERIC_READ' access when connecting to the pipe. If
-- the client must change pipe settings by calling the SetNamedPipeHandleState
-- function, the client must specify GENERIC_READ and 'fILE_WRITE_ATTRIBUTES'
-- access when connecting to the pipe.
pIPE_ACCESS_OUTBOUND :: PipeOpenMode
pIPE_ACCESS_OUTBOUND = #{const PIPE_ACCESS_OUTBOUND}

-- | If you attempt to create multiple instances of a pipe with this flag,
-- creation of the first instance succeeds, but creation of the next instance
-- fails with 'eRROR_ACCESS_DENIED'.
--
-- Windows 2000: This flag is not supported until Windows 2000 SP2 and Windows
-- XP.
fILE_FLAG_FIRST_PIPE_INSTANCE :: PipeOpenMode
fILE_FLAG_FIRST_PIPE_INSTANCE = #{const FILE_FLAG_FIRST_PIPE_INSTANCE}

-- | Write-through mode is enabled. This mode affects only write operations on
-- byte-type pipes and, then, only when the client and server processes are on
-- different computers. If this mode is enabled, functions writing to a named
-- pipe do not return until the data written is transmitted across the network
-- and is in the pipe's buffer on the remote computer. If this mode is not
-- enabled, the system enhances the efficiency of network operations by
-- buffering data until a minimum number of bytes accumulate or until a maximum
-- time elapses.
fILE_FLAG_WRITE_THROUGH :: PipeOpenMode
fILE_FLAG_WRITE_THROUGH = #{const FILE_FLAG_WRITE_THROUGH}

--fILE_FLAG_OVERLAPPED = #{const FILE_FLAG_OVERLAPPED}  --  NOT SUPPORTED!

-- }}} PipeOpenMode -----------------------------------------------------------

-- {{{ PipeMode ---------------------------------------------------------------

type PipeMode = DWORD

-- | Data is written to the pipe as a stream of bytes. This mode cannot be used
-- with 'pIPE_READMODE_MESSAGE'. The pipe does not distinguish bytes written
-- during different write operations.
pIPE_TYPE_BYTE :: PipeMode
pIPE_TYPE_BYTE = #{const PIPE_TYPE_BYTE}

-- | Data is written to the pipe as a stream of messages. The pipe treats the
-- bytes written during each write operation as a message unit. The
-- 'getLastError' function returns 'eRROR_MORE_DATA' when a message is not read
-- completely. This mode can be used with either 'pIPE_READMODE_MESSAGE' or
-- 'pIPE_READMODE_BYTE'.
pIPE_TYPE_MESSAGE :: PipeMode
pIPE_TYPE_MESSAGE = #{const PIPE_TYPE_MESSAGE}

-- | Data is read from the pipe as a stream of bytes. This mode can be used
-- with either PIPE_TYPE_MESSAGE or PIPE_TYPE_BYTE.
pIPE_READMODE_BYTE :: PipeMode
pIPE_READMODE_BYTE = #{const PIPE_READMODE_BYTE}

-- | Data is read from the pipe as a stream of messages. This mode can be only
-- used if PIPE_TYPE_MESSAGE is also specified.
pIPE_READMODE_MESSAGE :: PipeMode
pIPE_READMODE_MESSAGE = #{const PIPE_READMODE_MESSAGE}

-- | Blocking mode is enabled. When the pipe handle is specified in the
-- ReadFile, WriteFile, or ConnectNamedPipe function, the operations are not
-- completed until there is data to read, all data is written, or a client is
-- connected. Use of this mode can mean waiting indefinitely in some situations
-- for a client process to perform an action.
pIPE_WAIT :: PipeMode
pIPE_WAIT = #{const PIPE_WAIT}

-- | Nonblocking mode is enabled. In this mode, ReadFile, WriteFile, and
-- ConnectNamedPipe always return immediately.
--
-- Note that nonblocking mode is supported for compatibility with Microsoft LAN
-- Manager version 2.0 and should not be used to achieve asynchronous I/O with
-- named pipes. For more information on asynchronous pipe I/O, see Synchronous
-- and Overlapped Input and Output on MSDN.
pIPE_NOWAIT :: PipeMode
pIPE_NOWAIT = #{const PIPE_NOWAIT}

-- | Connections from remote clients can be accepted and checked against
-- the security descriptor for the pipe.
--
-- Windows Server 2003 and Windows XP/2000:  This flag is not supported.
pIPE_ACCEPT_REMOTE_CLIENTS :: PipeMode
pIPE_ACCEPT_REMOTE_CLIENTS = #{const PIPE_ACCEPT_REMOTE_CLIENTS}

-- | Connections from remote clients are automatically rejected.
--
-- Windows Server 2003 and Windows XP/2000:  This flag is not supported. To
-- achieve the same results, deny access to the pipe to the NETWORK ACE.
pIPE_REJECT_REMOTE_CLIENTS :: PipeMode
pIPE_REJECT_REMOTE_CLIENTS = #{const PIPE_REJECT_REMOTE_CLIENTS}

-- }}} PipeMode ---------------------------------------------------------------

-- {{{ TimeOut ----------------------------------------------------------------

type TimeOut = DWORD

-- | The time-out interval is the default value specified by the server
-- process in the CreateNamedPipe function.
nMPWAIT_USE_DEFAULT_WAIT :: TimeOut
nMPWAIT_USE_DEFAULT_WAIT = #{const NMPWAIT_USE_DEFAULT_WAIT}

-- | The function does not return until an instance of the named pipe is
-- available.
nMPWAIT_WAIT_FOREVER :: TimeOut
nMPWAIT_WAIT_FOREVER = #{const NMPWAIT_WAIT_FOREVER}

-- }}} TimeOut ----------------------------------------------------------------

-- {{{ createNamedPipe --------------------------------------------------------

-- | Creates an instance of a named pipe and returns a handle for subsequent
-- pipe operations. A named pipe server process uses this function either to
-- create the first instance of a specific named pipe and establish its basic
-- attributes or to create a new instance of an existing named pipe.
--
-- If the function succeeds, the return value is a handle to the server end of
-- a named pipe instance.
createNamedPipe
    :: FilePath
    -- ^ The unique pipe name. This string must have the following form:
    --
    -- @
    -- \\\\.\\pipe\\pipename
    -- @
    --
    -- The pipename part of the name can include any character other than a
    -- backslash, including numbers and special characters. The entire pipe
    -- name string can be up to 256 characters long. Pipe names are not case
    -- sensitive.
    -> PipeOpenMode
    -- ^ The open mode. The function fails if 'PipeOpenMode' specifies anything
    -- other than @0@ or if it contains invalid flag.
    -> PipeMode
    -- ^ The pipe mode. The function fails if 'PipeMode' specifies anything
    -- other than @0@ or if it contains invalid flag.
    -> MaxInstances
    -- ^ The maximum number of instances that can be created for this pipe. The
    -- first instance of the pipe can specify this value; the same number must
    -- be specified for other instances of the pipe. Acceptable values are in
    -- the range 1 through 'pIPE_UNLIMITED_INSTANCES' (255).
    --
    -- If this parameter is 'pIPE_UNLIMITED_INSTANCES', the number of pipe
    -- instances that can be created is limited only by the availability of
    -- system resources. If nMaxInstances is greater than
    -- 'pIPE_UNLIMITED_INSTANCES', the return value is 'iNVALID_HANDLE_VALUE'
    -- and GetLastError returns 'eRROR_INVALID_PARAMETER'.
    -> OutBufferSize
    -- ^ The number of bytes to reserve for the output buffer.
    -> InBufferSize
    -- ^ The number of bytes to reserve for the input buffer.
    -> DefaultTimeOut
    -- ^ The default time-out value, in milliseconds, if the @WaitNamedPipe@
    -- function specifies @NMPWAIT_USE_DEFAULT_WAIT.@ Each instance of a named
    -- pipe must specify the same value.
    --
    -- A value of zero will result in a default time-out of 50 milliseconds.
    -> Maybe LPSECURITY_ATTRIBUTES
    -- ^ A pointer to a 'LPSECURITY_ATTRIBUTES' structure that specifies a
    -- security descriptor for the new named pipe and determines whether child
    -- processes can inherit the returned handle. If 'LPSECURITY_ATTRIBUTES'
    -- value is @NULL@, the named pipe gets a default security descriptor and
    -- the handle cannot be inherited. The ACLs in the default security
    -- descriptor for a named pipe grant full control to the @LocalSystem@
    -- account, administrators, and the creator owner. They also grant read
    -- access to members of the Everyone group and the anonymous account.
    -> IO HANDLE
    -- ^ On success it returns handle to the server end of a named pipe
    -- instance.
createNamedPipe name mode pipeMode max outSize inSize defTimeOut secAttrs =
    withTString name
        $ failIf (== iNVALID_HANDLE_VALUE) errMsg . createNamedPipe'
  where
    createNamedPipe' n = c_CreateNamedPipe n mode pipeMode max outSize inSize
        defTimeOut (maybePtr secAttrs)

    errMsg = "CreateNamedPipe " <> show name

-- | FFI call to @CreateNamedPipe@ function.
--
-- Creates an instance of a named pipe and returns a handle for subsequent pipe
-- operations. A named pipe server process uses this function either to create
-- the first instance of a specific named pipe and establish its basic
-- attributes or to create a new instance of an existing named pipe.
--
-- <https://msdn.microsoft.com/en-us/library/windows/desktop/aa365150(v=vs.85).aspx MSDN: CreateNamedPipe function>
--
-- @
-- HANDLE WINAPI CreateNamedPipe(
--     _In_     LPCTSTR               lpName,
--     _In_     DWORD                 dwOpenMode,
--     _In_     DWORD                 dwPipeMode,
--     _In_     DWORD                 nMaxInstances,
--     _In_     DWORD                 nOutBufferSize,
--     _In_     DWORD                 nInBufferSize,
--     _In_     DWORD                 nDefaultTimeOut,
--     _In_opt_ LPSECURITY_ATTRIBUTES lpSecurityAttributes
-- );
-- @
foreign import WINDOWS_CCONV unsafe "windows.h CreateNamedPipeW"
    c_CreateNamedPipe
        :: LPCTSTR
        -> PipeOpenMode
        -> PipeMode
        -> MaxInstances
        -> OutBufferSize
        -> InBufferSize
        -> DefaultTimeOut
        -> LPSECURITY_ATTRIBUTES
        -> IO HANDLE

-- }}} createNamedPipe --------------------------------------------------------

-- {{{ connectNamedPipe -------------------------------------------------------

-- | Enables a named pipe server process to wait for a client process to
-- connect to an instance of a named pipe. A client process connects by calling
-- the 'System.Win32.File.createFile' function.
connectNamedPipe :: HANDLE -> IO Bool
connectNamedPipe h =
    successOrCheckIfAlreadyConnected $ c_ConnectNamedPipe h nullPtr
  where
    -- Based on following example from MSDN:
    --
    --   fConnected = ConnectNamedPipe(hPipe, NULL) ?
    --                TRUE : (GetLastError() == ERROR_PIPE_CONNECTED);
    --
    -- Seems that ConnectNamedPipe behaves as a wait, but it fails if there is
    -- a client already connected and that is the reason for the error check.
    successOrCheckIfAlreadyConnected action = action >>= \r -> if r
        then return r
        else (== #{const ERROR_PIPE_CONNECTED}) <$> getLastError

-- | FFI call to @ConnectNamedPipe@ function.
--
-- Enables a named pipe server process to wait for a client process to connect
-- to an instance of a named pipe. A client process connects by calling either
-- the @CreateFile@ or @CallNamedPipe@ function.
--
-- <https://msdn.microsoft.com/en-us/library/windows/desktop/aa365146(v=vs.85).aspx MSDN: ConnectNamedPipe function>
--
-- @
-- BOOL WINAPI ConnectNamedPipe(
--     _In_        HANDLE       hNamedPipe,
--     _Inout_opt_ LPOVERLAPPED lpOverlapped
-- );
-- @
--
-- Since this is basically a wait\/sleep function, we need to use *safe* and
-- *interruptible* foreign call, because they delay the GC sync.
--
-- For more inofrmation about InterruptibleFFI see GHC documentation, e.g.:
-- https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/ffi-chap.html#interruptible-foreign-calls
foreign import WINDOWS_CCONV interruptible "windows.h ConnectNamedPipe"
    c_ConnectNamedPipe
        :: HANDLE
        -> LPOVERLAPPED
        -> IO Bool

-- }}} connectNamedPipe -------------------------------------------------------

-- {{{ disconnectNamedPipe ----------------------------------------------------

-- | Disconnects the server end of a named pipe instance from a client process.
disconnectNamedPipe :: HANDLE -> IO ()
disconnectNamedPipe =
    failIfFalse_ "DisconnectNamedPipe" . c_DisconnectNamedPipe

-- | FFI call to @DisconnectNamedPipe@ function.
--
-- Disconnects the server end of a named pipe instance from a client process.
--
-- <https://msdn.microsoft.com/en-us/library/windows/desktop/aa365166(v=vs.85).aspx>
--
-- @
-- BOOL WINAPI DisconnectNamedPipe(
--     _In_ HANDLE hNamedPipe
-- );
-- @
foreign import WINDOWS_CCONV safe "windows.h DisconnectNamedPipe"
    c_DisconnectNamedPipe
        :: HANDLE
        -> IO Bool

-- }}} disconnectNamedPipe ----------------------------------------------------

-- {{{ waitNamedPipe ----------------------------------------------------------

-- | Waits until either a time-out interval elapses or an instance of the
-- specified named pipe is available for connection (that is, the pipe's
-- server process has a pending ConnectNamedPipe operation on the pipe).
--
-- Function returns 'Data.Bool.False' if an instance of the pipe is available
-- before the time-out interval elapses, and 'Data.Bool.True' otherwise.
-- (Do note that this logic differs from the @WaitNamedPipe@ WINAPI function.)
--
-- If no instances of the specified named pipe exist, the function returns
-- immediately, regardless of the time-out value.
waitNamedPipe
    :: FilePath
    -- ^ The name of the named pipe. The string must include the name of the
    -- computer on which the server process is executing. A period may be used
    -- for the servername if the pipe is local. The following pipe name format
    -- is used:
    --
    -- @
    -- \\servername\pipe\pipename
    -- @
    -> TimeOut
    -- ^ The number of milliseconds that the function will wait for an
    -- instance of the named pipe to be available. You can used one of the
    -- following values instead of specifying a number of milliseconds:
    -- 'nMPWAIT_USE_DEFAULT_WAIT', 'nMPWAIT_WAIT_FOREVER'.
    -> IO Bool
    -- ^ 'Data.Bool.True' if no instance of the pipe is available.
waitNamedPipe name timeOut =
    withTString name $ \n ->
        failUnlessTrueOr #{const ERROR_SEM_TIMEOUT} errMsg $
            c_WaitNamedPipe n timeOut
  where
    errMsg = "WaitNamedPipe " <> show name

-- | FFI call to @WaitNamedPipe@ function.
--
-- Waits until either a time-out interval elapses or an instance of the
-- specified named pipe is available for connection (that is, the pipe's
-- server process has a pending @ConnectNamedPipe@ operation on the pipe).
--
-- <https://msdn.microsoft.com/en-us/library/windows/desktop/aa365800(v=vs.85).aspx MSDN: WaitNamedPipe function>
--
-- @
-- BOOL WINAPI WaitNamedPipe(
--     _In_ LPCTSTR lpNamedPipeName,
--     _In_ DWORD   nTimeOut
-- );
-- @
--
-- Since this is basically a wait\/sleep function, we need to use *safe* and
-- *interruptible* foreign call, because they delay the GC sync.
foreign import WINDOWS_CCONV interruptible "windows.h WaitNamedPipeW"
    c_WaitNamedPipe
        :: LPCTSTR
        -> TimeOut
        -> IO Bool

-- }}} waitNamedPipe ----------------------------------------------------------

-- {{{ Utility Functions ------------------------------------------------------

-- | Closes an open object handle.
--
-- Function returns 'Data.Bool.False' when operation was successful, and
-- 'Data.Bool.True' when @ERROR_INVALID_HANDLE@ was returned by underlying
-- low-level call. This function enables us to ignore already closed handles.
closeHandleCheckInvalidHandle :: HANDLE -> IO Bool
closeHandleCheckInvalidHandle h =
    failUnlessTrueOrInvalidHandle "CloseHandle" (c_CloseHandle h)

-- | Flushes the buffers of a specified file and causes all buffered data to
-- be written to a file.
--
-- Function returns 'Data.Bool.False' when operation was successful, and
-- 'Data.Bool.True' when @ERROR_INVALID_HANDLE@ was returned by underlying
-- low-level call. This function enables us to ignore already closed handles.
flushFileBuffersCheckInvalidHandle :: HANDLE -> IO Bool
flushFileBuffersCheckInvalidHandle h =
    failUnlessTrueOrInvalidHandle "FlushFileBuffers" (c_FlushFileBuffers h)

-- | INTERNAL FUNCTION! DO NOT EXPORT!
--
-- Check that function either succeeded or if it failed due to
-- @ERROR_INVALID_HANDLE@. This way we can avoid issues with non-idempotent
-- disconnect\/close\/flush operations.
failUnlessTrueOrInvalidHandle :: String -> IO Bool -> IO Bool
failUnlessTrueOrInvalidHandle = failUnlessTrueOr #{const ERROR_INVALID_HANDLE}

-- | INTERNAL FUNCTION! DO NOT EXPORT!
--
-- Check that function either succeeded (returns 'Data.Bool.False') or if it
-- failed due to @err@ ('Data.Bool.True'). If it failed due to other error,
-- exception is thrown.
failUnlessTrueOr :: ErrCode -> String -> IO Bool -> IO Bool
failUnlessTrueOr err fnName f =
    failUnlessSuccessOr err fnName $ do
        r <- f
        if r
            then return #{const ERROR_SUCCESS}
            else getLastError

-- | Catch and handle @ERROR_PIPE_BUSY@ errors. Useful for wrapping
-- 'System.Win32.File.createFile' calls.
catchPipeBusy :: IO a -> (IOError -> IO a) -> IO a
catchPipeBusy action handler = action `catchIOError` handler'
  where
    handler' e = do
        isPipeBusy <- (== #{const ERROR_PIPE_BUSY}) <$> getLastError
        if isPipeBusy
            then handler e
            else ioError e

-- }}} Utility Functions ------------------------------------------------------
