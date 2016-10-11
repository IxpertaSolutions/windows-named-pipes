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
  where

import Prelude (error)

import Data.Bits ((.|.))
import Data.Bool (not, otherwise)
import Data.Eq (Eq((==)))
import Data.Function (($), (.), on)
import qualified Data.List as List (filter, null)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.Ord (Ord(compare))
import Data.String (IsString(fromString), String)
import GHC.Generics (Generic)
import System.IO (FilePath, IO)
import Text.Show (Show(showsPrec), showString)

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI (mk)
import System.Win32.Types (HANDLE)

import System.Win32.NamedPipes.Internal
    ( createNamedPipe
    , pIPE_ACCESS_DUPLEX
    , pIPE_READMODE_MESSAGE
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
    isValid = not . List.null $ List.filter (== '\\') str

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
    fromString s = case pipeName s of
        Just n -> n
        Nothing -> error
            "fromString: PipeName: Backslash ('\\') is an invalid character."

-- | Equality is case-insensitive.
instance Eq PipeName where
    (==) = onCiPipeName (==)

-- | Ordering is case-insensitive.
instance Ord PipeName where
    compare = onCiPipeName compare

-- }}} PipeName ---------------------------------------------------------------

-- {{{ PipePath ---------------------------------------------------------------

data PipePath
    = LocalPipe PipeName
    | RemotePath String PipeName
  deriving Generic

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
        RemotePath srv (PipeName n) -> (Just $ CI.mk srv, CI.mk n)
{-# INLINE onCiPipePath #-}

instance Show PipePath where
    showsPrec _ = showString . pipePathToFilePath

-- | Equality is case-insensitive.
instance Eq PipePath where
    (==) = onCiPipePath (==)

-- | Ordering is case-insensitive.
instance Ord PipePath where
    compare = onCiPipePath compare

-- }}} PipePath ---------------------------------------------------------------

-- | Convert 'PipePath' to a valid Windows 'FilePath' referencing a Named Pipe.
-- Such paths are in form:
--
-- @
-- \\\\%ServerName%\\pipe\\%PipeName%
-- @
pipePathToFilePath :: PipePath -> FilePath
pipePathToFilePath = \case
    LocalPipe n -> format "." n
    RemotePath srv n -> format srv n
  where
    format serverName (PipeName name) =
        "\\\\" <> serverName <> "\\pipe\\" <> name

bindPipe :: PipeName -> IO PipeHandle
bindPipe name =
    createNamedPipe strName openMode pipeMode maxInstances inBufSize outBufSize
        timeOut securityAttrs
  where
    strName = pipePathToFilePath $ LocalPipe name

    openMode = pIPE_ACCESS_DUPLEX
    pipeMode = pIPE_TYPE_MESSAGE .|. pIPE_READMODE_MESSAGE .|. pIPE_WAIT
    maxInstances = pIPE_UNLIMITED_INSTANCES

    inBufSize = bufSize
    outBufSize = bufSize
    bufSize = 4096 -- TODO: Find a solid value.

    timeOut = 0
    securityAttrs = Nothing

getPipe :: PipePath -> IO PipeHandle
getPipe = getPipe
