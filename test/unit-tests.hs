{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       Main
-- Description:  Unit tests executor
-- Copyright:    (c) 2015-2016, Ixperta Solutions s.r.o.
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  portable
--
-- Unit tests executor.
module Main (main)
  where

import System.IO (IO)

import Test.Framework (defaultMain)

import TestCase (tests)


main :: IO ()
main = defaultMain tests
