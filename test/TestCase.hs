{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       TestCase
-- Description:  All test cases aggregated and exported as tests :: [Test].
-- Copyright:    (c) 2015-2016, Ixperta Solutions s.r.o.
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- All test cases aggregated and exported as @'tests' :: ['Test']@.
module TestCase (tests)
  where

import Test.Framework (Test, testGroup)

import qualified TestCase.Data.Streaming.NamedPipes
  as Data.Streaming.NamedPipes (tests)


tests :: [Test]
tests =
    [ testGroup "Data.Streaming.NamedPipes" Data.Streaming.NamedPipes.tests
    ]
