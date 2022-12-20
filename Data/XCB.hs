-- |
-- Module    :  Data.XCB
-- Copyright :  (c) Antoine Latter 2008
-- License   :  BSD3
--
-- Maintainer:  Antoine Latter <aslatter@gmail.com>
-- Stability :  provisional
-- Portability: portable
--
-- The 'Data.XCB' module can parse the contents of the xcb-proto
-- XML files into Haskell data structures.
--
-- Pretty-printers are provided to aid in the debugging - they do
-- not pretty-print to XML, but to a custom human-readable format.
module Data.XCB
    (module Data.XCB.Types
    ,module Data.XCB.FromXML
    ,module Data.XCB.Pretty
    ) where

import Data.XCB.Types
import Data.XCB.FromXML
import Data.XCB.Pretty
