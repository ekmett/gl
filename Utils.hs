{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple string munging utilities
----------------------------------------------------------------------------
module Utils (splitOn, strip, replace) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b = intercalate b . splitOn a
