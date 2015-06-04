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
import Data.List (intercalate, isPrefixOf)

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b = intercalate b . splitOn a

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn xs ys0 = go [] ys0 where
  go acc ys
    | isPrefixOf xs ys = reverse acc : go [] (drop (length xs) ys)
    | otherwise = case ys of
      z:zs -> go (z:acc) zs
      [] -> [reverse acc]
