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
module Utils (joinOn, splitOn, strip, replace) where

import Data.Char (isSpace)
import Data.List (intersperse)
import Data.List.Split (splitOn)

joinOn :: [a] -> [[a]] -> [a]
joinOn x = concat . intersperse x

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b = joinOn b . splitOn a
