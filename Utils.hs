module Utils (joinOn, splitOn, strip, replace) where

import Data.Char (isSpace)
import Data.List (intersperse)
import Data.List.Split (splitOn)

joinOn :: [a] -> [[a]] -> [a]
joinOn x xs = intersperse x xs >>= id

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b = joinOn b . splitOn a
