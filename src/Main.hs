{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014-2019 Edward Kmett and Gabríel Arthúr Pétursson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
--
-- Loaded by cabal to configure the project.
--
-- Local hooks are provided to generate the API on build, producing haddocks and to enable @cabal sdist@
----------------------------------------------------------------------------
import Control.Monad
import Data.Functor
import Generator (generateSource)
import Parser (parseFile)
import Registry (deshenaniganize)
import Prelude
import System.Directory

src :: FilePath
src = "gl/generated-src"

main :: IO ()
main = do
  registry <- parseFile "gl.xml"
  man <- lines <$> readFile "man.txt"
  extensions <- lines <$> readFile "extensions.txt"

  b <- doesDirectoryExist src
  when b $ do
    putStr "Deleting old source..."
    removeDirectoryRecursive src
    putStrLn "done"

  putStr "Generating API..."
  generateSource src (deshenaniganize registry) man [ (x,y) | [x,y] <- map words extensions ]
  putStrLn "done"
