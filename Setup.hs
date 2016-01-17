{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014 Edward Kmett and Gabríel Arthúr Pétursson
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
import Data.Functor
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.FilePath
import Generator (generateSource)
import Parser (parseFile)
import Registry (deshenaniganize)
import Prelude

generateAPI :: LocalBuildInfo -> IO ()
generateAPI l = do
  registry <- parseFile "gl.xml"
  man <- lines <$> readFile "man.txt"
  extensions <- lines <$> readFile "extensions.txt"
  putStr "Generating API..."
  generateSource (buildDir l </> "autogen") (deshenaniganize registry) man [ (x,y) | [x,y] <- map words extensions ]
  putStrLn "done"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \p l h f -> generateAPI l >> buildHook simpleUserHooks p l h f
  , haddockHook = \p l h f -> generateAPI l >> haddockHook simpleUserHooks p l h f
  , sDistHook = \p ml h f -> case ml of
     Nothing -> fail "No local buildinfo available. configure first"
     Just l -> do
       let editlib lib = lib { libBuildInfo = editBuildInfo (libBuildInfo lib) }
           editBuildInfo bi = bi { hsSourceDirs = (buildDir l </> "autogen") : hsSourceDirs bi }
           p' = p { library = fmap editlib (library p) }
       generateAPI l >> sDistHook simpleUserHooks p' ml h f
  }
