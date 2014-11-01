module Main
  ( main
  ) where

import Gen.Generator
import Gen.Parser
import Gen.Registry

main :: IO ()
main = do
  registry <- parseFile "gl.xml"
  generateSource (deshenaniganize registry)
