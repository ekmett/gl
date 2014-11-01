module Main
  ( main
  ) where

import Generator
import Parser
import Registry

main :: IO ()
main = do
  registry <- parseFile "gl.xml"
  generateSource (deshenaniganize registry)
