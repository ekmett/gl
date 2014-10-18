module Main (
	main
) where

import Generator
import Parser
import Registry

main :: IO ()
main = do
	registry <- return . deshenaniganize =<< parseFile "gl.xml"

	generateSource registry

	return ()
