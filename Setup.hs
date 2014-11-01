{-# OPTIONS_GHC -Wall #-}
import Distribution.Simple
import System.Directory (canonicalizePath)
import System.FilePath
import Generator (generateSource)
import Parser (parseFile)
import Registry (deshenaniganize)

-- ugly. There must be a safer way to get the absolute path of the dist dir
getDistDir :: IO FilePath
getDistDir = canonicalizePath "dist" 

generateAPI :: IO ()
generateAPI = do
  registry <- parseFile "gl.xml"
  dd <- getDistDir
  putStr "Generating API..."
  generateSource (dd </> "build" </> "autogen") (deshenaniganize registry)
  putStrLn "done"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \p l h f -> generateAPI >> buildHook simpleUserHooks p l h f
  , sDistHook = \p l h f -> generateAPI >> sDistHook simpleUserHooks p l h f
  }
