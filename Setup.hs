import Data.List (unionBy)
import Distribution.Simple
import Distribution.Simple.BuildPaths (srcPref)
import Distribution.Simple.PreProcess (PPSuffixHandler, PreProcessor(..), knownSuffixHandlers)
import Distribution.Simple.SrcDist (sdist)
import System.Directory (canonicalizePath)
import System.FilePath (FilePath, (</>))
import Generator (generateSource)
import Parser (parseFile)
import Registry (deshenaniganize)

-- | Combine the preprocessors in the given hooks with the
-- preprocessors built into cabal.
allSuffixHandlers :: UserHooks -> [PPSuffixHandler]
allSuffixHandlers hooks
    = overridesPP (hookedPreProcessors hooks) knownSuffixHandlers
    where
      overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
      overridesPP = unionBy (\x y -> fst x == fst y)

-- ugly. There must be a safer way to get the absolute path of the dist dir
getDistDir :: IO FilePath
getDistDir = canonicalizePath "dist" 

generateAPI :: IO ()
generateAPI = do
  registry <- parseFile "gl.xml"
  dd <- getDistDir
  generateSource True (dd </> "build" </> "autogen") (deshenaniganize registry)

generateSDist :: IO ()
generateSDist = do
  registry <- parseFile "gl.xml"
  dd <- getDistDir
  generateSource False "sdist" (deshenaniganize registry)

autoHandler :: PPSuffixHandler
autoHandler = ("auto", \_ _ -> PreProcessor False $ \_ _ _ -> return () )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \p l h f -> generateAPI >> buildHook simpleUserHooks p l h f
  , sDistHook = \p l h f -> generateSDist >> sdist p l f srcPref (autoHandler : allSuffixHandlers h)
  }
