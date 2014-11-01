module Module
  ( Module(..)
  , Export(..)
  , Body(..)
  , renderModule
  , saveModule
  ) where

import Data.String.Utils
import System.Directory
import System.FilePath
import System.IO
import Text.Printf

data Module = Module
  { moduleName :: String
  , moduleExport :: [Export]
  , moduleBody :: [Body]
  } deriving (Eq, Show)

data Export
  = Section
    { sectionHeading :: String
    , sectionExport :: [String]
    }
  | Subsection
    { sectionHeading :: String
    , sectionExport :: [String]
    } deriving (Eq, Show)

data Body 
  = Import [String]
  | Function String String String
  | Pattern String String String
  | Code String
  deriving (Eq, Show)

renderModule :: Module -> String
renderModule m =
  printf 
    ("-- This file was automatically generated.\n" ++
    "{-# LANGUAGE ScopedTypeVariables, PatternSynonyms #-}\n" ++
    "module %s%s where\n\n%s")
     (moduleName m) (renderExports $ moduleExport m) (join "\n\n" . map renderBody $ moduleBody m)


renderExports :: [Export] -> String
renderExports [] = ""
renderExports exports =
    printf " (\n%s)"
  . join "\n" . map (uncurry renderExport)
  . zip (True : repeat False)
  $ filter nonEmpty exports
  where
    renderExport :: Bool -> Export -> String
    renderExport first (Section heading export) =
      printf "  -- * %s\n  %s %s"
        heading
        (if first then " " else ",")
        ((++"\n") . join "\n  , " $ export)
    renderExport first (Subsection heading export) =
      printf "  -- ** %s\n  %s %s"
        heading
        (if first then " " else ",")
        ((++"\n") . join "\n  , " $ export)
    nonEmpty :: Export -> Bool
    nonEmpty (Section _ []) = False
    nonEmpty (Subsection _ []) = False
    nonEmpty _ = True

renderBody :: Body -> String
renderBody body = case body of
  Import m -> join "\n" $ map (printf "import %s") m
  Function name signature body -> printf "%s :: %s\n%s %s" name signature name body
  Pattern name signature body -> printf "pattern %s %s :: %s" name body signature
  Code code -> code

saveModule :: Bool -> FilePath -> Module -> IO ()
saveModule r fp m = do
  createDirectoryIfMissing True folderPath
  writeFile filePath $ renderModule m
  where
    filePath = (fp </>) . (++ if r then ".hs" else ".auto") . replace "." [pathSeparator] $ moduleName m
    folderPath = (join [pathSeparator] . init $ split [pathSeparator] filePath)
