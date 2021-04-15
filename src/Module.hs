{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014 Edward Kmett and Gabríel Arthúr Pétursson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Module
  ( Module(..)
  , Export(..)
  , Body(..)
  , renderModule
  , saveModule
  ) where

import qualified Data.List as List
import System.Directory
import System.FilePath
import Text.Printf
import Utils

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
  | Pattern String (Maybe String) String
  | Code String
  deriving (Eq, Show)

renderModule :: Module -> String
renderModule m =
  printf
    ("-- This file was automatically generated.\n" ++
    "{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}\n" ++
    "module %s%s where\n\n%s")
     (moduleName m) (renderExports $ moduleExport m) (List.intercalate "\n\n" . map renderBody $ moduleBody m)


renderExports :: [Export] -> String
renderExports [] = ""
renderExports exports =
    printf " (\n%s)"
  . List.intercalate "\n"
  . zipWith renderExport (True : repeat False)
  $ filter nonEmpty exports
  where
    renderExport :: Bool -> Export -> String
    renderExport first (Section heading export) =
      printf "  -- * %s\n  %s %s"
        heading
        (if first then " " else ",")
        ((++"\n") . List.intercalate "\n  , " $ export)
    renderExport first (Subsection heading export) =
      printf "  -- ** %s\n  %s %s"
        heading
        (if first then " " else ",")
        ((++"\n") . List.intercalate "\n  , " $ export)
    nonEmpty :: Export -> Bool
    nonEmpty (Section _ []) = False
    nonEmpty (Subsection _ []) = False
    nonEmpty _ = True

renderBody :: Body -> String
renderBody body = case body of
  Import m -> List.intercalate "\n" $ map (printf "import %s") m
  Function name signature b -> printf "%s :: %s\n%s %s" name signature name b
  Pattern name (Just signature) b -> printf "pattern %s %s :: %s" name b signature
  Pattern name Nothing b -> printf "pattern %s %s" name b
  Code code -> code

saveModule :: FilePath -> Module -> IO ()
saveModule fp m = do
  createDirectoryIfMissing True folderPath
  writeFile filePath $ renderModule m
  where
    filePath = fp </> replace "." [pathSeparator] (moduleName m) <.> "hs"
    folderPath = List.intercalate [pathSeparator] . init $ splitOn [pathSeparator] filePath
