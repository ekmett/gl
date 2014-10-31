module Module (
	  Module(..)
	, Export(..)
	, Body(..)
	, renderModule
	, saveModule
) where

import Data.String.Utils
import System.Directory
import System.IO
import Text.Printf

data Module = Module {
              moduleName :: String
            , moduleExport :: [Export]
            , moduleBody :: [Body]
            } deriving (Eq, Show)

data Export = Section {
              sectionHeading :: String
            , sectionExport :: [String]
            }
            | Subsection {
              sectionHeading :: String
            , sectionExport :: [String]
            } deriving (Eq, Show)

data Body = Import [String]
          | Function String String String
          | Pattern String String String
          | Code String
          deriving (Eq, Show)

renderModule :: Module -> String
renderModule m =
	printf str
		(moduleName m)
		(renderExports $ moduleExport m)
		(join "\n\n" . map renderBody $ moduleBody m)
	where str =
		"-- This file was automatically generated.\n" ++
                "{-# LANGUAGE ScopedTypeVariables, PatternSynonyms #-}\n" ++
		"module %s%s where\n" ++
		"\n" ++
		"%s"

renderExports :: [Export] -> String
renderExports [] = ""
renderExports exports =
	  printf " (\n%s)"
	. join "\n" . map (uncurry renderExport)
	$ zip (True : repeat False) exports
	where
		renderExport :: Bool -> Export -> String
		renderExport first (Section heading export) =
			printf "\t-- * %s\n\t%s %s"
				heading
				(if first then " " else ",")
				((++"\n") . join "\n\t, " $ export)
		renderExport first (Subsection heading export) =
			printf "\t-- ** %s\n\t%s %s"
				heading
				(if first then " " else ",")
				((++"\n") . join "\n\t, " $ export)

renderBody :: Body -> String
renderBody body = case body of
	Import m -> join "\n" $ map (printf "import %s") m
	Function name signature body -> printf "%s :: %s\n%s %s"
		name signature name body
        Pattern name signature body -> printf "pattern %s %s :: %s"
                name body signature
	Code code -> code

saveModule :: Module -> IO ()
saveModule m = do
	createDirectoryIfMissing True $ folderPath
	writeFile filePath $ renderModule m
	where
		filePath = (++".hs") . replace "." "/" $ moduleName m
		folderPath = join "/" . init $ split "/" filePath
