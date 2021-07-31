{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014 Edward Kmett and Gabríel Arthúr Pétursson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Generator (generateSource) where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import qualified Data.Foldable as Foldable
import Data.Functor
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Printf
import Module
import Registry
import Utils
import Prelude

data Entry
  = F String
  | E String
  deriving (Eq, Ord, Show)

type Signature = String
type Name = String
type ModuleName = String
type SectionName = String
type ExtensionName = String
type ExportItem = String

data Category = C SectionName (Set ExportItem)
  deriving (Eq, Ord, Show)

saneEnum :: Name -> Name
saneEnum = ("GL_"++) . List.intercalate "_" . tail . splitOn "_"

saneModule :: Name -> Name
saneModule "422Pixels" = "FourTwoTwoPixels"
saneModule x = x

sanePrefix :: Name -> Name
sanePrefix "3DFX" = "ThreeDFX"
sanePrefix x = x

wrap :: Int -> Maybe String -> String -> String
wrap 0 _ s = s
wrap c w'@(Just w) s
  | any isSpace s = wrap (c-1) w' $ printf "%s (%s)" w s
  | otherwise = wrap (c-1) w' $ printf "%s %s" w s
wrap _ Nothing s = s

link :: Map Name Category -> Name -> String
link cs n = case Map.lookup n cs of
  Just (C _ ss) -> case compare (Set.size ss) 1 of
    GT -> "'Graphics.GL.Internal.Shared." ++ n ++ "'"
    EQ -> "'" ++ Set.findMin ss ++ "." ++ n ++ "'"
    LT -> "@" ++ n ++ "@"
  _ -> "@" ++ n ++ "@"

commandDescription :: Map String [String] -> Map Name Category -> Command -> Set String -> String
commandDescription fm cs (Command cmdName _cmdType cmdParameters vecEquiv alias) man = concat $
  [ "-- | Usage: @" ++ unwords (("'" ++ cmdName ++ "'") : map parameterName cmdParameters) ++ "@\n" ] ++
  [ case Map.lookup grp fm of
      Just xs -> printf "--\n-- The parameter @%s@ is a @%s@, one of: %s.\n"
         (parameterName param) grp $ List.intercalate ", " (map (link cs) xs)
      Nothing -> printf "--\n-- The parameter @%s@ is a @%s@.\n" (parameterName param) grp
  | param <- cmdParameters, Just grp <- [parameterGroup param]
  ] ++
  [ "--\n-- The length of @" ++ parameterName param ++ "@ should be " ++ describeLength x ++ ".\n"
  | param <- cmdParameters, Just x <- [parameterLen param]
  ] ++
  [ "--\n-- This command is an alias for " ++ link cs a ++ ".\n" | Just a <- [alias] ] ++
  [ "--\n-- The vector equivalent of this command is " ++ link cs v ++ ".\n" | Just v <- [vecEquiv] ] ++
  [ "--\n-- Manual page: <https://www.opengl.org/sdk/docs/man/html/" ++ cmdName ++ ".xhtml>\n" | Set.member cmdName man ]
  where
    describeLength x = "@" ++ x ++ "@"

commandSignature :: Maybe Name -> Command -> Signature
commandSignature monad command =
  List.intercalate " -> " $ parameterSignature (commandParameters command) ++ [returnSignature $ commandType command]
  where
    parameterSignature :: [Parameter] -> [String]
    parameterSignature = map (typeSignature . parameterType)

    returnSignature :: Type -> String
    returnSignature t = wrap 1 monad . wrap (typePointer t) (Just "Ptr") $
      case typeName t of
        Nothing -> "()"
        Just "GLvoid" -> "()"
        Just x -> x

    typeSignature :: Type -> String
    typeSignature t = wrap (typePointer t) (Just "Ptr") $
      case typeName t of
        Nothing -> "()"
        Just "GLvoid" -> "()"
        Just "struct _cl_context" -> "()"
        Just "struct _cl_event" -> "()"
        Just x -> x

commonName :: Signature -> Name
commonName
  = concat
  . splitOn "GL"
  . concatMap (filter isAlphaNum . replace "()" "V")
  . splitOn " -> "
  . ioish

dynamicName :: Signature -> Name
dynamicName xs = "dyn" ++ commonName xs

invokerName :: Signature -> Name
invokerName xs = "ffi" ++ commonName xs

extensionModuleName :: ExtensionName -> ModuleName
extensionModuleName name =
  printf "Graphics.GL.Ext.%s.%s"
    (sanePrefix prefix) (saneModule $ camelCase (List.intercalate "_" rest))
  where
    (prefix, rest) = case splitOn "_" name of
      (_:prefix':rest') -> (prefix', rest')
      _                 -> error $ "extensionModuleName: Unexpected name: " ++ name

    camelCase :: String -> String
    camelCase str = concatMap (\s -> case s of
                                       (x:xs) -> toUpper x : xs
                                       []      -> error "extensionModuleName.camelCase: Unexpected empty list")
                            $ splitOn "_" str

profileModuleName :: String -> String -> (ModuleName, Maybe ModuleName)
profileModuleName feature profile =
  (printf "Graphics.GL.%s" *** liftM (printf "Graphics.GL.%s")) submodule
  where
    submodule = case (feature, profile) of
      ("GL_VERSION_1_0", _) -> ("Standard10", Nothing)
      ("GL_VERSION_1_1", _) -> ("Standard11", Nothing)
      ("GL_VERSION_1_2", _) -> ("Standard12", Nothing)
      ("GL_VERSION_1_3", _) -> ("Standard13", Nothing)
      ("GL_VERSION_1_4", _) -> ("Standard14", Nothing)
      ("GL_VERSION_1_5", _) -> ("Standard15", Nothing)
      ("GL_VERSION_2_0", _) -> ("Standard20", Nothing)
      ("GL_VERSION_2_1", _) -> ("Standard21", Nothing)
      ("GL_VERSION_3_0", _) -> ("Standard30", Nothing)
      ("GL_VERSION_3_1", _) -> ("Standard31", Nothing)

      ("GL_VERSION_3_2", "core") -> ("Core32", Just "Compatibility32")
      ("GL_VERSION_3_2", "compatibility") -> ("Compatibility32", Nothing)
      ("GL_VERSION_3_2", _) -> ("Core32", Nothing)

      ("GL_VERSION_3_3", "core") -> ("Core33", Just "Compatibility33")
      ("GL_VERSION_3_3", "compatibility") -> ("Compatibility33", Nothing)
      ("GL_VERSION_3_3", _) -> ("Core33", Nothing)

      ("GL_VERSION_4_0", "core") -> ("Core40", Just "Compatibility40")
      ("GL_VERSION_4_0", "compatibility") -> ("Compatibility40", Nothing)
      ("GL_VERSION_4_0", _) -> ("Core40", Nothing)

      ("GL_VERSION_4_1", "core") -> ("Core41", Just "Compatibility41")
      ("GL_VERSION_4_1", "compatibility") -> ("Compatibility41", Nothing)
      ("GL_VERSION_4_1", _) -> ("Core41", Nothing)

      ("GL_VERSION_4_2", "core") -> ("Core42", Just "Compatibility42")
      ("GL_VERSION_4_2", "compatibility") -> ("Compatibility42", Nothing)
      ("GL_VERSION_4_2", _) -> ("Core42", Nothing)

      ("GL_VERSION_4_3", "core") -> ("Core43", Just "Compatibility43")
      ("GL_VERSION_4_3", "compatibility") -> ("Compatibility43", Nothing)
      ("GL_VERSION_4_3", _) -> ("Core43", Nothing)

      ("GL_VERSION_4_4", "core") -> ("Core44", Just "Compatibility44")
      ("GL_VERSION_4_4", "compatibility") -> ("Compatibility44", Nothing)
      ("GL_VERSION_4_4", _) -> ("Core44", Nothing)

      ("GL_VERSION_4_5", "core") -> ("Core45", Just "Compatibility45")
      ("GL_VERSION_4_5", "compatibility") -> ("Compatibility45", Nothing)
      ("GL_VERSION_4_5", _) -> ("Core45", Nothing)

      ("GL_VERSION_4_6", "core") -> ("Core46", Just "Compatibility46")
      ("GL_VERSION_4_6", "compatibility") -> ("Compatibility46", Nothing)
      ("GL_VERSION_4_6", _) -> ("Core46", Nothing)

      ("GL_VERSION_ES_CM_1_0", "common") -> ("EmbeddedCommon11", Nothing)
      ("GL_VERSION_ES_CM_1_0", _) -> ("EmbeddedLite11", Nothing)

      ("GL_ES_VERSION_2_0", _) -> ("Embedded20", Nothing)
      ("GL_ES_VERSION_3_0", _) -> ("Embedded30", Nothing)
      ("GL_ES_VERSION_3_1", _) -> ("Embedded31", Nothing)
      ("GL_ES_VERSION_3_2", _) -> ("Embedded32", Nothing)

      ("GL_SC_VERSION_2_0", _) -> ("SafetyCritical20", Nothing)

      a -> error $ show a

implicitPrelude :: String -> ([String], [Body])
implicitPrelude m = case m of
  "Graphics.GL.Compatibility32" -> mk [
      "Graphics.GL.Core32"
    ]
  "Graphics.GL.Compatibility33" -> mk [
      "Graphics.GL.Compatibility32"
    , "Graphics.GL.Core33"
    ]
  "Graphics.GL.Compatibility40" -> mk [
      "Graphics.GL.Compatibility33"
    , "Graphics.GL.Core40"
    ]
  "Graphics.GL.Compatibility41" -> mk [
      "Graphics.GL.Compatibility40"
    , "Graphics.GL.Core41"
    ]
  "Graphics.GL.Compatibility42" -> mk [
      "Graphics.GL.Compatibility41"
    , "Graphics.GL.Core42"
    ]
  "Graphics.GL.Compatibility43" -> mk [
      "Graphics.GL.Compatibility42"
    , "Graphics.GL.Core43"
    ]
  "Graphics.GL.Compatibility44" -> mk [
      "Graphics.GL.Compatibility43"
    , "Graphics.GL.Core44"
    ]
  "Graphics.GL.Compatibility45" -> mk [
      "Graphics.GL.Compatibility44"
    , "Graphics.GL.Core45"
    ]
  "Graphics.GL.Compatibility46" -> mk [
      "Graphics.GL.Compatibility45"
    , "Graphics.GL.Core46"
    ]
  "Graphics.GL.Core33" -> mk [
      "Graphics.GL.Core32"
    ]
  "Graphics.GL.Core40" -> mk [
      "Graphics.GL.Core33"
    ]
  "Graphics.GL.Core41" -> mk [
      "Graphics.GL.Core40"
    ]
  "Graphics.GL.Core42" -> mk [
      "Graphics.GL.Core41"
    ]
  "Graphics.GL.Core43" -> mk [
      "Graphics.GL.Core42"
    ]
  "Graphics.GL.Core44" -> mk [
      "Graphics.GL.Core43"
    ]
  "Graphics.GL.Core45" -> mk [
      "Graphics.GL.Core44"
    ]
  "Graphics.GL.Core46" -> mk [
      "Graphics.GL.Core45"
    ]
  "Graphics.GL.EmbeddedCommon11" -> mk [
      "Graphics.GL.EmbeddedLite11"
    ]
  "Graphics.GL.Embedded30" -> mk [
      "Graphics.GL.Embedded20"
    ]
  "Graphics.GL.Embedded31" -> mk [
      "Graphics.GL.Embedded30"
    ]
  "Graphics.GL.Embedded32" -> mk [
      "Graphics.GL.Embedded31"
    ]
  "Graphics.GL.Standard11" -> mk [
      "Graphics.GL.Standard10"
    ]
  "Graphics.GL.Standard12" -> mk [
      "Graphics.GL.Standard11"
    ]
  "Graphics.GL.Standard13" -> mk [
      "Graphics.GL.Standard12"
    ]
  "Graphics.GL.Standard14" -> mk [
      "Graphics.GL.Standard13"
    ]
  "Graphics.GL.Standard15" -> mk [
      "Graphics.GL.Standard14"
    ]
  "Graphics.GL.Standard20" -> mk [
      "Graphics.GL.Standard15"
    ]
  "Graphics.GL.Standard21" -> mk [
      "Graphics.GL.Standard20"
    ]
  "Graphics.GL.Standard30" -> mk [
      "Graphics.GL.Standard21"
    ]
  "Graphics.GL.Standard31" -> mk [
      "Graphics.GL.Standard30"
    ]
  "Graphics.GL.Ext.ANDROID.ExtensionPackEs31a" -> mk [
      "Graphics.GL.Ext.KHR.Debug"
    , "Graphics.GL.Ext.KHR.TextureCompressionAstcLdr"
    , "Graphics.GL.Ext.KHR.BlendEquationAdvanced"
    , "Graphics.GL.Ext.OES.SampleShading"
    , "Graphics.GL.Ext.OES.SampleVariables"
    , "Graphics.GL.Ext.OES.ShaderImageAtomic"
    , "Graphics.GL.Ext.OES.ShaderMultisampleInterpolation"
    , "Graphics.GL.Ext.OES.TextureStencil8"
    , "Graphics.GL.Ext.OES.TextureStorageMultisample2dArray"
    , "Graphics.GL.Ext.EXT.CopyImage"
    , "Graphics.GL.Ext.EXT.DrawBuffersIndexed"
    , "Graphics.GL.Ext.EXT.GeometryShader"
    , "Graphics.GL.Ext.EXT.GpuShader5"
    , "Graphics.GL.Ext.EXT.PrimitiveBoundingBox"
    , "Graphics.GL.Ext.EXT.ShaderIoBlocks"
    , "Graphics.GL.Ext.EXT.TessellationShader"
    , "Graphics.GL.Ext.EXT.TextureBorderClamp"
    , "Graphics.GL.Ext.EXT.TextureBuffer"
    , "Graphics.GL.Ext.EXT.TextureCubeMapArray"
    , "Graphics.GL.Ext.EXT.TextureSRGBDecode"
    ]
  _ -> ([], [])
  where
    mk names = (map ("module "++) names, [Import names])

requires :: String -> Require -> State (Map Entry Category) ()
requires name req = do
  forM_ (requireEnums req) $ \e ->
    modify $ Map.adjust (\(C v m) -> C v $ Set.insert name m) (E $ saneEnum e)

  forM_ (requireCommands req) $ \f ->
    modify $ Map.adjust (\(C v m) -> C v $ Set.insert name m) (F f)

entries :: Registry -> State (Map Entry Category) ()
entries registry = do
  forM_ (registryCommands registry) $ \f ->
    modify $ Map.insert
      (F $ commandName f)
      (C (commandSignature (Just "m") f) Set.empty)

  forM_ (registryEnums registry) $ \e ->
    modify $ Map.insert
      (E . saneEnum $ enumName e)
      (C (enumValue e) Set.empty)

  forM_ (registryExtensions registry) $ \ext ->
    forM_ (extensionRequires ext) $ \req ->
      requires (extensionModuleName $ extensionName ext) req

  forM_ (registryFeatures registry) $ \fe -> do
    let feature = featureName fe

    forM_ (featureRequires fe) $ \req -> do
      let name = fst . profileModuleName feature $ requireProfile req
      requires name req

      when ("Graphics.GL.Standard" `List.isPrefixOf` name) $
        requires "Graphics.GL.Core32" req

    forM_ (featureRemoves fe) $ \rm -> do
      let profile = removeProfile rm
      let (name, removeName) = profileModuleName feature profile

      forM_ (removeEnums rm) $ \e -> do
        modify $ Map.adjust
          (\(C v m) -> C v $ Set.delete name m)
          (E $ saneEnum e)

        case removeName of
          Just name' -> modify $ Map.adjust
            (\(C v m) -> C v $ Set.insert name' m) (E $ saneEnum e)
          Nothing -> return ()

      forM_ (removeCommands rm) $ \f -> do
        modify $ Map.adjust
          (\(C v m) -> C v $ Set.delete name m)
          (F f)

        case removeName of
          Just name' -> modify $ Map.adjust
            (\(C v m) -> C v $ Set.insert name' m) (F f)
          Nothing -> return ()

modules :: Registry
        -> Map Entry Category
        -> State (Map String [(Bool, Entry, String)]) ()
modules registry entr = do
  forM_ (registryExtensions registry) $ \ext ->
    modify $ Map.insert (extensionModuleName $ extensionName ext) []

  forM_ profiles $ \profile ->
    modify $ Map.insert (printf "Graphics.GL.%s" profile) []

  forM_ (Map.toList entr) $ \(k, C v ms) ->
    forM_ (Set.toList ms) $ \m ->
      modify $ Map.alter (f (Set.size ms > 1, k, v)) m
  where
    f r Nothing = Just [r]
    f r (Just a) = Just $ a ++ [r]

    profiles = [
        "Compatibility32"
      , "Compatibility33"
      , "Compatibility40"
      , "Compatibility41"
      , "Compatibility42"
      , "Compatibility43"
      , "Compatibility44"
      , "Compatibility45"
      , "Compatibility46"
      , "Core32"
      , "Core33"
      , "Core40"
      , "Core41"
      , "Core42"
      , "Core43"
      , "Core44"
      , "Core45"
      , "Core46"
      , "EmbeddedCommon11"
      , "EmbeddedLite11"
      , "Embedded20"
      , "Embedded30"
      , "Embedded31"
      , "Embedded32"
      , "SafetyCritical20"
      , "Standard10"
      , "Standard11"
      , "Standard12"
      , "Standard13"
      , "Standard14"
      , "Standard15"
      , "Standard20"
      , "Standard21"
      , "Standard30"
      , "Standard31"
      ]

data FunMap = FunMap
  { funSignatures   :: Map Name Signature           -- signatures by method name
  , funDescriptions :: Map Name String              -- descriptions by method name
  , funExtensions   :: Map ModuleName ExtensionName -- module name to extension name
  } deriving (Eq, Show)

ioish :: Signature -> Signature
ioish = replace "m (" "IO (" . replace "m GL" "IO GL"

funMap :: Registry -> Map Name Category -> [(Bool, Entry, String)] -> Map String [String] -> [String] -> FunMap
funMap registry cs es rgs man = FunMap
  (Map.fromList [ (n, s) | (_, F n, s) <- es ])
  (Map.fromList [ (commandName cmd, commandDescription rgs cs cmd manset) | cmd <- registryCommands registry ])
  (Map.fromList $ map ((extensionModuleName&&&id).extensionName) $ registryExtensions registry)
  where manset = Set.fromList man

funBody :: FunMap -> Name -> Signature -> [Body]
funBody fm n v =
  [ Code $ funDescriptions fm Map.! n
  , Function n ("MonadIO m => " ++ v) $ strip $ printf "= %s %s" (invokerName v) np
  , Function np ("FunPtr (" ++ v' ++ ")") $ strip $ printf "= unsafePerformIO (getProcAddress %s)" (show n)
  , Code $ printf "{-# NOINLINE %s #-}" np
  ] where
  np = n ++ "FunPtr"
  v' = ioish v

mkFFI :: FunMap -> Module
mkFFI fm = Module "Graphics.GL.Internal.FFI" export body where
  export = [ Section "Invokers" (List.nub $ invokerName <$> Foldable.toList (funSignatures fm)) ]
  body = Import
    [ "Control.Monad.IO.Class"
    , "Foreign.C.Types"
    , "Foreign.Ptr"
    , "Graphics.GL.Types"
    , "Numeric.Fixed"
    , "Numeric.Half"
    ] : List.nub (Foldable.concatMap invokers $ funSignatures fm)

invokers :: Signature -> [Body]
invokers v =
  [ Code $ printf "foreign import CALLCONV \"dynamic\" %s :: FunPtr (%s) -> %s" nd v' v'
  , Function ni (printf "MonadIO m => FunPtr (%s) -> %s" v' v) $
      printf "fp %s = liftIO (%s fp %s)" params nd params
  ] where
  parts = splitOn " -> " v
  numArgs = subtract 2 $ length parts
  params = unwords $ map (\x -> "v" ++ show x) [0..numArgs]
  v' = ioish v
  nd = dynamicName v
  ni = invokerName v

mkShared :: FunMap -> [(Bool, Entry, String)] -> Module
mkShared fm entr = Module "Graphics.GL.Internal.Shared" [] body
  where
    imp =
      [ Import
        [ "Control.Monad.IO.Class"
        , "Foreign.Ptr"
        , "Graphics.GL.Types"
        , "Graphics.GL.Internal.FFI"
        , "Graphics.GL.Internal.Proc"
        , "System.IO.Unsafe"
        ]
      ]

    body = imp ++ concatMap bodyF (List.nub entr)
    bodyF (False, _, _) = []
    bodyF (_, E n, v) = patSynBody n v
    bodyF (_, F n, v) = funBody fm n v

mkModule :: FunMap -> Map String String -> String -> [(Bool, Entry, String)] -> Module
mkModule fm re m entr = Module m export body
  where
    entryName (E n) = "pattern " ++ n
    entryName (F n) = n

    (ie, ib) = implicitPrelude m
    hasUnsharedFunctions = any (\(s, e, _) -> not s && case e of F _ -> True; _ -> False) entr
    hasExt = Map.member m (funExtensions fm)

    export = case Map.lookup m (funExtensions fm) of
      Just en ->
        [ Section "Extension Support"
          [ "gl_" ++ (List.intercalate "_" . tail $ splitOn "_" en)
          ]
        , Section en $ ie ++ map (\(_, e, _) -> entryName e) entr
        ]
      Nothing ->
        [ Section m $ ie ++ map (\(_, e, _) -> entryName e) entr
        ]

    needsTypes (True, _, _) = False
    needsTypes (_ , E _, _) = False
    needsTypes (_ , F _, t) = "GL" `List.isInfixOf` t

    body =
      [ Import $ List.sort $ concat
        [ [ "Graphics.GL.Internal.Shared" | any (\(s, _, _) -> s) entr ]
        , [ "Graphics.GL.Internal.Proc"   | hasExt || hasUnsharedFunctions ]
        , [ "Graphics.GL.Types"           | any needsTypes entr ]
        , [ "Data.Set"                        | hasExt ]
        , guard hasUnsharedFunctions >>
          [ "Control.Monad.IO.Class"
          , "Foreign.Ptr"
          , "Graphics.GL.Internal.FFI"
          , "System.IO.Unsafe"
          ]
        ]
      ] ++
      ib ++
      extCheck ++
      concatMap bodyF entr

    tryLink en = case Map.lookup en re of
      Just uri -> "<" ++ uri ++ " " ++ en ++ ">"
      Nothing -> en

    extCheck = case Map.lookup m (funExtensions fm) of
      Just en
        | parts@(_:_) <- tail (splitOn "_" en) ->
          [ Code $ "-- | Checks that the " ++ tryLink en ++ " extension is available."
          , Function ("gl_" ++ List.intercalate "_" parts) "Bool"
            (printf "= member %s extensions\n{-# NOINLINE %s #-}"
              (show en)
              ("gl_" ++ List.intercalate "_" parts))
          ]
        | otherwise -> error $ "malformed extension: " ++ en
      Nothing -> []

    bodyF (True, _, _) = []
    bodyF (_, E n, v) = patSynBody n v
    bodyF (_, F n, v) = funBody fm n v

mkExtensionGather :: FunMap -> [Module]
mkExtensionGather fm = flip map extensionGroups $
  \x -> Module (printf "Graphics.GL.Ext.%s" $ sanePrefix x)
    [Section (printf "%s Extensions" x) $ map ("module "++) $ extInGroup x]
    [Import $ extInGroup x]
  where
  extInGroup grp
    = map fst
    . List.sort
    . filter (\x -> grp == (head . tail . splitOn "_" $ snd x))
    . Map.toList
    $ funExtensions fm

  extensionGroups
    = List.sort
    . List.nub
    . map (head . tail . splitOn "_" . snd)
    . Map.toList
    $ funExtensions fm

mkExtensionGroupGather :: [Module] -> Module
mkExtensionGroupGather ms = Module "Graphics.GL.Ext"
  [Section "Extensions" $ map (("module "++) . moduleName) ms]
  [Import $ map moduleName ms]

generateSource :: FilePath -> Registry -> [String] -> [(String, String)] -> IO ()
generateSource fp registry man extensions = do
  let s = execState (entries registry) Map.empty
  let m = execState (modules registry s) Map.empty
  let re = Map.fromList extensions
  let fm' = Foldable.concat m
  let s' = Map.fromList $ first entryName <$> Map.toList s
  let rgs = Map.map List.sort
          . Foldable.foldl' (\a (g, n) -> Map.insertWith (++) g [n] a) Map.empty
          . concatMap (\(Enumeratee n _ gs) -> map (flip (,) n) gs) $ registryEnums registry
  let fm = funMap registry s' fm' rgs man
  saveModule fp $ mkFFI fm
  saveModule fp $ mkShared fm fm'
  forM_ (Map.toList m) $ \(k,v) -> saveModule fp $ mkModule fm re k v
  let exts = mkExtensionGather fm
  forM_ exts $ saveModule fp
  saveModule fp $ mkExtensionGroupGather exts
  where
    entryName :: Entry -> String
    entryName (F s) = s
    entryName (E s) = s

patSynBody :: String -> String -> [Body]
patSynBody n v =
  [
#if __GLASGOW_HASKELL__ >= 800
    Pattern n (Just "(Eq a, Num a) => a") "",
#endif
    Pattern n Nothing ("= " ++ v)
  ]
