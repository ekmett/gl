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
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Tuple
import System.FilePath 
import Text.Printf
import Module
import Parser
import Registry
import Utils

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
saneEnum = ("GL_"++) . joinOn "_" . tail . splitOn "_"

saneModule :: Name -> Name
saneModule "422Pixels" = "FourTwoTwoPixels"
saneModule x = x

sanePrefix :: Name -> Name
sanePrefix "3DFX" = "ThreeDFX"
sanePrefix x = x

wrap :: Maybe String -> String -> String
wrap (Just w) s 
  | any isSpace s = printf "%s (%s)" w s
  | otherwise = printf "%s %s" w s
wrap Nothing s = s

commandSignature :: Maybe Name -> Command -> Signature
commandSignature monad command =
  joinOn " -> " $
    (parameterSignature $ commandParameters command) ++
    [returnSignature $ commandType command]
  where
    parameterSignature :: [(Type, String)] -> [String]
    parameterSignature params = map (typeSignature . fst) params

    returnSignature :: Type -> String
    returnSignature t = wrap monad . wrap (ptr t) $
      case typeName t of
        Nothing -> "()"
        Just "GLvoid" -> "()"
        Just x -> x

    ptr :: Type -> Maybe String
    ptr t = "Ptr" <$ guard (typePointer t)

    typeSignature :: Type -> String
    typeSignature t = wrap (ptr t) $
      case typeName t of
        Nothing -> "()"
        Just "GLvoid" -> "()"
        Just "struct _cl_context" -> "()"
        Just "struct _cl_event" -> "()"
        Just x -> x

commonName :: Signature -> Name
commonName
  = joinOn ""
  . splitOn "GL"
  . joinOn ""
  . map (filter isAlphaNum)
  . map (replace "()" "V")
  . splitOn " -> "
  . ioish

dynamicName :: Signature -> Name
dynamicName xs = "dyn" ++ commonName xs

invokerName :: Signature -> Name
invokerName xs = "ffi" ++ commonName xs

extensionModuleName :: ExtensionName -> ModuleName
extensionModuleName name =
  printf "Graphics.GL.Raw.Extension.%s.%s"
    (sanePrefix prefix) (saneModule $ camelCase (joinOn "_" rest))
  where
    (gl:prefix:rest) = splitOn "_" name

    camelCase :: String -> String
    camelCase str = concat . map (\(x:xs) -> toUpper x : xs) $
      splitOn "_" str

profileModuleName :: String -> String -> (ModuleName, Maybe ModuleName)
profileModuleName feature profile =
  ( printf "Graphics.GL.Raw.Profile.%s" $ fst submodule
  , snd submodule >>= return . printf "Graphics.GL.Raw.Profile.%s"
  )
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

      ("GL_VERSION_ES_CM_1_0", "common") -> ("EmbeddedCommon10", Nothing)
      ("GL_VERSION_ES_CM_1_0", _) -> ("EmbeddedLite10", Nothing)

      ("GL_ES_VERSION_2_0", _) -> ("Embedded20", Nothing)
      ("GL_ES_VERSION_3_0", _) -> ("Embedded30", Nothing)
      ("GL_ES_VERSION_3_1", _) -> ("Embedded31", Nothing)

      a -> error $ show a

implicitPrelude :: String -> ([String], [Body])
implicitPrelude m = case m of
  "Graphics.GL.Raw.Profile.Compatibility32" -> mk [
      "Graphics.GL.Raw.Profile.Core32"
    ]
  "Graphics.GL.Raw.Profile.Compatibility33" -> mk [
      "Graphics.GL.Raw.Profile.Compatibility32"
    , "Graphics.GL.Raw.Profile.Core33"
    ]
  "Graphics.GL.Raw.Profile.Compatibility40" -> mk [
      "Graphics.GL.Raw.Profile.Compatibility32"
    , "Graphics.GL.Raw.Profile.Core40"
    ]
  "Graphics.GL.Raw.Profile.Compatibility41" -> mk [
      "Graphics.GL.Raw.Profile.Compatibility40"
    , "Graphics.GL.Raw.Profile.Core41"
    ]
  "Graphics.GL.Raw.Profile.Compatibility42" -> mk [
      "Graphics.GL.Raw.Profile.Compatibility41"
    , "Graphics.GL.Raw.Profile.Core42"
    ]
  "Graphics.GL.Raw.Profile.Compatibility43" -> mk [
      "Graphics.GL.Raw.Profile.Compatibility42"
    , "Graphics.GL.Raw.Profile.Core43"
    ]
  "Graphics.GL.Raw.Profile.Compatibility44" -> mk [
      "Graphics.GL.Raw.Profile.Compatibility43"
    , "Graphics.GL.Raw.Profile.Core44"
    ]
  "Graphics.GL.Raw.Profile.Compatibility45" -> mk [
      "Graphics.GL.Raw.Profile.Compatibility44"
    , "Graphics.GL.Raw.Profile.Core45"
    ]
  "Graphics.GL.Raw.Profile.Core33" -> mk [
      "Graphics.GL.Raw.Profile.Core32"
    ]
  "Graphics.GL.Raw.Profile.Core40" -> mk [
      "Graphics.GL.Raw.Profile.Core33"
    ]
  "Graphics.GL.Raw.Profile.Core41" -> mk [
      "Graphics.GL.Raw.Profile.Core40"
    ]
  "Graphics.GL.Raw.Profile.Core42" -> mk [
      "Graphics.GL.Raw.Profile.Core41"
    ]
  "Graphics.GL.Raw.Profile.Core43" -> mk [
      "Graphics.GL.Raw.Profile.Core42"
    ]
  "Graphics.GL.Raw.Profile.Core44" -> mk [
      "Graphics.GL.Raw.Profile.Core43"
    ]
  "Graphics.GL.Raw.Profile.Core45" -> mk [
      "Graphics.GL.Raw.Profile.Core44"
    ]
  "Graphics.GL.Raw.Profile.EmbeddedCommon10" -> mk [
      "Graphics.GL.Raw.Profile.EmbeddedLite10"
    ]
  "Graphics.GL.Raw.Profile.Embedded30" -> mk [
      "Graphics.GL.Raw.Profile.Embedded20"
    ]
  "Graphics.GL.Raw.Profile.Embedded31" -> mk [
      "Graphics.GL.Raw.Profile.Embedded30"
    ]
  "Graphics.GL.Raw.Profile.Standard11" -> mk [
      "Graphics.GL.Raw.Profile.Standard10"
    ]
  "Graphics.GL.Raw.Profile.Standard12" -> mk [
      "Graphics.GL.Raw.Profile.Standard11"
    ]
  "Graphics.GL.Raw.Profile.Standard13" -> mk [
      "Graphics.GL.Raw.Profile.Standard12"
    ]
  "Graphics.GL.Raw.Profile.Standard14" -> mk [
      "Graphics.GL.Raw.Profile.Standard13"
    ]
  "Graphics.GL.Raw.Profile.Standard15" -> mk [
      "Graphics.GL.Raw.Profile.Standard14"
    ]
  "Graphics.GL.Raw.Profile.Standard20" -> mk [
      "Graphics.GL.Raw.Profile.Standard15"
    ]
  "Graphics.GL.Raw.Profile.Standard21" -> mk [
      "Graphics.GL.Raw.Profile.Standard20"
    ]
  "Graphics.GL.Raw.Profile.Standard30" -> mk [
      "Graphics.GL.Raw.Profile.Standard21"
    ]
  "Graphics.GL.Raw.Profile.Standard31" -> mk [
      "Graphics.GL.Raw.Profile.Standard30"
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

      when (isPrefixOf "Graphics.GL.Raw.Profile.Standard" name) $
        requires "Graphics.GL.Raw.Profile.Core32" req

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
    modify $ Map.insert (printf "Graphics.GL.Raw.Profile.%s" profile) []

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
      , "Core32"
      , "Core33"
      , "Core40"
      , "Core41"
      , "Core42"
      , "Core43"
      , "Core44"
      , "Core45"
      , "EmbeddedCommon11"
      , "EmbeddedLite11"
      , "Embedded20"
      , "Embedded30"
      , "Embedded31"
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
  , funExtensions :: Map ModuleName ExtensionName -- module name to extension name
  } deriving (Eq, Show)

ioish = replace "m (" "IO (" . replace "m GL" "IO GL"

funMap :: Registry -> [(Bool, Entry, String)] -> FunMap
funMap registry entries = FunMap
  (Map.fromList [ (n, s) | (_, F n, s) <- entries ])
  (Map.fromList $ map ((extensionModuleName&&&id).extensionName) $ registryExtensions registry)

funMapSignature :: String -> FunMap -> String
funMapSignature i (FunMap m _) = Map.findWithDefault undefined i m

funBody :: FunMap -> Name -> Signature -> [Body]
funBody fm n v =
  [ Function n ("MonadIO m => " ++ v) $ strip $ printf "= %s %s" (invokerName v) np
  , Function np ("FunPtr (" ++ v' ++ ")") $ strip $ printf "= unsafePerformIO (getProcAddress %s)" (show n)
  , Code $ printf "{-# NOINLINE %s #-}" np
  ] where
  np = n ++ "FunPtr"
  v' = ioish v

mkFFI :: FunMap -> Module
mkFFI fm = Module "Graphics.GL.Raw.Internal.FFI" export body where
  export = [ Section "Invokers" (nub $ invokerName <$> Foldable.toList (funSignatures fm)) ]
  body = 
    [ Import
      [ "Control.Monad.IO.Class"
      , "Foreign.C.Types"
      , "Foreign.Ptr"
      , "Graphics.GL.Raw.Types"
      ]
    ] ++ nub (Foldable.concatMap invokers (funSignatures fm))

invokers :: Signature -> [Body]
invokers v =
  [ Code $ printf "foreign import ccall \"dynamic\" %s :: FunPtr (%s) -> %s" nd v' v'
  , Function ni (printf "MonadIO m => FunPtr (%s) -> %s" v' v) $
      printf "fp %s = liftIO (%s fp %s)" params nd params
  ] where
  numArgs = subtract 2 . length $ splitOn " -> " v
  params = joinOn " " $ map (\x -> "v" ++ show x) [0..numArgs]
  v' = ioish v
  nd = dynamicName v
  ni = invokerName v
  
mkShared :: FunMap -> [(Bool, Entry, String)] -> Module
mkShared fm entr = Module "Graphics.GL.Raw.Internal.Shared" [] body
  where
    imp =
      [ Import
        [ "Control.Monad.IO.Class"
        , "Foreign.Ptr"
        , "Graphics.GL.Raw.Types"
        , "Graphics.GL.Raw.Internal.FFI"
        , "Graphics.GL.Raw.Internal.Proc"
        , "System.IO.Unsafe"
        ]
      ]

    body = imp ++ (concat . map bodyF $ nub entr)
    bodyF (False, _, _) = []
    bodyF (_, E n, v) = [Pattern n "GLenum" ("= " ++ v)]
    bodyF (_, F n, v) = funBody fm n v

mkModule :: FunMap -> String -> [(Bool, Entry, String)] -> Module
mkModule fm m entr = Module m export body
  where
    entryName (E n) = "pattern " ++ n
    entryName (F n) = n

    (ie, ib) = implicitPrelude m
    hasUnshared = any (\(s, _, _) -> not s) entr
    hasUnsharedFunctions = any (\(s, e, _) -> not s && case e of F _ -> True; _ -> False) entr
    hasExt = Map.member m (funExtensions fm)

    export = case Map.lookup m (funExtensions fm) of
      Just en ->
        [ Section "Extension Support" $
          [ "gl_" ++ (joinOn "_" . tail $ splitOn "_" en)
          ]
        , Section en $ ie ++ map (\(s, e, _) -> entryName e) entr
        ]
      Nothing ->
        [ Section m $ ie ++ map (\(s, e, _) -> entryName e) entr
        ]

    needsTypes (True, _, _) = False
    needsTypes (_ , E _, _) = True
    needsTypes (_ , F _, t) = isInfixOf "GL" t

    body =
      [ Import $ sort $ concat 
        [ [ "Graphics.GL.Raw.Internal.Shared" | any (\(s, _, _) -> s) entr ]
        , [ "Graphics.GL.Raw.Types"           | any needsTypes entr ]
        , [ "Data.Set"                        | hasExt ]
        , [ "Graphics.GL.Raw.Internal.Proc"   | hasExt || hasUnsharedFunctions ]
        , guard hasUnsharedFunctions >> 
          [ "Control.Monad.IO.Class"
          , "Foreign.Ptr"
          , "Graphics.GL.Raw.Internal.FFI"
          , "System.IO.Unsafe"
          ]
        ]
      ] ++
      ib ++
      extCheck ++
      concatMap bodyF entr

    extCheck = case Map.lookup m (funExtensions fm) of
      Just en ->
        [ Function
          ("gl_" ++ (joinOn "_" . tail $ splitOn "_" en))
          "Bool"
          ("= member " ++ show en ++ " extensions")
        ]
      Nothing -> []

    bodyF (True, _, _) = []
    bodyF (_, E n, v) = [Pattern n "GLenum" ("= " ++ v)]
    bodyF (_, F n, v) = funBody fm n v

mkExtensionGather :: FunMap -> [Module]
mkExtensionGather fm = flip map extensionGroups $
  \x -> Module (printf "Graphics.GL.Raw.Extension.%s" $ sanePrefix x)
    [Section (printf "%s Extensions" x) $ map ("module "++) $ extInGroup x]
    [Import $ extInGroup x]
  where
  extInGroup grp
    = map fst
    . sort
    . filter (\x -> grp == (head . tail . splitOn "_" $ snd x))
    . Map.toList
    $ funExtensions fm

  extensionGroups
    = sort
    . nub
    . map (head . tail . splitOn "_" . snd)
    . Map.toList
    $ funExtensions fm

mkExtensionGroupGather :: [Module] -> Module
mkExtensionGroupGather ms = Module "Graphics.GL.Raw.Extension"
  [Section "Extensions" $ map (("module "++) . moduleName) ms]
  [Import $ map moduleName ms]

generateSource :: FilePath -> Registry -> IO ()
generateSource fp registry = do
  let s = execState (entries registry) Map.empty
  let m = execState (modules registry s) Map.empty
  let fm' = Foldable.concat m
  let fm = funMap registry fm'
  saveModule fp $ mkFFI fm
  saveModule fp $ mkShared fm fm'
  forM_ (Map.toList m) $ saveModule fp . uncurry (mkModule fm)
  let exts = mkExtensionGather fm
  forM_ exts $ saveModule fp
  saveModule fp $ mkExtensionGroupGather exts
