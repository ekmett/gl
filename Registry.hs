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
module Registry
  ( Registry(..)
  , Command(..)
  , Enumeratee(..)
  , Extension(..)
  , Feature(..)
  , Group(..)
  , Remove(..)
  , Require(..)
  , Type(..)
  , lookupCommand
  , lookupEnum
  , deshenaniganize
  ) where

import Data.Functor

data Registry = Registry
  { registryCommands :: [Command]
  , registryEnums :: [Enumeratee]
  , registryExtensions :: [Extension]
  , registryFeatures :: [Feature]
  , registryGroups :: [Group]
  } deriving (Eq, Show)

data Command = Command
  { commandName :: String
  , commandType :: Type
  , commandParameters :: [(Type, String)]
  } deriving (Eq, Show)

data Enumeratee = Enumeratee
  { enumName :: String
  , enumValue :: String
  } deriving (Eq, Show)

data Extension = Extension
  { extensionName :: String
  , extensionPlatform :: String
  , extensionRequires :: [Require]
  } deriving (Eq, Show)

data Feature = Feature
  { featureName :: String
  , featureRequires :: [Require]
  , featureRemoves :: [Remove]
  } deriving (Eq, Show)

data Group = Group
  { groupName :: String
  , groupEnum :: [String]
  } deriving (Eq, Show)

data Remove = Remove
  { removeProfile :: String
  , removeComment :: String
  , removeEnums :: [String]
  , removeCommands :: [String]
  } deriving (Eq, Show)

data Require = Require
  { requireEnums :: [String]
  , requireCommands :: [String]
  , requireComment :: String
  , requireProfile :: String
  } deriving (Eq, Show)

data Type = Type
  { typeName :: Maybe String
  , typePointer :: Bool
  } deriving (Eq, Show)

lookupCommand :: Registry -> String -> Command
lookupCommand registry command =
  head . filter ((== command) . commandName) $ registryCommands registry

lookupEnum :: Registry -> String -> Enumeratee
lookupEnum registry enum =
  head . filter ((== enum) . enumName) $ registryEnums registry

-- | Resolve shenanigans in the OpenGL Registry
deshenaniganize :: Registry -> Registry
deshenaniganize registry = registry
  { registryFeatures = clean44feature . clean45feature <$> registryFeatures registry
  }

clean44feature :: Feature -> Feature
clean44feature feature
  | featureName feature == "GL_VERSION_4_4" = feature { featureRequires = clean44require <$> featureRequires feature }
  | otherwise = feature

clean44require :: Require -> Require
clean44require require = require
  { requireEnums = filter (`notElem` removed) $
    requireEnums require
  } where
  removed =
    [ "GL_MAP_READ_BIT"
    , "GL_MAP_WRITE_BIT"
    , "GL_STENCIL_INDEX"
    , "GL_STENCIL_INDEX8"
    , "GL_TRANSFORM_FEEDBACK_BUFFER"
    , "GL_UNSIGNED_INT_10F_11F_11F_REV"
    ]

clean45feature :: Feature -> Feature
clean45feature feature 
  | featureName feature == "GL_VERSION_4_5" = feature { featureRequires = clean45require <$> featureRequires feature }
  | otherwise = feature

clean45require :: Require -> Require
clean45require require = require
  { requireEnums = filter (`notElem` removed) $
    requireEnums require
  } where
  removed =
    [ "GL_BACK"
    , "GL_LOWER_LEFT"
    , "GL_NONE"
    , "GL_NO_ERROR"
    , "GL_TEXTURE_BINDING_1D"
    , "GL_TEXTURE_BINDING_1D_ARRAY"
    , "GL_TEXTURE_BINDING_2D"
    , "GL_TEXTURE_BINDING_2D_ARRAY"
    , "GL_TEXTURE_BINDING_2D_MULTISAMPLE"
    , "GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY"
    , "GL_TEXTURE_BINDING_3D"
    , "GL_TEXTURE_BINDING_BUFFER"
    , "GL_TEXTURE_BINDING_CUBE_MAP"
    , "GL_TEXTURE_BINDING_CUBE_MAP_ARRAY"
    , "GL_TEXTURE_BINDING_RECTANGLE"
    , "GL_UPPER_LEFT"
    ]
