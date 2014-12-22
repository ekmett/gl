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
  , Parameter(..)
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

data Parameter = Parameter
  { parameterName :: String
  , parameterType :: Type
  , parameterGroup :: Maybe String
  , parameterLen :: Maybe String
  } deriving (Eq, Show)

data Command = Command
  { commandName :: String
  , commandType :: Type
  , commandParameters :: [Parameter]
  , commandVectorEquivalent :: Maybe String
  , commandAlias :: Maybe String
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
  , typePointer :: Int
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
  { registryFeatures =
     cleanFeature "GL_VERSION_3_1" clean31require .
     cleanFeature "GL_VERSION_4_4" clean44require .
     cleanFeature "GL_VERSION_4_5" clean45require <$> registryFeatures registry
  , registryCommands = cleanCommand <$> registryCommands registry
  }

cleanCommand :: Command -> Command
cleanCommand cmd = cmd { commandParameters = cleanParameter <$> commandParameters cmd }

cleanParameter :: Parameter -> Parameter
cleanParameter param = param { parameterGroup = cleanParameterGroup <$> parameterGroup param }

cleanParameterGroup :: String -> String
cleanParameterGroup "PixelInternalFormat" = "InternalFormat"
cleanParameterGroup "SGIXFfdMask" = "FfdMaskSGIX"
cleanParameterGroup xs = xs

cleanFeature :: String -> (Require -> Require) -> Feature -> Feature
cleanFeature name f feature
  | featureName feature == name = feature { featureRequires = f <$> featureRequires feature }
  | otherwise = feature

clean31require :: Require -> Require
clean31require require = require
  { requireEnums = "GL_BLEND_COLOR" : requireEnums require
  }

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
