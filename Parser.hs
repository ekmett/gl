{-# LANGUAGE CPP #-}
{-# LANGUAGE Arrows #-}
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
module Parser
  ( parseFile
  ) where

import Registry
import Text.XML.HXT.Core

#ifdef HLINT
{-# ANN module "HLint: ignore Reduce duplication" #-}
#endif

to :: ArrowXml a => String -> a XmlTree XmlTree
to name = hasName name <<< isElem <<< getChildren

perhaps :: ArrowIf a => a b c -> a b (Maybe c)
perhaps x = (arr Just <<< x) `orElse` constA Nothing

parseCommand :: ArrowXml a => a XmlTree Command
parseCommand = proc x -> do
  command <- to "command" -< x
  name <- getText <<< getChildren <<< to "name" <<< to "proto" -< command
  typ <- parseType <<< to "proto" -< command
  params <- listA parseParameter -< command
  veceq   <- perhaps (getAttrValue0 "name" <<< to "vecequiv") -< command
  alias   <- perhaps (getAttrValue0 "name" <<< to "alias") -< command
  returnA -< Command name typ params veceq alias

parseParameter :: ArrowXml a => a XmlTree Parameter
parseParameter = proc x -> do
  param <- to "param" -< x
  typ   <- parseType -< param
  name  <- getText <<< getChildren <<< to "name" -< param
  grp   <- perhaps (getAttrValue0 "group") -< param
  len   <- perhaps (getAttrValue0 "len") -< param
  returnA -< Parameter name typ grp len

parseEnum :: ArrowXml a => a XmlTree Enumeratee
parseEnum = proc x -> do
  enum <- to "enum" -< x
  name <- getAttrValue0 "name" -< enum
  value <- getAttrValue0 "value" -< enum
  returnA -< Enumeratee name value

parseExtension :: ArrowXml a => a XmlTree Extension
parseExtension = proc x -> do
  extension <- to "extension" -< x
  name <- getAttrValue0 "name" -< extension
  supported <- getAttrValue "supported" -< extension
  require <- listA $ parseRequire <<< to "require" -< extension
  returnA -< Extension name supported require

parseFeature :: ArrowXml a => a XmlTree Feature
parseFeature = proc x -> do
  name <- getAttrValue0 "name" -< x
  require <- listA $ parseRequire <<< to "require" -< x
  remove <- listA $ parseRemove <<< to "remove" -< x
  returnA -< Feature name require remove

parseGroup :: ArrowXml a => a XmlTree Group
parseGroup = proc x -> do
  group <- to "group" -< x
  name <- getAttrValue0 "name" -< group
  enum <- listA $ getAttrValue0 "name" <<< to "enum" -< group
  returnA -< Group name enum

parseRemove :: ArrowXml a => a XmlTree Remove
parseRemove = proc x -> do
  profile <- getAttrValue "profile" -< x
  comment <- getAttrValue "comment" -< x
  enums <- listA $ getAttrValue0 "name" <<< to "enum" -< x
  commands <- listA $ getAttrValue0 "name" <<< to "command" -< x
  returnA -< Remove profile comment enums commands

parseRequire :: ArrowXml a => a XmlTree Require
parseRequire = proc x -> do
  comment <- getAttrValue "comment" -< x
  enums <- listA $ getAttrValue0 "name" <<< to "enum" -< x
  commands <- listA $ getAttrValue0 "name" <<< to "command" -< x
  profile <- getAttrValue "profile" -< x
  returnA -< Require enums commands comment profile

parseType :: ArrowXml a => a XmlTree Type
parseType = proc x -> do
  ptype <- perhaps $ getText <<< getChildren <<< to "ptype" -< x
  pointer <- (getText <<< getChildren) >. elem '*' . concat -< x
  returnA -< Type
    { typeName = ptype
    , typePointer = fromEnum pointer
    }

parse :: IOSLA (XIOState ()) XmlTree Registry
parse = proc x -> do
  registry <- to "registry" -< x
  groups <- listA $ parseGroup <<< to "groups" -< registry
  enums <- listA $ parseEnum <<< to "enums" -< registry
  extensions <- listA $ parseExtension <<< to "extensions" -< registry
  commands <- listA $ parseCommand <<< to "commands" -< registry
  features <- listA $ parseFeature <<< to "feature" -< registry
  returnA -< Registry
    { registryCommands = commands
    , registryEnums = enums
    , registryExtensions = extensions
    , registryFeatures = features
    , registryGroups = groups
    }

parseFile :: String -> IO Registry
parseFile file = return . head =<< runX (readDocument [withRemoveWS yes] file >>> parse)
