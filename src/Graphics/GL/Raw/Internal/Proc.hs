{-# LANGUAGE ForeignFunctionInterface, CPP #-}
--------------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2014, (c) Sven Panne 2013
-- License     :  BSD3
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module offers a portable way to retrieve OpenGL extension entries,
-- providing a portability layer upon platform-specific mechanisms like
-- @glXGetProcAddress@, @wglGetProcAddress@ or @NSAddressOfSymbol@.
--
-- This internal module offers convenience functions and re-exports for OpenGL
-- extension loading.
--
--------------------------------------------------------------------------------

module Graphics.GL.Raw.Internal.Proc
  ( getProcAddress
  , getProcAddressWithSuffixes
  , Invoker
  , getExtensionEntry
  , extensions
  ) where

import Control.Monad
import Data.Functor
import Data.Set as Set
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Error
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Raw.Internal.FFI (ffienumuintIOPtrubyte, ffienumPtrintIOV)
import System.IO.Unsafe

--------------------------------------------------------------------------------

-- | Retrieve an OpenGL extension entry by name. Returns 'nullFunPtr' when no
-- extension entry with the given name was found.
getProcAddress :: String -> IO (FunPtr a)
getProcAddress extensionEntry =
   withCString extensionEntry hs_gl_getProcAddress

foreign import ccall unsafe "hs_gl_getProcAddress"
   hs_gl_getProcAddress :: CString -> IO (FunPtr a)

-- | Retrieve an OpenGL extension entry by name, trying a list of name suffixes
-- in the given order. Returns 'nullFunPtr' when no extension entry with the
-- given name plus any of the suffixes was found.
getProcAddressWithSuffixes :: String -> [String] -> IO (FunPtr a)
getProcAddressWithSuffixes extensionEntry = foldM gpa nullFunPtr
   where gpa p s | p == nullFunPtr = getProcAddress (extensionEntry ++ s)
                 | otherwise       = return p

--------------------------------------------------------------------------------

type Invoker a = FunPtr a -> a

getExtensionEntry :: String -> String -> IO (FunPtr a)
getExtensionEntry extensionNameString extensionEntry =
   throwIfNullFunPtr ("unknown OpenGL extension entry " ++ extensionEntry ++
                      ", check for " ++ extensionNameString) $
      getProcAddressWithSuffixes extensionEntry extensionSuffixes

throwIfNullFunPtr :: String -> IO (FunPtr a) -> IO (FunPtr a)
throwIfNullFunPtr = throwIf (== nullFunPtr) . const

-- non-ARB extension suffixes are in descending order of number of extensions
extensionSuffixes :: [String]
extensionSuffixes = [
   "", "ARB", "EXT", "NV", "SGIX", "SGIS", "ATI", "APPLE", "SUN", "OES", "IBM",
   "MESA", "HP", "SGI", "OML", "AMD", "3DFX", "WIN", "PGI", "INTEL", "INGR",
   "GREMEDY", "SUNX", "S3", "REND", "MESAX" ]

extensions :: Set String
extensions = unsafePerformIO $ do
  glGetStringi  <- ffienumuintIOPtrubyte <$> getProcAddress "glGetStringi"
  glGetIntegerv <- ffienumPtrintIOV <$> getProcAddress "glGetIntegerv"
  numExtensions <- alloca $ \p -> glGetIntegerv 0x821D p >> peek p
  supported <- forM [0..fromIntegral numExtensions-1] $ glGetStringi 0x1F03 >=> peekCString . castPtr
  return $ Set.fromList supported
{-# NOINLINE extensions #-}
