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

module Graphics.GL.Raw.Proc
  ( getProcAddress
  , getProcAddressWithSuffixes
  , Invoker, getExtensionEntry, FunPtr, unsafePerformIO
  , extensions
  ) where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Error
import Foreign.Ptr
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
  glGetString   <- ffienumIOPtrubyte (getProcAddress "glGetString")
  glGetStringi  <- ffienumuintIOPtrubyte (getProcAddress "glGetStringi")
  glGetIntegerv <- ffienumPtrintIOV (getProcAddress "glGetIntegerv")
  numExtensions <- alloca $ \p -> glGetIntegerv 0x821D p >> peek p
  supported <- forM [0..(fromIntegral numExtensions)-1] $ \n -> peekCString . castPtr =<< glGetStringi 0x1F03 n
  return $ Set.fromList supported
{-# NOINLINE extensions #-}

foreign import ccall "dynamic"
  ffienumIOPtrubyte :: FunPtr (GLenum -> IO (Ptr GLubyte)) -> GLenum -> IO (Ptr GLubyte)
 
foreign import ccall "dynamic"
  ffienumuintenumPtrintIOV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ()) -> GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ()

foreign import ccall "dynamic"
  ffienumPtrintIOV :: FunPtr (GLenum -> Ptr GLint -> IO ()) -> GLenum -> Ptr GLint -> IO ()
