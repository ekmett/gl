{-# LANGUAGE ForeignFunctionInterface, CPP #-}
--------------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett and Gabríel Arthúr Pétursson 2014-2016, (c) Sven Panne 2013
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

module Graphics.GL.Internal.Proc
  ( getProcAddress
  , Invoker
  , extensions
  ) where

import Control.Monad
#if __GLASGOW_HASKELL__ < 710
import Data.Functor
#endif
import Data.Set as Set
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Internal.FFI
  ( ffienumIOPtrubyte
  , ffienumuintIOPtrubyte
  , ffienumPtrintIOV
  )
import System.IO.Unsafe

--------------------------------------------------------------------------------

-- | Retrieve an OpenGL extension entry by name. Returns 'nullFunPtr' when no
-- extension entry with the given name was found.
getProcAddress :: String -> IO (FunPtr a)
getProcAddress extensionEntry =
  withCString extensionEntry hs_gl_getProcAddress

foreign import ccall unsafe "hs_gl_getProcAddress"
  hs_gl_getProcAddress :: CString -> IO (FunPtr a)

--------------------------------------------------------------------------------

type Invoker a = FunPtr a -> a

extensions :: Set String
extensions = unsafePerformIO $ do
  -- glGetStringi is only present in OpenGL 3.0 and OpenGL ES 3.0, and newer.
  glGetStringiFunPtr <- getProcAddress "glGetStringi"
  if glGetStringiFunPtr == nullFunPtr then do
    glGetString <- ffienumIOPtrubyte <$> getProcAddress "glGetString"
    supported <- glGetString 0x1F03 >>= peekCString . castPtr
    return $ Set.fromList (words supported)
  else do
    let glGetStringi = ffienumuintIOPtrubyte glGetStringiFunPtr
    glGetIntegerv <- ffienumPtrintIOV <$> getProcAddress "glGetIntegerv"
    numExtensions <- alloca $ \p -> glGetIntegerv 0x821D p >> peek p
    supported <- forM [0..fromIntegral numExtensions-1] $ glGetStringi 0x1F03 >=> peekCString . castPtr
    return $ Set.fromList supported
{-# NOINLINE extensions #-}
