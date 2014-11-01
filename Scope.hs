-- This file was automatically generated.
{-# LANGUAGE ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Raw.Extensions (
) where

import Control.Applicative
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Set as Set
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Raw.Types
import Graphics.GL.Raw.Proc
import Unsafe.Coerce

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
