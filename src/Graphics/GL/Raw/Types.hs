{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014 Edward Kmett and Gabríel Arthúr Pétursson
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Graphics.GL.Raw.Types (
  -- * Types
  -- ** Function Types
    GLDEBUGPROC
  , GLDEBUGPROCAMD
  , GLDEBUGPROCARB
  , GLDEBUGPROCKHR

  , mkGLDEBUGPROC
  , mkGLDEBUGPROCAMD
  , mkGLDEBUGPROCARB
  , mkGLDEBUGPROCKHR

  -- ** Common Types
  , GLbitfield
  , GLboolean
  , GLbyte
  , GLchar
  , GLcharARB
  , GLclampd
  , GLclampf
  , GLclampx
  , GLdouble
  , GLeglImageOES
  , GLenum
  , GLfixed
  , GLfloat
  , GLhalfNV
  , GLhandleARB
  , GLint
  , GLint64
  , GLint64EXT
  , GLintptr
  , GLintptrARB
  , GLshort
  , GLsizei
  , GLsizeiptr
  , GLsizeiptrARB
  , GLsync
  , GLubyte
  , GLuint
  , GLuint64
  , GLuint64EXT
  , GLushort
  , GLvdpauSurfaceNV
) where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Numeric.Half

type GLDEBUGPROC =
  FunPtr (GLenum
       -> GLenum
       -> GLuint
       -> GLenum
       -> GLsizei
       -> Ptr GLchar
       -> Ptr ()
       -> IO ())

type GLDEBUGPROCAMD =
  FunPtr (GLuint
       -> GLenum
       -> GLenum
       -> GLsizei
       -> Ptr GLchar
       -> Ptr ()
       -> IO ())

type GLDEBUGPROCARB =
  FunPtr (GLenum
       -> GLenum
       -> GLuint
       -> GLenum
       -> GLsizei
       -> Ptr GLchar
       -> Ptr ()
       -> IO ())

type GLDEBUGPROCKHR =
  FunPtr (GLenum
       -> GLenum
       -> GLuint
       -> GLenum
       -> GLsizei
       -> Ptr GLchar
       -> Ptr ()
       -> IO ())

foreign import ccall "wrapper"
  mkGLDEBUGPROC :: (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> Ptr () -> IO ()) -> IO GLDEBUGPROC

foreign import ccall "wrapper"
  mkGLDEBUGPROCAMD :: (GLuint -> GLenum -> GLenum -> GLsizei -> Ptr GLchar -> Ptr () -> IO ()) -> IO GLDEBUGPROCAMD

foreign import ccall "wrapper"
  mkGLDEBUGPROCARB :: (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> Ptr () -> IO ()) -> IO GLDEBUGPROCARB

foreign import ccall "wrapper"
  mkGLDEBUGPROCKHR :: (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> Ptr () -> IO ()) -> IO GLDEBUGPROCKHR

type GLbitfield       = Word32
type GLboolean        = Word8
type GLbyte           = Int8
type GLchar           = Word8
type GLcharARB        = Word8
type GLclampd         = Double
type GLclampf         = Float
type GLclampx         = Int32
type GLdouble         = Double
type GLeglImageOES    = Ptr ()
type GLenum           = Word32
type GLfixed          = GLint
type GLfloat          = Float
type GLhalfNV         = Half
type GLint            = Int32
type GLint64          = Int64
type GLint64EXT       = Int64
type GLintptr         = CPtrdiff
type GLintptrARB      = CPtrdiff
type GLshort          = Int16
type GLsizei          = Int32
type GLsizeiptr       = CPtrdiff
type GLsizeiptrARB    = CPtrdiff
type GLsync           = Ptr ()
type GLubyte          = Word8
type GLuint           = Word32
type GLuint64         = Word64
type GLuint64EXT      = Word64
type GLushort         = Word16
type GLvdpauSurfaceNV = CPtrdiff

#if __APPLE__
type GLhandleARB = Ptr ()
#else
type GLhandleARB = Word32
#endif
