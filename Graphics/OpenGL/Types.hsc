module Graphics.OpenGL.Types (
	  module Foreign.Ptr

	-- * Types
	-- ** Function Types
	, GLDEBUGPROC
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

type GLbitfield = CUInt
type GLboolean = CUChar
type GLbyte = CSChar
type GLchar = CChar
type GLcharARB = CChar
type GLclampd = CDouble
type GLclampf = CFloat
type GLclampx = CInt
type GLdouble = CDouble
type GLeglImageOES = Ptr ()
type GLenum = CUInt
type GLfixed = GLint
type GLfloat = CFloat
type GLhalfNV = CUShort
type GLint = CInt
type GLint64 = Int64
type GLint64EXT = Int64
type GLintptr = CPtrdiff
type GLintptrARB = CPtrdiff
type GLshort = CShort
type GLsizei = CInt
type GLsizeiptr = CPtrdiff
type GLsizeiptrARB = CPtrdiff
type GLsync = Ptr ()
type GLubyte = CUChar
type GLuint = CUInt
type GLuint64 = Word64
type GLuint64EXT = Word64
type GLushort = CUShort
type GLvdpauSurfaceNV = GLintptr

#if __APPLE__
type GLhandleARB = Ptr ()
#else
type GLhandleARB = CUInt
#endif
