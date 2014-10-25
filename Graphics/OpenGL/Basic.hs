module Graphics.OpenGL.Basic (
	  module Graphics.OpenGL.Types

	, OpenGL
	, Scope
	, GLLoader

	-- OpenGL Initialization
	, initGL

	-- Convenience Functions
	, runGL
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Foreign.C.String
import Graphics.OpenGL.Internal.Scope
import Graphics.OpenGL.Types

-- | Load the OpenGL functions and query available extensions
--
-- A valid OpenGL context must be made current before calling this function.
-- The given function is used to load the OpenGL functions, which is typically
-- provided by your windowing utility.
--
-- The scope is _only_ valid for the context it was initialized in. Any
-- attempt to use it with other contexts is undefined behaviour.
initGL :: GLLoader -> IO Scope
initGL = initScope

-- | Run the given sequence of OpenGL commands with the given scope.
runGL :: MonadIO m => Scope -> OpenGL m a -> m a
runGL = flip runReaderT
