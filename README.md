Building
========

Get the latest OpenGL Registry.

	wget https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/api/gl.xml

Build and run the generator.

	cabal configure
	cabal build opengl-wrangler-gen
	./dist/build/opengl-wrangler-gen/opengl-wrangler-gen

Build and install the library.

	cabal build
	cabal install
