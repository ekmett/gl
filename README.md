Building
========

Get the latest OpenGL Registry.

	wget https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/api/gl.xml

Build and run the generator.

	cabal configure -fgen
	cabal build gl-gen
	./dist/build/gl-gen/gl-gen

Build and install the library.

	cabal build
	cabal install
