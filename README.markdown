glgen
=====

[![Hackage](https://img.shields.io/hackage/v/gl.svg)](https://hackage.haskell.org/package/gl) [![Build Status](https://github.com/ekmett/gl/workflows/Haskell-CI/badge.svg)](https://github.com/ekmett/gl/actions?query=workflow%3AHaskell-CI)

The `gl` package supplies low level bindings to all of the OpenGL specification for Haskell.

This package, `glgen`, is used to build the `gl` package.

Usage
-----

1. Download the latest and greatest xml specification for OpenGL from Khronos.

2. Replace the copy in this directory.

3. `cabal new-run glgen`

4. `cabal new-build gl`



Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
