0.6
---
* Stopped exporting all extensions from `Graphics.GL`. You'll need to import `Graphics.GL.Extensions` as well.
* Added exports for `GLhalf` and `GLhalfARB`

0.5
---
* Haddock links to the OpenGL ES 2 registry
* `Compatibility40` depends on `Compatibility33`
* Shorter modules names to try to eke out a successful windows build.

0.4
---
* Haddocks!
* Fixed a major issue where pointers to pointers in the API were getting the wrong types.

0.3
---
* Switched to non-C style types for the most part. `Word32`, etc. have better understood support within the Haskell ecosystem. `CPtrdiff` remains as it varies across viable target platforms.
* Added a dependency on `Numeric.Fixed` from the `fixed` package for `GLfixed`.

0.2
---
* Support `Half` from the `half` package for `GLhalfNV`, so you can compute with the results.

0.1
---
* Initial release
