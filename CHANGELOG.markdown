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
