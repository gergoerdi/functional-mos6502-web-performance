See [this page][blogpost] for a detailed description of the benchmark
and its results.

|   Implementation  | Generated code size (bytes) | Average time of 4142 instructions (ms) |
| ----------------- | --------------------------- | -------------------------------------- |
| JavaScript        |                      12,877 |   0.98                                 |
| ReasonML/ReScript	|                      27,252 |	  1.77                                 |
| Idris 2           |                      60,379 |	  6.38                                 |
| Clean             |                     225,283 |	 39.41                                 |
| PureScript        |                     151,536 |	137.03                                 |
| GHC/GHCJS*        |                  12,948,449 | 250.52                                 |
| GHC/Asterius      |                   1,448,826 |	346.73                                 |


\* The GHCJS implementation is not yet fully integrated into the
benchmark runner. 

[blogpost]: https://unsafeperform.io/blog/2022-07-02-a_small_benchmark_for_functional_languages_targeting_web_browsers/
