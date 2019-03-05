# cached

Cache values to disk.

The module `Data.Cached` lets you cache values to disk to avoid re-running
(potentially long) computations between consecutive executions of your
program. Cached values are recomputed only when needed, i.e. when other
cached values on which they depend change. Independent computations are
run in parallel. It offers convenient fonctions for caching to text files,
but caching and uncaching using arbitrary IO actions is also possible.

The module was motivated by writing scientific data flows, simulation
experiments or data science scripts. Those often involve long
computations and create "flows" where the output of some computation
are the inputs of others, until final results are produced (values,
figures, statistical tests, etc.).

See the module Data.Cached documentation:
- On hackage: <https://hackage.haskell.org/package/cached/docs/Data-Cached.html>
- In the source: [./src/Data/Cached.hs](./src/Data/Cached.hs)
