## Using cabal packages with HEAD GHC through head.hackage

head.hackage is infrastructure to test hackage packages during GHC development,
but it can also serve the purpose of compiling cabal projects using the 

You can find the full instructions and links to documentation [here](https://ghc.gitlab.haskell.org/head.hackage/), but the short version is to run
```
curl https://ghc.gitlab.haskell.org/head.hackage/cabal.project >> cabal.project.local
cabal update
```
and that's it.
Your cabal project will now use the patched packages from head.hackage before
trying the usual hackage.

## Installing development builds

You can [manually install](https://www.haskell.org/ghcup/guide/#installing-custom-bindists) a ghc bindist using ghcup!
```
ghcup install ghc -u 'file:///Users/romes/ghc-dev/fixes/_build/bindist/ghc-9.9.20230723-aarch64-apple-darwin.tar.xz' 9.9-linear-core-patch
```
If you're developing on GHC, or have checked out a branch, you can build a
binary distribution by instructing the build system (hadrian) to do so:
```
./hadrian/build -j --freeze1 --flavour=default+no_profiled_libs+omit_pragmas binary-dist
```

