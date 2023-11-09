# Creating a macOS app with Haskell and Swift

This is an in-depth guide into developing a native macOS application using
Haskell with Swift and SwiftUI, covering the set-up required to ... and tackling
challenges such as converting complex data types

(Part 2 discusses iOS applications)

## Overview of Architecture

Make diagram


## Hello, Swift, its Haskell!

This is the `Hello, World!` section:
1. We'll setup a Haskell (foreign) library exporting a function `hs_double` that
    doubles an integer, using the C FFI
2. Setup a SwiftUI app that calls `hs_double`
3. Package the Haskell code into a shared library
4. Create a Swift Framework `HaskellFramework` to export the Haskell functions
   (imported from the stub C header files) that is linked against the Haskell
   shared library
5. Import `HaskellFramework` into the SwiftUI app, to bring `hs_double` into
   scope -- to then run the macOS app!

You can follow along and view each step as a series of individual commits from ... (link to repo)

### The Haskell library doubles the fun

We're jumping straight into a full-fledged Haskell projected managed with cabal,
where we define a shared library using the `foreign-library` stanza.
1. Start with a normal cabal file with a `library` stanza that exposes `MyLib`
2. Add the function `hs_double` to `MyLib` that operates on `CInt`s
    ```haskell
    module MyLib where
    import Foreign.C

    hs_double :: CInt -> CInt
    hs_double x = 2 * x
    ```
    The organization of the code here isn't terribly important. Perhaps in a
    real project you could want to, for instance, only use C types like `CInt`
    in the foreign library bits, or perhaps you'd rather declare the `foreign
    export` together with the function declaration (we'll instead include it in
    the foreign-library).
2. In the cabal file, add a `foreign-library` stanza with
    ```cabal
    foreign-library haskell-foreign-framework
        type: native-shared

        -- This should work on Mac, despite being undefined behaviour
        -- See https://www.hobson.space/posts/haskell-foreign-library/ (great read)
        options: standalone

        -- We copy the C stub headers to a folder in the root.
        -- If you have foreign-export declarations in the library
        -- be sure to add this flag there too (so all stubs get added
        -- to the `haskell-framework-include` folder)
        ghc-options: -stubdir=haskell-framework-include

        other-modules: MyForeignLib
        build-depends: base, haskell-framework
        hs-source-dirs: flib
    ```
    Unfortunately, `options: standalone` is only officially supported (and
    required) by Windows, even though it is exactly what we need, however,
    unofficially, a macOS distribution should be able to safely use this option
    -- for more information see this [write-up explaining why this option is
    undefined for macOS](https://www.hobson.space/posts/haskell-foreign-library/).

    In the future, this might work out of the box without being undefined
    behaviour, or the behaviour on mac may have changed s.t. this no longer
    works... but let's hope for the former.

    We also output the C stub header files to a directory in the project root.
    Do add this to `.gitignore`.

3. Create the file `flib/MyForeignLib.hs` that declares a `foreign export` of
   `hs_double` imported from `MyLib` and re-exports it:
    ```haskell
    module MyForeignLib (hs_double) where
    import Foreign.C
    import MyLib (hs_double)
    foreign export ccall hs_double :: CInt -> CInt
    ```
    If we don't re-export the function, ...
4. `cabal build` should now generate a `haskell-framework-include` folder with a
   `MyForeignLib_stub.h`, and a `libhaskell-foreign-framework.dylib` shared library
   somewhere under `dist-newstyle` (you can `find . -name libhaskell-foreign-framework.dylib` to find it)
5. We'll test linking against this library in a C main program to check whether it works as expected.
    Create `scripts/test-haskell-foreign-lib.sh` with a script that compiles a
    main function in C which calls `hs_double`. A few notes:
        - We need to pass the path to the built shared library (`$HS_FLIB_PATH`)
            to the compiler
        - We need to pass the path to the headers (`$HS_HEADERS_PATH`)
        - We hardcode the @rpath linker variable to point to the relative shared
            library path (just for testing purposes).
            I think we can ship the executable pointing relatively to the shared
            library, at which point doing this will be more sensible (since
            we'll be using relative paths within the installed app)
        - We need to call `hs_init` and `hs_exit` to init the runtime system
            (see the [relevant GHC user guide section](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html#using-the-ffi-with-ghc))
        - We need to compile the C library using `ghc`, as it will automatically
            include and link the rts headers and library. To use a C compiler
            we'd also need to find the rts headers and library of our Haskell
            installation.
    ```bash
    #!/usr/bin/env bash

    set -e

    if ! test -f "haskell-framework.cabal"; then
        echo "Run this script from the root of your project!"
        exit 1
    fi

    HS_FLIB_PATH=$(dirname $(find . -name libhaskell-foreign-framework.dylib))
    HS_HEADERS_PATH=haskell-framework-include

    echo "
    #include <stdio.h>
    #include <MyForeignLib_stub.h>
    #include <HsFFI.h>
    int main(void) {
        int argc = 0;
        char* argv[] = { NULL };
        char** argp = argv;
        hs_init(&argc, &argp);
        printf(\"%d\n\", hs_double(4));
        hs_exit();
        return 0;
    }
    " > conftestmain.c

    # We use `ghc` instead of `gcc` because otherwise we also need to provide the
    # include and lib path of the runtime system (Rts)
    ghc -no-hs-main -o conftest conftestmain.c \
        -lhaskell-foreign-framework \
        -I"$HS_HEADERS_PATH" \
        -L"$HS_FLIB_PATH" \
        -optl-Wl,-rpath,"$HS_FLIB_PATH"

    if [ 8 -eq $(./conftest) ]; then
        echo "Foreign library successfully called!"
    else
        echo "Bad bad foreign library!"
        exit 1
    fi

    rm -f conftest*
    ```
    You should get "Foreign library successfully called!"


