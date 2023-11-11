---

title: Creating a macOS app with Haskell and Swift

description: This is an in-depth guide into developing a native macOS application using
             Haskell with Swift and SwiftUI, covering the set-up required to ... and tackling
             challenges such as converting complex data types


tags: haskell, swift, interop, macos

---

This is an in-depth guide into developing a native macOS application using
Haskell with Swift and SwiftUI, covering the set-up required to ... and tackling
challenges such as converting complex data types

(Part 2 discusses iOS applications)

I'm using XCode 15 and GHC 9.8

TODO: Instead of describing where to place content relative to what exists
already, simply use diff files (though we need the syntax highlighting to work)

## Overview of Architecture

Make diagram


## Hello, Swift, its Haskell!

This is the `Hello, World!` section:

1. We'll setup a Haskell (foreign) library exporting a function `hs_factorial` that
    doubles an integer, using the C FFI
2. Setup a SwiftUI app that calls `hs_factorial`
3. Package the Haskell code into a shared library
4. Create a Swift module `HaskellFramework` to export the Haskell functions
   (imported from the stub C header files), and setup linking against the
   Haskell shared library
5. Import `HaskellFramework` into the SwiftUI app, to bring `hs_factorial` into
   scope -- to then run the macOS app!

You can follow along and view each step as a series of individual commits from ... (link to repo)

The directory structure should be something like

```md
SwiftHaskell (the XCode project)
| haskell-framework (which contains haskell-framework.cabal)
```

### Setting up the SwiftUI app

Let's set-up a simple XCode project using SwiftUI for the main interface.  Fire
up XCode and create a macOS Application, named `SwiftHaskell`, using SwiftUI,
excluding tests. Choose a Personal Team rather than None - you might have to
create a (free of charge) one.

There should have been created `SwiftHaskellApp.swift` and `ContentView.swift`.
Change `ContentView.swift` to instead display the result of calling
`hs_factorial(5)`, even though `hs_factorial` is not yet in scope. It should have:
```swift
import SwiftUI

struct ContentView: View {
    var body: some View {
        VStack {
            Text("Hello, Haskell: \(hs_factorial(5))!")
        }
        .padding()
    }
}
```

Before proceeding, create a New File > Configuration Settings File (also known
as a `.xcconfig` file) `BuildSettings.xcconfig`. We'll use this file to write
all our build settings textually instead of using XCode's build settings
navigator. To set this config as the default, go to the project tab, under `Info
> Configurations`, to the BuildSettings (note that for the configuration to show
up in XCode, the `.xcconfig` must be in the tree navigator (which happens by
default if you created the module within XCode). You can read more, or see
exactly how to set an `.xcconfig` file as the configuration, in this [write-up
on `xcconfig` by NSHipster](https://nshipster.com/xcconfig/)

### Setting up a Haskell foreign library

Create a folder `haskell-framework` within the XCode project, `cd` into it, and
follow from there.

We're jumping straight into a full-fledged Haskell projected managed with cabal,
where we define a shared library using the `foreign-library` stanza.

Start with a normal cabal file with a `library` stanza that exposes `MyLib`, and
add the function `hs_factorial` to `MyLib` that operates on `CInt`s:
```haskell
module MyLib where
import Foreign.C

hs_factorial :: CInt -> CInt
hs_factorial x = product [1..x]
```
The organization of the code here isn't terribly important. Perhaps in a
real project you could want to, for instance, only use C types like `CInt`
in the foreign library bits, or perhaps you'd rather declare the `foreign
export` together with the function declaration (we'll instead have it
by itself in the foreign-library, instead of moving everything there).

In the cabal file, add a `foreign-library` stanza with
```haskell
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
<!---->
In the future, this might work out of the box without being undefined
behaviour, or the behaviour on mac may have changed s.t. this no longer
works... but let's hope for the former.
<!---->
We also output the C stub header files to a directory in the project root.
Do add this to `.gitignore`.

Create the file `flib/MyForeignLib.hs` that declares a `foreign export` of
`hs_factorial` imported from `MyLib` and `foreign export`s it:
```haskell
module MyForeignLib where
import Foreign.C
import MyLib (hs_factorial)
foreign export ccall hs_factorial :: CInt -> CInt
```
It doesn't seem that re-exporting the function is enough for it to be
included in the shared library (might be a bug), we do need the `foreign
export` here.

`cabal build` should now generate a `haskell-framework-include` folder with a
`MyForeignLib_stub.h`, and a `libhaskell-foreign-framework.dylib` shared library
somewhere under `dist-newstyle` (you can `find . -name
libhaskell-foreign-framework.dylib` to find it)

We'll test linking against this library in a C main program to check whether it works as expected.
Create `scripts/test-haskell-foreign-lib.sh` with a script that compiles a
main function in C which calls `hs_factorial`. A few notes:
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
    hs_init(NULL, NULL);
    printf(\"%d\n\", hs_factorial(5));
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

RESULT=$(./conftest)

if [ 120 -eq $RESULT ]; then
    echo "Foreign library successfully called!"
else
    echo "Bad bad foreign library!"
    exit 1
fi

rm -f conftest*
```
You should get `Foreign library successfully called!`!

### Linking the Haskell library with the SwiftUI executable

Here's the recipe for invoking a foreign exported Haskell function in Swift:
1. Create a Swift module exporting Haskell functions through a module map
   pointing to the headers exporting the Haskell functions
2. Extend the module search path with the location of your new module map
3. Import that module as a module in the SwiftUI code, and use the desired function
4. At link time, the shared library with the symbols used by the program must be
   linked against, and must be found in the rpath which can be done by copying
   the shared library into the app bundled Frameworks

To define a Swift module where Haskell functions will live, we create a module
map listing all the headers exporting Haskell functions, using Clang's module
maps. A [clang module](https://clang.llvm.org/docs/Modules.html) looks something like
```haskell
module HaskellFramework {
    header "haskell-framework/haskell-framework-include/MyForeignLib.h"
    export *
}
```
which can then be imported into Swift code with `import HaskellFramework`, as
long as it is available as `module.modulemap` in the module search path.
Importing this module brings into scope all names exported from the listed header.

For our use case, we will use the [**inferred submodules**](https://clang.llvm.org/docs/Modules.html#submodule-declaration)
feature of modules.
With inferred submodules, we can simply define an **umbrella** list of headers,
and we get a submodule for each header in the directory (arbitrarily nested,
where a header `A/B/C.h` becomes a submodule named `MainModule.A.B.C`)

In the root of the XCode project, write a `module.modulemap` file with
```haskell
module HaskellFramework {
    umbrella "haskell-framework/haskell-framework-include"
    
    explicit module * {
        export *
    }
    
}
```
The `umbrella` keyword specifies the directory where to find the header files
for our submodules, and the `explicit module *` lines are the *inferred
submodule* part, as each header will result in a declaration roughly like
`explicit module HeaderName { header "umbrella/HeaderName.h" ... }`
In effect, our module above will expand to:
```haskell
module HaskellFramework {
    explicit module MyForeignLib {
        header "haskell-framework/haskell-framework-include/MyForeignLib.h"
        export *
    }
}
```
Again, to be clear, this is what our original `module.modulemap` using the
`umbrella` keyword currently expands to, **not the file we wrote**.

Having written our `module.modulemap`, we need to extend the compiler's module
search path to find this module. As we've also set-up our `xcconfig`-based
configuration, we can do this by simply writing into `BuildSettings.xcconfig`:
```bash
SWIFT_INCLUDE_PATHS=$(PROJECT_DIR)
```
This is equivalent to changing the "Swift Compiler - Search Paths > Import
Paths" build setting in XCode (in fact, you can inspect that setting on the
rightmost inspector panel to see that its `xcconfig` name is indeed
`SWIFT_INCLUDE_PATHS` -- this is also all explained in the [`xcconfig`
article](https://nshipster.com/xcconfig/)).

If you return to `ContentView.swift` where `hs_factorial` is being called, and
add at the top of the file
```swift
import HaskellFramework.MyForeignLib_stub
```
There is still a piece of the puzzle missing for our Haskell function call to be
accepted at compile time: our stub header includes `<HsFFI.h>`, which XCode will
not find when compiling. We need to extend our Header Search Path with the path
to the RTS headers. We can find where our RTS headers are by invoking in the
shell
```sh
ghc-pkg field rts include-dirs --simple-output
```
Currently, our `BuildSettings.xcconfig` can only contain statically known
information. Fortunately, we can `#include` other `xcconfig` files (that may
have been generated dynamically) in our `BuildSettings.xcconfig` (as described
by the [`xcconfig` write-up](https://nshipster.com/xcconfig/)).
We add the include directive in the `BuildSettings.xcconfig` file.
```c
#include "DynamicBuildSettings.xcconfig"
```
We will generate `DynamicBuildSettings.xcconfig` with a script
`haskell-framework/scripts/gen-dynamic-settings.sh` that calls the
above command to figure out the rts include path. We extend
`HEADER_SEARCH_PATHS`, where XCode will search for headers when building with the
rts includes (the literal string `$(inherit)` is `xcconfig` syntax for
inheriting the options set before applying the configuration):
```bash
#!/usr/bin/env bash

set -e
if ! test -f "haskell-framework/haskell-framework.cabal"; then
    echo "Run this script from the root of your XCode project!"
    exit 1
fi

echo "
HEADER_SEARCH_PATHS=\$(inherit) $(ghc-pkg field rts include-dirs --simple-output | tr ' ' '\n' | tail -n1)
" > DynamicBuildSettings.xcconfig

echo "Created DynamicBuildSettings.xcconfig!"
```
Do add `DynamicBuildSettings.xcconfig` to `.gitignore`. Note that asking for the
`include-dirs` of `rts` outputs two directories:
```txt
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/ffi
/Users/romes/.ghcup/ghc/9.8.1/lib/ghc-9.8.1/lib/../lib/aarch64-osx-ghc-9.8.1/rts-1.0.2/include
```
However, the `ffi` header is already included in a module by default in XCode
when building an application, so we cut it out (and only keep the `rts` folder)
so we do not get a `Redefinition of module 'FFI'` error (that's what the `tr`
and `tail -n1` together achieve).

You should now see the function is found by XCode as its definition is now in scope.
However, if you try building the program you'll encounter link time errors: as
one might reasonably expect, we also need to link the Haskell shared library -- we've only
told the Swift compiler where to find the definitions of the functions, but have
not actually provided the executable with the library where they are effectively.

The Haskell foreign library we've created in previous sections compiles to a
shared dynamic library. To link against it when building our Swift application
we need to pass `-lhaskell-foreign-framework` to the compilation toolchain. This
can be done in two (compatible as in both can co-exist) ways:
- Add a `link "haskell-foreign-framework"` declaration to the module map (explained [here](https://clang.llvm.org/docs/Modules.html#link-declaration))
    - There is a note about this feature not yet being widely supported in the
        reference page, however, it is sufficient to link the library on my
        XCode 15 distribution.
- Add the `-lhaskell-foreign-framework` flag to the `OTHER_LDFLAGS` build setting in `BuildSettings.xcconfig`
    - You can add this even if you've already specified the link directive
After adding the `link` directive, your `module.modulemap` should contain:
```haskell
module HaskellFramework {
    umbrella "haskell-framework/haskell-framework-include"
    
    explicit module * {
        export *
    }

    link "haskell-foreign-framework"
}
```

Lastly, we need to add the shared library path to the library search path and
copy it to the Frameworks folder that is bundled with the application. By
copying the library to this folder we ensure it can be found at runtime since
the run-path dependencies are searched for in the Frameworks folder
(`@executable_path/../Frameworks`).

In practice, we need extend the `LIBRARY_SEARCH_PATHS` setting dynamically and
to add a "Copy" Build Phase which copies the shared library to the listed
Frameworks folder. At this time, I do not know how to do this Copy outside of
XCode -- do shoot me a text if you know how. It is also unfortunate that we have
to hardcode the path to the dynamic library there, instead of computing it at build time.

Find the path to the foreign library by running haskell-foreign-framework` in the `haskell-framework` directory:
```txt
cabal list-bin haskell-foreign-library
```
Then, under the project target settings, add (by clicking in the little plus
sign) a `New Copy Files Phase` and, clicking in the plus sign of the new listing
of files to copy, add the path to the haskell-foreign-framework `.dylib` (the
shared library) by clicking on "Add Other".

To the `haskell-framework/scripts/gen-dynamic-settings.sh`, add the following
lines before echoing to the file
```bash
pushd . > /dev/null
cd haskell-framework
FLIB_PATH=$(cabal list-bin haskell-foreign-framework)
popd > /dev/null
```
and to what is written to `DynamicBuildSettings.xcconfig` add the following line
```bash
LIBRARY_SEARCH_PATHS=\$(inherit) $(dirname $FLIB_PATH)
```

In theory, copying the shared library to Frameworks works out mentioned reasons
(in the runtime run path search path), but I'll try to explain why it works:

> A run-path dependent library is a dependent library whose complete install
> name is not known when the library is created (see How Dynamic Libraries Are
> Used). Instead, the library specifies that the dynamic loader must resolve the
> library’s install name when it loads the executable that depends on the
> library.

In this section I have to explain the rpath, or link to the good resources, and
maybe show things with `otool -L`, etc...

At this point, you should be able to link the application successfully, and run
it.

### The RTS must be initialized

Surprise! Running the application will fail at runtime, when `hs_factorial` is
called. To call haskell functions from an executable written in other language,
one must first initialize the Haskell runtime system, and terminate it when
appropriate. We need to call the functions `hs_init` and `hs_end`, exposed in
`HsFFI.h`. We will write two wrapper functions in our foreign library to invoke
instead, as suggested in the [FFI chapter of the GHC user guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html#using-the-ffi-with-ghc).

We create a `cbits` folder in the `haskell-framework` Haskell project to put our
C files and headers, and add them to the `foreign-library` stanza of the cabal
file:
```haskell
include-dirs: cbits
c-sources: cbits/MyForeignLibRts.c
install-includes: MyForeignLibRts.h
```
You can see what these options do in [this cabal documentation section](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-includes).
We create the `cbits/MyForeignLibRts.c` wrapping the calls to `hs_init` and
`hs_end` as described by the document linked above:
```c
#include <stdlib.h>
#include <stdio.h>
#include <HsFFI.h>

HsBool flib_init() {

    printf("Initialising flib\n");

    // Initialise Haskell runtime
    hs_init(NULL, NULL);

    // Do other library initialisations here

    return HS_BOOL_TRUE;
}

void flib_end() {
    printf("Terminating flib\n");
    hs_exit();
}
```
It might seem that you could `foreign import` these functions into the Haskell
library and re-export them with `foreign export`, however, if they are exported
from Haskell, they themselves require the RTS to be initialised, effectively
defeating the purpose of being functions that initialise the RTS. Therefore, we
write a header file that we ship with the library for it to be included by the
Swift project. The file `cbits/MyForeignLibRts.h` contains:
```c
#include <HsFFI.h>

HsBool flib_init();
void flib_end();
```

Back to the Swift side, we need to augment our module map with a module mapping
to the RTS initialisation header. We add a second submodule declaration:
```diff
+    explicit module RTSManage {
+        header "haskell-framework/cbits/MyForeignLibRts.h"
+    }

    link "haskell-foreign-framework"
}
```
The symbols will be included in the foreign library.

Finally, in `SwiftHaskellApp.swift`, we extend the `@main` `App` by overriding
the `init()` function and calling `flib_init()`, and setting up an observer to
call `flib_end()` when the application terminates. We also need to import `HaskellFramework.RTSManage`
to bring the lib functions into scope:
```swift
init() {
    flib_init()

    NotificationCenter.default.addObserver(forName: NSApplication.willTerminateNotification, object: nil, queue: .main) { _ in
        // terminating
        flib_end()
    }
}
```

Running your application should work and proudly print `120` on the screen.
This is the end of part 1!
Next up is communicating more interesting data types, and making things more
ergonomic to use, while developing a simple app.

## References

- [Clang module](https://clang.llvm.org/docs/Modules.html)
- [swift-haskell-tutorial by nanotech](https://github.com/nanotech/swift-haskell-tutorial/tree/master)
- [Haskell foreign library and options: standalone](https://www.hobson.space/posts/haskell-foreign-library/)
- [xcconfig by NSHipster](https://nshipster.com/xcconfig/)
- [Using the FFI with GHC](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html#using-the-ffi-with-ghc)

