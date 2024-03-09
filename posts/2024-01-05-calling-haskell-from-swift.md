---

title: Calling Haskell from Swift

description: Crossing the language boundary between Haskell and Swift.
             This is the second part of an in-depth guide into developing native
             applications using Haskell with Swift.

tags: haskell, swift

---

This is the second installment of the in-depth series of blog-posts on
developing native macOS and iOS applications using both Haskell and
Swift/SwiftUI. This post covers how to call (non-trivial) Haskell functions from
Swift by using a foreign function calling-convention strategy similar to that
described by [Calling Purgatory from Heaven: Binding to Rust in
Haskell](https://well-typed.com/blog/2023/03/purgatory/) that requires argument
and result marshaling. Despite marshaling being required for robustly traversing
the foreign language boundary, I additionally explore calling Haskell from Swift
without any kind of marshaling by instead coercing the memory representation of
a Haskell value into a Swift one -- this is mostly a (very unsafe) curiosity,
but gives me an excuse to write a bit about low-level details in Haskell!

You may find the other blog posts in this series interesting:

1. [Creating a macOS app with Haskell and Swift](2023-11-10-creating-a-macos-app-with-haskell-and-swift.html)

The series of blog posts is further accompanied by a [github
repository](https://github.com/alt-romes/haskell-x-swift-project-steps) where
each commit matches a step of this tutorial. If in doubt regarding any step,
check the matching commit to make it clearer.
Additionally, I'm writing a build tool and libraries to facilitate the
interoperability between Haskell and Swift at [haskell-swift](https://github.com/alt-romes/haskell-swift).

This write-up has been cross-posted to [Well-Typed's Blog](https://well-typed.com/blog/).


# Introduction

We'll pick up from where the last post ended -- we have set up an XCode project
that includes our headers generated from Haskell modules with `foreign export`s
and links against the foreign library declared in the cabal file. We have
already been able to call a very simple Haskell function on integers from Swift
via Haskell's C foreign export feature and Swift's C interoperability.

This part concerns itself with calling idiomatic Haskell functions, which
typically involve user-defined datatypes as inputs and outputs, from Swift.
Moreover, these functions should be made available to Swift transparently, such
that Swift calls them as it does other idiomatic functions, with user defined
structs and classes.

For the running example, the following not-very-interesting function will
suffice to showcase the method we will use to expose this function from Haskell
to Swift, which easily scales to other complex data types and functions.
```haskell
data User
  = User { name :: String
         , age  :: Int
         }

birthday :: User -> User
birthday user = user{age = user.age + 1}
```
should be called from Swift as
```swift
struct User {
    let name: String
    let age: Int
}

// birthday(user: User(name: "Anton", age: 33)) = User(name: "Anton", age: 34)
func birthday(user: User) -> User {
    // Calls Haskell function...
}
```

To support this workflow, we need a way to **convert the User datatype from
Haskell to Swift**, and vice versa. There's more than one way to do it, but we
are going to **serialize (most) inputs and outputs** of a function -- though, in
the end, I promise to also dive a bit into coercing in-memory representations of
a datatype in between Haskell and Swift.

Note that, even though the serialization method here described seems complex, it
can be automated with Template Haskell and Swift Macros and packed into a neat
interface -- which I've done at [haskell-swift](https://github.com/alt-romes/haskell-swift).

As a preliminary step, we write the `User` data type and `birthday` function to
`haskell-framework/src/MyLib.hs`, and the Swift equivalents to
`SwiftHaskell/ContentView.swift` from [`haskell-x-swift-project-steps`](https://github.com/alt-romes/haskell-x-swift-project-steps).

# Marshaling Inputs and Outputs

Marshaling the inputs and outputs of a function, from the Swift perspective,
means to serialize the input values into strings, and receive the output value as
a string which is then decoded into a Swift value. The Haskell perspective is
dual.

Marshaling/serializing is a very robust solution to foreign language interoperability.
Despite the small overhead of encoding and decoding at a function call, it
almost automatically extends to, and enables, all sorts of data to be
transported across the language boundary, without it being vulnerable to
compiler implementation details and memory representation incompatibilities.

We will use the same marshaling strategy that [Calling Purgatory from Heaven: Binding to Rust in Haskell](https://well-typed.com/blog/2023/03/purgatory) does.
In short, the idiomatic Haskell function is wrapped by a low-level one which
deserializes the Haskell values from the argument buffers, and serializes the
function result to a buffer that the caller provides. More specifically,

* For each argument of the original function, we have a `Ptr CChar` and `Int` -- a string of characters and the size of that string (a.k.a `CStringLen`)
* For the result of the original function, we have two additional arguments, `Ptr CChar` and `Ptr Int` --
    an empty buffer in memory, and a pointer to the size of that buffer, both allocated by the caller.
* For each argument, we parse the C string into a Haskell value that serves as an argument to the original function.
* We call the original function
* We overwrite the memory location containing the original size of the buffer with the *required* size of the buffer to fit the result (which may be smaller or larger than the actual size).
    If the buffer is large enough we write the result to it.
* From the Swift side, we read the amount of bytes specified in the memory
    location that now contains the *required* size. If it turns out that the
    *required size* is larger than the buffer's size, we need to retry the
    function call with a larger buffer.

  - This means we might end up doing the work twice, if the original buffer size
    is not big enough. Some engineering work might allow us to re-use the
    result, but we'll stick with retrying from scratch for simplicity.

We will use `JSON` as the serialization format: both Haskell and Swift have good
support for automatically deriving JSON instances and it is easier to guarantee
JSON instances match than binary order-and-alignment-dependent serialization
instances.

## Haskell's Perspective

Extending the `User` example requires `User` to be decodable, which can be done automatically by adding to the `User` declaration:

```haskell
deriving stock Generic
deriving anyclass (ToJSON, FromJSON)
```

With the appropriate extensions and importing the necessary modules in `MyLib`:

```haskell
{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}

-- ...

import GHC.Generics
import Data.Aeson
```

The `MyForeignLib` module additionally must import

```haskell
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Data.Aeson
import Data.ByteString
import Data.ByteString.Unsafe
```

Now, let's (foreign) export a function `c_birthday` that wraps
`birthday` above in `haskell-framework/flib/MyForeignLib.hs`, using the
described method:

First, the type definition of the function receives the buffer with the `User` argument, and a
buffer to write the `User` result to. We cannot use tuples because they are not
supported in foreign export declarations, but the intuition is that the first
two arguments represent the original `User` input, and the two latter arguments
represent the returned `User`.

```haskell
c_birthday :: Ptr CChar -> Int -> Ptr CChar -> Ptr Int -> IO ()
```

Then, the implementation -- decode the argument, encode the result, write
result size to the given memory location and the result itself to the buffer, if
it fits.

```haskell
c_birthday cstr clen result size_ptr = do
```

We transform the `(Ptr CChar, Int)` pair into a `ByteString` using
`unsafePackCStringLen`, and decode a `User` from the `ByteString` using
`decodeStrict`:
```haskell
  -- (1) Decode C string
  Just user <- decodeStrict <$> unsafePackCStringLen (cstr, clen)
```

We apply the original `birthday` function to the decoded `user`. In our example,
this is a very boring function, but in reality this is likely a complex
idiomatic Haskell function that we want to expose to.
```haskell
  -- (2) Apply `birthday`
  let user_new = birthday user
```

We encode the `new_user :: User` as a `ByteString`, and use
`unsafeUseAsCStringLen` to get a pointer to the bytestring data and its length.
Finally, we get the size of the result buffer, write the actual size of the
result to the given memory location, and, if the actual size fits the buffer,
copy the bytes from the bytestring to the given buffer.
```haskell
  -- (3) Encode result
  unsafeUseAsCStringLen (toStrict $ encode user_new) $ \(ptr,len) -> do

    -- (3.2) What is the size of the result buffer?
    size_avail <- peek size_ptr

    -- (3.3) Write actual size to the int ptr.
    poke size_ptr len

    -- (3.4) If sufficient, we copy the result bytes to the given result buffer
    if size_avail < len
       then do
         -- We need @len@ bytes available
         -- The caller has to retry
         return ()
       else do
         moveBytes result ptr len
```
If the written *required* size is larger than the given buffer, the caller will
retry.

Of course, we must export this as a C function.
```haskell
foreign export ccall c_birthday :: Ptr CChar -> Int -> Ptr CChar -> Ptr Int -> IO ()
```
This makes the `c_birthday` function wrapper available to Swift in the generated
header and at link time in the dynamic library.

## Swift's Perspective

In Swift, we want to be able to call the functions exposed from Haskell via
their wrappers, from a wrapper that feels native to Swift itself. In our
example, that means wrapping a call to `c_birthday` in a new Swift `birthday`
function.

In `ContentView.swift`, we make `User` JSON-encodable/decodable by conforming to
the `Codable` protocol:
```swift
struct User: Codable {
    // ...
}
```

Then, we implement the Swift side of `birthday` which *simply* calls
`c_birthday` -- the whole logic of `birthday` is handled by the Haskell side
function (recall that `birthday` could be incredibly complex, and other
functions exposed by Haskell will indeed be).
```swift
func birthday(user: User) -> User {
    // ...
}
```

Note: in the implementation, a couple of blocks have to be wrapped with a `do {
... } catch X { ... }` but I omit them in this text. You can see the commit
relevant to the Swift function wrapper implementation in the repo with all of
these details included.

First, we encode the Swift argument into a binary representation (`Data`) (plus its length) that will serve
as arguments to the foreign C function.
```swift
let enc = JSONEncoder()
let dec = JSONDecoder()

var data: Data = try enc.encode(user)
let data_len = Int64(data.count)
```

However, a Swift `Data` value, which represents binary data, cannot be passed
directly to C as a pointer. For that, we must use `withUnsafeMutableBytes` to
get an `UnsafeMutableRawBufferPointer` out of the `Data` -- that we can pass to
the C foreign function. `withUnsafeMutableBytes` receives a closure that uses an
`UnsafeMutableRawBufferPointer` in its scope and returns the value returned by
the closure. Therefore we can return the result of calling it on the user `Data`
we encoded right away:

```swift
return data.withUnsafeMutableBytes { (rawPtr: UnsafeMutableRawBufferPointer) in
    // here goes the closure that can use the raw pointer,
    // the code for which we describe below
}
```

We allocate a buffer for the C foreign function to insert the result of
calling the Haskell function, and also allocate memory to store the size of the
buffer. We use `withUnsafeTemporaryAllocation` to allocate a buffer that can be
used in the C foreign function call. As for `withUnsafeMutableBytes`, this
function also takes a closure and returns the value returned by the closure:
```swift
// The data buffer size
let buf_size = 1024000 // 1024KB

// A size=1 buffer to store the length of the result buffer
return withUnsafeTemporaryAllocation(of: Int.self: 1) { size_ptr in
    // Store the buffer size in this memory location
    size_ptr.baseAddress?.pointee = buf_size

    // Allocate the buffer for the result (we need to wrap this in a do { ...} catch for reasons explained below)
    do {
        return withUnsafeTemporaryAllocation(byteCount: buf_size, alignment:1) { res_ptr in

            // Continues from here ...
        }
    } catch // We continue here in due time ...
}
```

We are now nested deep within 3 closures: one binds the pointer to the
argument's data, the other the pointer to the buffer size, and the other the
result buffer pointer. This means we can now call the C foreign function
wrapping the Haskell function:
```swift
c_birthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)
```

Recalling that the Haskell side will update the size pointed to by `size_ptr` to
the size required to serialize the encoded result, we need to check if
this required size exceeds the buffer we allocated, or read the data otherwise:

```swift
if let required_size = size_ptr.baseAddress?.pointee {
    if required_size > buf_size {
        // Need to try again
        throw HsFFIError.requiredSizeIs(required_size)
    }
}

return dec.decode(User.self, from: Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none))
```

where `HsFFIError` is a custom error defined as
```swift
enum HsFFIError: Error {
    case requiredSizeIs(Int)
}
```
We must now fill in the `catch` block to retry the foreign function call with a
buffer of the right size:
```swift
} catch HsFFIError.requiredSizeIs(let required_size) {
    return withUnsafeTemporaryAllocation(byteCount: required_size, alignment:1)
    { res_ptr in
        size_ptr.baseAddress?.pointee = required_size
        c_birthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)

        return dec.decode(User.self, from: Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none))
    }
}
```

That seems like a lot of work to call a function from Haskell! However, despite
this being a lot of code, not a whole lot is happening: we simply serialize the
argument, allocate a buffer for the result, and deserialize the result into it.
In the worst case, if the serialized result does not fit (the serialized data
has over 100 thousand characters), then we *naively* compute the function a
second time (it would not be terribly complicated to avoid this work by caching
the result and somehow resuming the serialization with the new buffer).
Furthermore, there is a lot of bureocracy in getting the raw pointers to send
off to Haskell land -- the good news is that all of this can be automated away
behind automatic code generation with Template Haskell and Swift Macros.

<details>
<summary>Expand for the complete function</summary>

```swift
func birthday (user : User) -> User {
    let enc = JSONEncoder()
    let dec = JSONDecoder()
    do {
        var data : Data = try enc.encode(user)
        let data_len = Int64(data.count)
        return try data.withUnsafeMutableBytes { (rawPtr:UnsafeMutableRawBufferPointer) in

            // Allocate buffer for result
            let buf_size = 1024000

            return try withUnsafeTemporaryAllocation(of: Int.self, capacity: 1) { size_ptr in
                size_ptr.baseAddress?.pointee = buf_size

                do {
                    return try withUnsafeTemporaryAllocation(byteCount: buf_size, alignment: 1) {
 res_ptr in

                        c_birthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)

                        if let required_size = size_ptr.baseAddress?.pointee {
                            if required_size > buf_size {
                                throw HsFFIError.requiredSizeIs(required_size)
                            }
                        }
                        return try dec.decode(User.self, from: Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none))
                    }
                } catch HsFFIError.requiredSizeIs(let required_size) {
                    print("Retrying with required size: \(required_size)")
                    return try withUnsafeTemporaryAllocation(byteCount: required_size, alignment:
 1) { res_ptr in
                        size_ptr.baseAddress?.pointee = required_size

                        c_birthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)

                        return try dec.decode(User.self, from: Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none))
                    }
                }
            }
        }
    } catch {
        print("Error decoding JSON probably: \(error)")
        return User(name: "", age: 0)
    }
}
```

</details>

We can test that this is working by replacing `ContentView` with:

```swift
struct ContentView: View {
    var body: some View {
        VStack {
            let user = birthday(user: User(name: "Ellie", age: 24))
            Text("Post-birthday, \(user.name) is: \(user.age)!")
        }
        .padding()
    }
}
```

And you should see:

![Fig 1. Swift app displays result of calling idiomatic Haskell function via idiomatic Swift wrapper](/images/calling-haskell-from-swift/ss1.jpeg)


# Metaprogramming at the boundaries

I want to give a quick preview of what is made possible by using compile-time
code generation features (Template Haskell in Haskell, Swift Macros in Swift).
This foreign function code generation API is exposed by the
[haskell-swift](https://github.com/alt-romes/haskell-swift) project, namely the
`swift-ffi` Haskell library and `haskell-ffi` Swift package. (Since it is out of
the scope of this tutorial, I will not cover how exactly the compile-time
code-generation code works, but instead use the API provided by these libraries)

These top-level foreign interaction facilities, coupled with the build tool also
provided by [haskell-swift](https://github.com/alt-romes/haskell-swift), one can
easily bootstrap and develop programs mixing Haskell and Swift! (Look forward to
a tutorial on bootstrapping and developing such a mixed project in the near future).

Let us consider the same example where we define an idiomatic `birthday :: User
-> User` function in Haskell and want to be able to call it from Swift as
`birthday(user: User) -> User`

## Haskell's perspective

To expose the `birthday` function to Swift, we simply use the `foreignExportSwift`
Template Haskell function. The whole module could look like this:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module MyLib where

-- ...
import Swift.FFI

data User
 = User { name :: String
        , age  :: Int
        }
        deriving stock    Generic
        deriving anyclass FromJSON
        deriving anyclass ToJSON

birthday :: User -> User
birthday User{age=x, name=y} = User{age=x+1, name=y}

$(foreignExportSwift 'birthday)
```
The key bit is the last `foreignExportSwift` call which will expose a C function
with the marshalling-based calling convention we outlined above.

## Swift's perspective

On the Swift side, we want to use the dual `@ForeignImportSwift` macro which
generates a Swift function wrapper which in turn invokes the C function exposed
by Haskell with the above marshalling strategy. The Swift file could
look like:

```swift
import HaskellFFI

struct User: Codable {
    let name: String
    let age: Int
}

@ForeignImportHaskell
func birthday(cconv: HsCallJSON, user: User) -> User { stub() }
```

where `birthday` could be called e.g. as:
```swift
birthday(user: User(name: "Pierre", age: 55))
```


# Coercing Haskell memory objects to Swift

Welcome to the dark corner (section?) of this blog post: the part where we
unsafely coerce an object in the Haskell heap into Swift. This section
will serve to walk the reader through some of the lower level details of
Haskell, and explore what it could mean to not marshal data between the language
boundary.

Even though we will avoid serialization and coerce memory representations, we
will still go through the C foreign function interface. In theory, it should be
possible to figure out the ABI/how to call Haskell symbols and somehow load and
call them in the Swift program accordingly, but I didn't go that far.

Changing up the data types a bit, also to avoid Strings because their memory
representations both in Haskell and Swift are complex to figure out, we have
```swift
struct Rect {
    let width: Int
    let height: Int
}

func double(sq: Rect) -> Rect {
    // wants to call /directly/ the Haskell function
}
```
and
```haskell
data Rect
  = Rect { width :: Int
         , height :: Int
         }
double :: Rect -> Rect
double Rect{width=x, height=y} = Rect{width=x*2, height=y*2}
```

## Memory representation of datatype in Haskell

In Haskell, most values are heap-allocated. This means that in the following
program, `x` is a pointer to a heap object of type `Rect`:
```haskell
myrect = Rect{width=12, height=24}

area = putStrLn ("Area: " ++ show (width myrect * height myrect))
```
We can actually inspect the memory representation of x by looking at the `C--`
code resulting from compiling this program standalone (`C--` is an intermediate
representation used by the Glasgow Haskell Compiler, which sits somewhere
between C and Assembly, and is in fact further compiled to Assembly by the
compiler in subsequent steps). Conjure up the invocation:
```bash
ghc -fforce-recomp -ddump-cmm Memory.hs > Memory.cmm
```
Opening `area.cmm` we can find the symbol `myrect`:
```c
section ""data" . Memory.myrect_closure" {
     Memory.myrect_closure:
         const Memory.Rect_con_info;
         const stg_INTLIKE_closure+449;
         const stg_INTLIKE_closure+641;
         const 3;
}
```
Essentially, the `myrect_closure` label points to 4 words: a `con_info` which
is metadata about the constructor used, 2 words have an `stg_INTLIKE_closure`,
which are the integers `12` and `24` that we used, and the last word is the
static link field (grep for `Note [STATIC_LINK fields]` in the GHC codebase for
details).

Note that the integers are not their literal number because the `Int` datatype
is also heap allocated (as almost everything else in Haskell). Instead, they are
static pointers to known integer values (two occurrences of the same integer will
use share the same location and pointer).
<details>
<summary>Here's a short GHC note about integer closures</summary>

```txt
Note [CHARLIKE and INTLIKE closures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These are static representations of Chars and small Ints, so that
we can remove dynamic Chars and Ints during garbage collection and
replace them with references to the static objects.
```
</details>

## Memory representation of struct in Swift

In Swift's side we can try a similar thing, let's compile a standalone Swift
program with the `Rect` struct defininition and with a top-level binding:
```swift
func makeRect(w: Int, h: Int) -> Rect {
    return Rect(width: 533, height:6464)
}
```
and chant the invocation with the flag to print the LLVM IR generated code:

```bash
swiftc Rect.swift -emit-ir -o - | swift demangle > Rect.S
```
Grepping `Rect.S` for `Rect` you should find:
```llvm
%T4main4RectV = type <{ %TSi, %TSi }>
%TSi = type <{ i64 }>
```
This syntax identifies the LLVM type of `Rect` to be a [packed
structure](https://llvm.org/docs/LangRef.html#t-struct) of two 64bit integers.
<details>
<summary>Here's an excerpt from the LLVM documentation on packed structures</summary>
```llvm
; Syntax
%T1 = type { <type list> }     ; Identified normal struct type
%T2 = type <{ <type list> }>   ; Identified packed struct type

; Examples:
{ i32, i32, i32 } ; A triple of three i32 values
{ float, ptr } ; A pair, where the first element is a float and the second element is a pointer.
<{ i8, i32 }> ; A packed struct known to be 5 bytes in size.
```
</details>
In short, the `Rect` type is a `type <{ i64, i64 }>` which means 8*2 bytes in memory.
To double check, we can look for the `makeRect` function that should return
exactly a 16 byte structure.

## Interpreting Haskell memory in Swift

Summarising, a `Rect` value in Swift will look like (A) in memory, and a Haskell
`Rect` value will look like (B) in memory:
```txt
    ┌────┬────┐
(A) │ 533│6464│
    └────┴────┘

    ┌────┐                        
(B) │    │                        
    └─┬──┘                        
      │   ┌────┬────┬────┬────┐   
      └──►│Rect│    │    │  3 │   
          └────┴┬───┴─┬──┴────┘   
                │     │           
          ┌─────┘     └─┐         
          │             │         
         ┌▼───┬────┐   ┌▼───┬────┐
         │ I# │ 12 │   │ I# │ 12 │
         └────┴────┘   └────┴────┘
```
The first thing we can address is storing the unboxed integers in the fields of
the Haskell datatype instead of static closures to boxed integers. To do this,
we make the field strict with the `!` annotation, and we use the `{-# UNPACK
#-}` pragma to force the unboxing when compiling with optimisations (although it
is likely that compiling with optimisations will unbox the strict integer
anyway without the pragma^[As an exercise, check this by looking at the `C--` representation of the
program compiled with optimisations, with the integer fields being strict, but
without using the `UNPACK` pragma]). That is, update `Rect` to the following,
recompile, and check the `C--` representation again.
```haskell
data Rect
  = Rect { width :: {-# UNPACK #-} !Int
         , height :: {-# UNPACK #-} !Int
         }
```
You'll see that compiling with optimisations will get rid of the static link
field and store literal integers in the fields of the datatype:
```c
Memory.myrect_closure:
    const Memory.Rect_con_info;
    const 12;
    const 24;
```
The memory now looking like
```txt
       ┌────┐                        
myrect │    │                        
       └─┬──┘                        
         │   ┌────┬────┬────┐
         └──►│Rect│ 12 │ 24 │  
             └────┴────┴────┘   
```

### Unsafe Coercing

It seems much simpler how we can return from a C function wrapping a Haskell
function a value which can be understood as a `struct Rect` in Swift. We need
to:

1. Unsafe coerce `myrect :: Rect` into a `myrect :: Ptr ()`, a pointer to the beginning of
   the Rect heap^[Really, the `myrect` value is not in the heap as it is static
   data put by the compiler in the data section, but for our purposes that
   difference does not matter] object
2. Add the size of a word to the `myrect` pointer, to get a pointer pointing to the
   beginning of the integer data (points to the field with `12`).

But this will not work. The problem is that occurrences of `myrect` in a
function body will not actually be a pointer to the first word of the Haskell
value in the heap. It is rather the pointer we want plus 1 (i.e. `myrect = ptr_to_rect_value_in_heap+1`).
We can see this by looking again at `C--` for this simple function:
```haskell
giveMyRect :: () -> Rect
giveMyRect () = myrect
```
Whose corresponding `C--` code looks like:
```c
Memory.giveMyRect_entry() {
   // ...
   R1 = Memory.myrect_closure+1;
   Sp = Sp + 8;
   call (P64[Sp])(R1)
}
```
Note how we return `myrect_closure+1`, the pointer to the `myrect_closure` plus
the tag.

The short story is that GHC will use an optimisation technique called [*pointer
tagging*](https://en.wikipedia.org/wiki/Tagged_pointer) to add a tag to every
heap pointer denoting which of the datatype constructors was used to construct
that value, or whether the value has not yet been evaluated:

* If the tag is `0` then the pointed-to value is either unevaluated or we don't
    know
* If the tag is `1` then we know the pointed-to value to have been constructed
    with the first constructor of the datatype
* If the tag is `2` then we know the pointed-to value to have been constructed
    with the second constructor of the datatype, and on and on up until `7`,
    which is the last possible tag^[Why `7`? For the slightly longer story:
    because in a 64bit system, memory is aligned to 64 bits, meaning that
    pointers to heap allocated memory will always have 3 bits that are
    necessarily `0` valued -- so we can use these 3 bits for tags -- that is, we
    can use tags from 0 to 7.].

This is a very cool optimisation because it allows us to choose the branch of
a case expression without needing to de-reference the pointer to check which
constructor the value used, as long as it has already been evaluated, because we
can simply check the tag, or e.g. avoiding memory accesses to check if a value
has already been evaluated as we can know, again, simply by looking at the tag.

So, we really need another step:

1.5. Subtract `1` from the unsafe-coerced `myrect :: Ptr ()`.

Let us write the Haskell function that we will export in the Haskell foreign
library `MyForeignLib.hs` (after copying over the last `Rect` definition). This
function returns a `Ptr ()`, where the unit type has no
real meaning as we coerce it away, knowing that the pointer points to a `Rect` in
memory that can be understood by Swift as a `Rect` struct.
```haskell
give_rect :: Ptr ()
give_rect =
  let
    -- Step 1
    tagged_ptr = unsafeCoerce myrect :: Ptr ()
    -- Step 1.5
    ptr_minus_1 = (unsafeCoerce tagged_ptr :: Word) - 1
    ptr_con_info = unsafeCoerce ptr_minus_1 :: Ptr ()
    -- Step 2
    ptr_final = ptr_con_info `plusPtr` 8 :: Ptr () -- 8 bytes
   in ptr_final

foreign export ccall give_rect :: Ptr ()
```

On the Swift side we need simply to de-reference a `Rect` from the pointer to
the Haskell memory. In `ContentView` we add:
```swift

```

## Conclusion

As you may have come to understand, this approach in practice, despite fun, is
pretty much the horrifying opposite of robust -- given how much we are relying
on the undefined memory layout of Swift, GHC optimisations, unsafe coerce, and
just so much undefined behaviour in general.

We only skimmed the surface of it despite having simplified it
massively. For example, if we weren't using a top-level binding, we'd have to
worry about the garbage collector getting to the heap object before Swift could
(and instead we'd have to use something like `StablePtr`).

The veredict is that this is not a reasonable option to be considered for the
large applications mixing Haskell and Swift that are our goal.
