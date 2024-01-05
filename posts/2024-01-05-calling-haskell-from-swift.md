---

title: Calling Haskell from Swift

description: TODO
             Second part of an in-depth guide into developing native
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

For example, a Haskell function
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
Haskell to Swift**, and vice versa. There's certainly more than one way to do
it, but we are going to **serialize (most) inputs and outputs** of a function.

In the end, I promise to also dive a bit into coercing in-memory representations
of a datatype in between Haskell and Swift!

As a first step, we write the `User` data type and `birthday` function to
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
compiler implementation details and memory representation incompatabilities.

We will use the same marshaling strategy that [Calling Purgatory from Heaven: Binding to Rust in Haskell](https://well-typed.com/blog/2023/03/purgatory) does.
In short, the idiomatic Haskell function is wrapped by a low-level one which
deserializes the Haskell values from the argument buffers, and serializes the
function result to a buffer that the caller provides. More specifically,

* For each argument of the original function, we have a pair `(Ptr CChar, Int)` -- a string of characters and the size of that string
* For the result of the original function, we have an additional argument `(Ptr CChar, Ptr Int)` --
    an empty buffer in memory and a pointer to the size of that buffer, allocated by the caller.
* For each argument, we read the C string into a Haskell argument to the original function.
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

We apply the original `birthday` function to the decoded `user`.
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
