---

title: Unsafe Coercing Haskell to Swift

description: Crossing the language boundary between Haskell and Swift.
             This is the second part of an in-depth guide into developing native
             applications using Haskell with Swift.

tags: haskell, swift

---

TODO
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
    -- WRONG!!! Needs to unsafeCoerce# to Addr#...
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
// Swift here
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
