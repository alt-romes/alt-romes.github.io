---
author: Rodrigo Mesquita
---

# Parser Combinators and Interpreting a PL

I propose we extend the functional programming course with a module covering
parser combinators, together with an assignment that covers some basics ideas
behind interpreting a programming language (e.g. using an environment state) for
the student to put them into practice.

## Motivation

A `Parser` (as defined in `parsec`) is an abstract data type that forms a Monad,
and monadic parser combinators useful functions that work in terms of `Parser`s. 

After learning about Monads and common instances such as
`Maybe`, `Reader`, `State`, the student should be able to work with the
abstractions provided by the Monad type class even without knowing about its
implementation.

Parser combinators would showcase functional parsers, the use of monads to
structure functional programs, and the use of special syntax for monadic
programs in Haskell.

Parser combinators are also very commonly used in pratice (in different
flavours, e.g. `attoparsec` for speed, `megaparsec` balanced with better error
messages), be it for processing some standard message format such as `JSON` or
for implementing a client requested DLS.

The student would then reinforce their understanding of monads and practice
using them with this new monadic type, would hopefully develop their interest
towards functional programming -- using parser combinators almost feels like
magic (when it's really a clever instance of Monad, and the syntatic sugar Haskell
provides for Monadic programming) -- and put this knowledge to good use in a real
project they might be curious about which is "implementing a small programming
language".

Regarding the said assignment on "implementing a small programming language",
I'd like to note how Haskell is a *best-in-class* language to implement
programming languages. I think that by going over parser combinators we unlock
the possibilty of writing a simple interpreted language. I believe many students
would have interest in doing so, and would be pleasantly surprised by the
simplicity of doing so in Haskell.

For this assignment we'd model our language as a data type in haskell, write a
parser for it and an interpreter for it. Additionally we can hint at the
necessity of an environment to keep track of variables (and students could put
again into practice the state monad (or perhaps writing a custom one)!)

I think it'd boil down to
```hs
data Expr = Var String
          | Num Int
          | Bool Bool
          | String String
          | Add Expr Expr
          | Mult Expr Expr
          | Equals Expr Expr
          | IfThenElse Expr [Expr] [Expr]
          | For Expr [Expr] ?
          | Assignment String Expr ?
          | Print Expr ?

type Program = [Expr]

type Environment = [(String, Expr)]

-- >>> parse "var x = 1; print (x + 2)"
-- [Assignment "x" (Num 1), Print (Add (Var "x") (Num 2))]
parse :: String -> Program
parse = undefined


-- >>> eval [Assignment "x" (Num 1), Print (Add (Var "x") (Num 2))] []
-- Right (["2"], [("x", Num 1)])
eval :: Program -> Environment -> Either Error ([String] {- list of strings to print -}, Environment) -- evaluation might fail with an error
eval = undefined
```

## Examples

If we use the `megaparsec` library, parsing a simple imperative language such as
```hs
var x = 1;

if (x == 1) { var x = x * 3; } else { var x = x * 2; };

print x;
```

Could look something like
```hs
assignment :: Parser Expr
assignment = do
    reservedWord "var"
    name <- identifier
    symbol "="
    val <- expr
    symbol ";"
    return (Assignment name val)

ifThenElse :: Parser Expr
ifThenElse = do
    reservedWord "if"
    symbol "("
    cond <- expr
    symbol ")"
    symbol "{"
    thens <- many expr
    symbol "}"
    reservedWord "else"
    symbol "{"
    elses <- many expr
    symbol "}"
    symbol ";"
    return (IfThenElse cond thens elses)

printP :: Parser Expr
printP = do
    reservedWord "print"
    e <- expr
    symbol ";"
    return (Print e)

expr :: Parser Expr
expr = ifThenElse <|> assignment <|> printP
```

## Costs and Drawbacks

I think the simple programming language idea is a fun and motivating example,
and think this simple one is quite feasible, however when conferring with
Haskell professors in the #haskell IRC channel, they said using parser
combinators I could instead just have the students write a JSON parser, which
would be simpler

That is, the drawback could be the parser combinators + programming language being too difficult.

Also, perhaps it would be more interesting to show case a lower-level use of
parser combinators, with more `space`, `many`, `many1`, `char`, `satisfy`,
etc...

## Alternatives

We could use parser combinators for a range of other things. As mentioned above,
we could write a JSON parser.

Perhaps we could bring more into light with parser combinators, for example the
`Alternative` instance of `Parser`.

We could focus on the implementation of the parser combinators and the instance
of the Monad type class.

## Unresolved Questions

Should we use a parsing library or define parser combinators by hand? Which
parsing library should we use? I would say `parsec` or `megaparsec`.

Which assignment would be best/most interesting?

## Endorsements

The idea of parser combinators was supported by more than one university
professor from the #haskell IRC channel (I believe they mentioned they also go
over them with their students).
