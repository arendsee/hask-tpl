Haskell implementations of the toy languages in Benjamin Pierce's "Types an
Programming Languages" textbook.


## Chapter 3 - untyped arithmetic expressions

This is a Haskell implementation of simple language with the following grammar:

t := true                   -- constant true
     false                  -- constant false
     0                      -- constant zero
     if t then t else t     -- if else
     succ t                 -- successor
     pred t                 -- predecessor
     iszero t               -- check is value is zero

### Implementation

I expressed the grammar with

```
data Expr
  = T 
  | F 
  | IfThen Expr Expr Expr
  | Zero
  | Succ Expr 
  | Pred Expr
  | IsZero Expr
  | Stuck
```

I didn't write a lexer or parser, but you can call `cabal repl` to open the
GHCi shell with the interpreter loaded. Then you can write expressions like:

```
ghci> IfThen(IsZero Zero) (IfThen (IsZero (Succ Zero)) T F) F
```

These will simply be printed.

To evaluate one step:

```
ghci> step ( IfThen(IsZero Zero) (IfThen (IsZero (Succ Zero)) T F) F )

 IfThen T (IfThen (IsZero (Succ Zero)) T F) F
```

To evaluate all the way to a normal, use `walk`. This will produce a list of
intermediate terms.
