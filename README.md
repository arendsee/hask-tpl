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
