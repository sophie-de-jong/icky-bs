# ICKY-BS
Simple SKI combinator calculus language written in Rust.

## Quick Start
```console
$ cargo run examples/logic.ski
```

## Goal/Motivation
The goal was to create a simple language based on six basic combinators from SKI combinator calculus and try and push it as far as possible. Currently, the project is at a state where you could realistically use it to write some basic programs, however inefficiently. While a lot is abstracted from the programmer, everything under the hood is implemented and evaluated with combinator calculus.

## Combinators
Currently, the language supports six common combinators from SKI combinator calculus. They are defined as the following:
```
I x     -> x
K x y   -> x
S f g x -> f x ( g x )
B f g x -> f ( g x )
C f g x -> f x g
Y f     -> f (Y f)
```

Combinators can be seen as functions, where the name is the capital letter and its arguments are the symbols following it. For example, this combinator expression reduces as follows...
```
K I x y
 => I y
 => y
```
...and this more complicated one evaluates to the following
```
B C (C I) x y z
 => C ((C I) x) y z
 => ((C I) x) z y
 => (C I x) z y
 => C I x z y
 => I z x y
 => z x y
```

## Definitions
You can define new combinators based on old ones, and the interpreter will compile the expression into SKI combinator calculus. For example:
```
// TRUE is the combinator that takes two functions and returns the first...
TRUE f g := f
// FALSE is the combinator that takes two functions and returns the second...
FALSE f g := g

/force TRUE
 => K
/force FALSE
 => K I
```
Once you have these definitions, you can build even more:
```
NOT p := p FALSE TRUE
NOT TRUE
 => FALSE
NOT FALSE
 => TRUE
```
The standard library is full of these basic language features you can expect (like booleans, numbers, arithmetic, data structures) but they are all defined in terms of these six combinators!

## Commands
To make the language usable, there are a few commands you can use:
- `/link <file name>` imports all the definitions from a file into the current session
- `/repr <combinator>` see the definition of a combinator
- `/source <combinator>` see where a combinator is defined
- `/debug` prints out a list of all the defined combinators
- `/quit` quits the current session
- `/force <expr>` forces an expression to evaluate, even if arguments are missing

## References
- https://www.youtube.com/watch?v=gnrSedVucXs
- https://en.wikipedia.org/wiki/SKI_combinator_calculus
- https://en.wikipedia.org/wiki/Combinatory_logic
- https://en.wikipedia.org/wiki/Church_encoding
- https://en.wikipedia.org/wiki/Lambda_calculus
