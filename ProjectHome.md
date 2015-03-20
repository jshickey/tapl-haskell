# Overview #

The goal of this project is to provide [Haskell](http://www.haskell.org/) ports of all [OCaml](http://caml.inria.fr/) implementations for ["Types and Programming Languages"](http://www.cis.upenn.edu/~bcpierce/tapl) (TAPL) by Benjamin C. Pierce.

For each system found under "Implementation" at [TAPL](http://www.cis.upenn.edu/~bcpierce/tapl), there is a source directory of the same name.  Code in the "generator" directory (described below) combines this code with common code in order to generate a corresponding subdirectory under the "gen" directory.  The resulting code is completely self-contained, allowing the user to experiment with each of these implementations independently.

Each Haskell implementation uses [Parsec](http://legacy.cs.uu.nl/daan/parsec.html) for parsing, and has an [HUnit](http://hunit.sourceforge.net/) test suite.

# Code Generation #

As I added ports for the different implementations, I realized that I was duplicating a lot of code.  I abstracted some into common files, but when one of the main types would change slightly (e.g., a new type was added), I didn't know how to cleanly abstract across the different versions of the type.  I considered [Template Haskell](http://www.haskell.org/th/), but decided against it, because I could not see how to use it to create (relatively) simple Haskell code that be used by someone whose primary interest is in studying TAPL.

I eventually settled on using code generation (implemented in the "generator" directory).  In a config file, an implementation specifies sets of terms, types, tests, and other global options.  All of the necessary code is then generated in a directory of the same name under the top-level "gen" directory.  The generator itself is not pretty, but this approach allows the user to explore an individual implementation without suffering (too much) from abstractions that are not relevant to the code at hand.  (Of course, any changes will be lost unless they are migrated to the generator.)

The code generator is very primitive, and for some systems (e.g., ones without types), code "generation" consists mainly of copying files, with only minimal generation of the Makefile.

# Status #

On the "Downloads" page, there is a tar file with working ports of all of the implementations in the first three parts of the book (which covers chapters 1-19).  Specifically, the included implementations are:

  * arith (Chapters 3 - 4)
  * untyped (Chapters 5 - 7)
  * fulluntyped (Chapters 5 - 7)
  * tyarith (Chapter 8)
  * simplebool (Chapter 10)
  * fullsimple (Chapter 9 and 11)
  * fullref (Chapters 13 and 18)
  * fullerror (Chapter 14)
  * fullsub (Chapters 15 - 17)
  * rcdsubbot (Chapters 15 - 17)
  * bot (Chapter 16)

As of 2010-04-22, I have put this project on indefinite hold (unless someone else wants to pick it up, which I would be happy to oblige).  I simply don't have sufficient time to devote to it.  See the wiki for notes on where I left off development.

# Notes #

The code has only been tested using GHC on Ubuntu.

Contributions and/or feedback are very much welcomed.  Please contact [Ryan W. Porter](http://www.ryanwporter.com/).