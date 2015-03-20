# Overall Status #

As of 2010-04-22, development was put on hold.  There are working ports of all implementations in Parts I through III (see the "Downloads" page).

I was in the process of converting the implementations over to code generation.  The generator code allows for two types of generation, one that consists of mainly copying code from the source directory, and another that actually promotes code reuse (but is still very primitive).  For some implementations, the former makes sense as a final state, while for others I only used it so that I could create a working download.

# Per-Implementation Status #

### Migrated: Part I (Untyped Systems) ###

All of these use copy- (that is, fake-) code generation.  However, because they differ so much from the implementations involving types, this isn't that bad of a final state.

  * arith (Chapters 3 - 4)
  * untyped (Chapters 5 - 7)
  * fulluntyped (Chapters 5 - 7)

### Under Migration: Part II (Simple Types) & Part III (Subtyping) ###

I was in the process of migrating implementations in these two parts.

  * tyarith (Chapter 8) -- done, using copy-code generation (this is possibly okay)
  * simplebool (Chapter 10) -- temporarily uses copy-code gen (should be converted)
  * fullsimple (Chapter 9 and 11) -- done, using real code gen
  * fullref (Chapters 13 and 18) -- temporarily uses copy-code gen (should be converted)
  * fullerror (Chapter 14) -- temporarily uses copy-code gen (should be converted)
  * fullsub (Chapters 15 - 17) -- done, using real code gen
  * rcdsubbot (Chapters 15 - 17) -- temporarily uses copy-code gen (should be converted)
  * bot (Chapter 16) -- temporarily uses copy-code gen (should be converted)

### In Progress: Part IV (Recursive Types) ###

I had started development of these implementation, but they are very incomplete.  It may be better to start over from scratch than to continue development on them.

  * fullisorec (Chapter 20)
  * fullequirec (Chapter 20)
  * equirec (Chapter 21)

### TODO: Part V (Polymorphism) & Part VI (Higher-Order Systems) ###

None of these have been started.

  * reconbase & recon & fullrecon(Chapter 22)
  * fullpoly (Chapters 23 & 24)
  * fullfsub (Chapter 26)
  * fomsub & fullfomsub (Chapters 26, 30)
  * fullfsubref (Chapter 27)
  * purefsub & fullfsub (Chapter 28)
  * fomega & fullomega (Chapters 23, 29, 30)
  * fullupdate (Chapter 32)

### TODO: Exercises ###

In addition to implementations, Pierce provides code for some exercises.  I did not work on ports of them.

  * letexercise
  * joinexercise