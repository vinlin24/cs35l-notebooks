**Week 9 Lecture Notes**

- [Git Internals (Continued)](#git-internals-continued)
  - [Comparison to File Systems](#comparison-to-file-systems)
  - [Creating Git Objects](#creating-git-objects)
    - [Object Types](#object-types)
  - [Commit Objects](#commit-objects)
  - [Internal Form for Blobs](#internal-form-for-blobs)
- [Compression](#compression)
  - [(1) Huffman Coding](#1-huffman-coding)
    - [Decompressing](#decompressing)
    - [Table Generation Algorithm](#table-generation-algorithm)
  - [(2) Dictionary Coding](#2-dictionary-coding)
    - [Decompressing](#decompressing-1)
  - [zlib](#zlib)
- [Git Tagging](#git-tagging)
  - [Signed Tags](#signed-tags)
    - [Cryptography Aside](#cryptography-aside)
- [Reflog](#reflog)
  - [Object Naming Algebra](#object-naming-algebra)
    - [Commit Name Notation](#commit-name-notation)
- [Submodules](#submodules)
- [How Programs Run](#how-programs-run)
  - [Interpreters](#interpreters)
  - [Bytecode](#bytecode)
  - [Compilers](#compilers)
  - [JIT Compilation](#jit-compilation)
- [GCC Internals](#gcc-internals)
  - [Compiler Portability](#compiler-portability)
  - [GCC Target Assumptions](#gcc-target-assumptions)
  - [GCC Source Code](#gcc-source-code)
    - [Files](#files)

---


# Git Internals (Continued)


## Comparison to File Systems

It seems that Git uses a collection of files to represent a repository (and the index). In reality, Git actually uses a combination of secondary storage and RAM (cache).

A local Git repository and the index are made up of **objects** and "other stuff". Git objects are like a tree of files in the file system.

**REMINDER:** Every file has a unique index, namely the **inode number**, which you can see with `ls -`.

Analogously, SHA-1 checksums for Git objects have the role that inode numbers have in file systems. They are comparable to pointers in C/C++, values that uniquely identify the actual objects they reference.

A crucial difference is that files in the file system can be mutable. inode numbers thus exist *independently* of the contents of their files. However, checksums uniquely identify objects *by their content*. Therefore, you **cannot** change objects' contents.

A similarity is that they are both directed acyclic graphs (DAGs). In the file system, it's guaranteed by the OS that you cannot have cycles. For Git objects, you cannot create a cycle because you can only *add* to history, not change it.


## Creating Git Objects

Generate a checksum from string content:

```console
$ echo 'Arma virumque cano.' | git hash-object --stdin
24b390b0e3489b71977f5c7242a4679287349242
```

You can also supply a file name as a positional argument instead of using `--stdin`.

Computing the checksum *and* writing it to the repository:

```console
$ echo 'Arma virumque cano.' | git hash-object --stdin -w
24b390b0e3489b71977f5c7242a4679287349242
$ # This object now exists in the file system
$ ls -l .git/objects/24/b390b0e3489b71977f5c7242a4679287349242
-r--r--r-- 1 vinlin 197609 36 Nov 22 03:46 .git/objects/24/b390b0e3489b71977f5c7242a4679287349242
```

Notice that the file has 444 permissions. *No one* is allowed to write to this file.

*There's always a troublemaker!*

```shell
chmod u+w .git/objects/24/b390b0e3489b71977f5c7242a4679287349242
```

You're technically *allowed* to do this, but in doing so, you're violating an invariant that Git trusts in order to properly function, so live your with your consequences I guess.

Decoding the content of the created Git object:

```console
$ git cat-file -p 24b390b0e3489b71977f5c7242a4679287349242
Arma virumque cano.
```

You can check the *type* of the file with:

```console
$ # !$ is shorthand for the last arg of prev cmd!
$ git cat-file -t !$
blob
```


### Object Types

- **blob**: represents any bytes sequence, like regular files in a file system
- **tree**: represents a node in a tree of objects; maps names to SHA-1 checksums of blobs or other tree

You can see the organization in action with something like:

```console
$ git cat-file -p 'main^{tree}'
100644 blob fa86c55e687fef76cb5801776756a6cb204cd2f9  .gitignore
100644 blob 630e72d7faef82103be5f27139030b67ef942ca0  README.md
100644 blob 1c6781665caca7fa3350bdfcb4a820fbec2dab05  week1.md
100644 blob f1f8f3890378332cffb6ddfb3dbc84c20e3a9470  week2.md
100644 blob d94ba050bf8045d989617e2fdf75206b2d7d97a4  week3.md
100644 blob 1131c977a2f8db9031e081a16c028bf493dfd02a  week4.md
100644 blob 4923796df144352f4784e7a90cf58a98e64d7e04  week5.md
100644 blob 8017ea5aaff41016d6813e329f75a008d2b8cd0a  week6.md
100644 blob e34d49dc0e8754b9de4e1a46449541c15e31af86  week7.md
100644 blob 8110b211ea75e284e8ad5a55b8e97c9927a0dc8a  week8.md
```

Each commit points to a tree. The tree is like a directory in a file system. Above, `main^{tree}` refers to the tree object referenced by the commit object referenced by the branch named `main`.

Examining the output, we see 4 columns:

```
100644 blob fa86c55e687fef76cb5801776756a6cb204cd2f9  .gitignore
```
```
(mode) (type) (SHA-1 checksum) (name)
```

The first column is octal digits, the last three of which represent the Linux permissions of the file in question (so `644` means `rw-r--r--`).

The second column is the type, **blob**, another **tree**, etc.

The third column is the SHA-1 checksum of the *referred* object. After all, a tree just represents a pointer to a bunch of other objects.

The fourth column is the name of the file.


## Commit Objects

Every commit object points to two things. First is the tree the commit represents. The second is the parent commit object(s).

```console
$ git cat-file -p main
tree aa3ca55b785ab21cfbbca0a89843151d389dcac8
parent 65422241d84b3087142adcf0906b5f86e69230e3
author Vincent Lin <vinlin24@outlook.com> 1668666720 -0800
committer Vincent Lin <vinlin24@outlook.com> 1668666720 -0800

Add 11/16 lecture notes
```

We see that every commit object contains information about its parent, author and committer.

You can think of `git log` (a *porcelain* command) as pretty-printing what `cat-file -p REF` (a *plumbing* command) tells us.

Three levels going on with every commit object:

```
(commit object)...
       |
       |                  +---------> (other objects)
       v                  |
(commit object) --> (tree object) --> (other objects)
       |                  |
       |                  +---------> (other objects)
       v
(commit object)...
```

Every commit points to a different tree, but the tree can share the objects they reference. When you make a commit for small changes, you don't have to rebuild a bunch of objects. Unchanged objects are *reused*.

However, because changing a file would update the tree containing it, changing a deeply nested file would require new tree objects all the way up to the project root for the new commit.

If a file is extremely large, Git can store a *diff* instead of a full copy and just remember how to restore the full content when the blob is needed.


## Internal Form for Blobs

Represents some byte stream `B`, say 47 bytes long. The form has a header that tells the type of the object, some control information like how large it is, and then the actual content.

```
+-+-+-+-+-+-+-+--+------------------------------+
|b|l|o|b| |4|7|\0|<--------------B------------->|
+-+-+-+-+-+-+-+--+------------------------------+
```

This string is then compressed using zlib, and that's the form it is stored in on the file system.

This string is also what is fed to the SHA-1 checksum algorithm.

**ASIDE:** Mathematicians in the past decade have been working to try and crack SHA-1 and have succeeded for the most part. SHA-1 is no longer "reliable", but the chance of a *random* collision (not one with file contents engineered to hash a collision) is still astronomically small.


# Compression

There are two big ideas used in compression programs like zlib, etc.


## (1) Huffman Coding

The best possible algorithm under its constraints:

1. Input is a sequence of symbols taken from a known alphabet.
3. We know the *popularity* of each input symbol.
4. Each input symbol corresponds to some bit stream.

When you have these constraints, you can assemble a table mapping input symbols to output bit strings, with lengths chosen based on how frequent they are. With ASCII characters in the context of the English language as an example:

| Symbol  | Bit String   |
| ------- | ------------ |
| (space) | 00           |
| e       | 010          |
| t       | 011          |
| a       | 100          |
| o       | 1010         |
| ...     | ...          |
| ^Q      | 111011011101 |

Notice that the least popular symbols could map to a representation that takes up even more space than their original form, but that's okay because they occur so scarcely that on average it does not hinder the total length by much.

Then, compression is simply a matter of iterating over the input symbols and performing a lookup.


### Decompressing

1. You have a pre-computed table shared by the compressor and de-compressor.
2. Sender computes the table and sends it first. This introduces some overhead in transmission, but more importantly, it has to read all of the input first.
3. **Dynamic Hoffman tables** - both sender and recipient start off with a table in their head that's perfectly balanced (for example, every character standing for itself). Sender then sends the first byte and updates its Hoffman table according to the byte that it already sent. The recipient does the same, and because both sides can detect the same popularity of the data they are sending/receiving, their tables are kept in sync. This is a little slower because they have to update the tables dynamically as they go but not by much. You also have to agree on what to do when there's a tie in popularity. Both sides need to be able to stay in lock step with each other.


### Table Generation Algorithm

Suppose you computed a table of probabilities (possibly by counting the frequencies of each symbol in some sample text):

| Symbol   | Probability |
| -------- | ----------- |
| (space)  | 0.1         |
| e        | 0.07        |
| t        | 0.05        |
| ...      | ...         |
| **SUM:** | 1.0         |

You build a **Huffman tree** to determine the bit string to assign. Review your MATH 61 notes lol.


## (2) Dictionary Coding

An approach that can be more optimal than Huffman coding by relaxing a constraint. Instead of mapping individual symbols to bit strings, you map sequences of symbols to bit strings.

For example:

| Sequence | Encoding |
| -------- | -------- |
| the      | 1        |
| a        | 2        |
| an       | 3        |
| or       | 4        |
| ...      | ...      |

You read the text, divide it into *words* (hence "dictionary" coding), and then for each word, you assign a number to it. Of course, *words* here refer to any byte streams, so they likely include punctuation marks or spaces for efficiency.

If the sender and recipient know this table, then a string can be compressed much more efficiently than when using Huffman coding.


### Decompressing

There is also **dynamic dictionary coding.** Start with a trivial dictionary with 256 entries, where each word is length 1 and represented with some number from 0-255. As data is transmitted, the tables on both side are updated dynamically and in the same way such that by the end, they can use dictionary coding.

Suppose you already sent a substantial chunk of data. You can send data in terms of what you already sent. For example, if you're about to send the word "French", and you've sent "French" 97500 bytes ago, you can send a pair of integers, offset and length, like (97500, 6), which the recipient can decode with their copy of the data.

There is also the **sliding window algorithm** where you have a moving range of the sent data that you can use for this approach. This is necessary because RAM has a limit.


## zlib

zlib uses both of these approaches. It uses dictionary coding to output a sequence of numbers, and then it uses Huffman coding on that sequence.

**Does compression guarantee you get something smaller than what you started off with?**

No. If this were true, than you can compress an indefinite number of times, all the way until 1 byte, which would then have to be compressed to 0 bytes under this assumption. This is obviously impossible to do without losing information.

**ASIDE:** A **general purpose compression algorithm** has to be able to work with *any* byte sequence. Certain algorithms can be specialized for certain types of input, but software like Git needs something general purpose like zlib because it has to be able to work with any blob.


# Git Tagging

A **branch** is a lightweight movable name for a commit.

A **tag** is a heavier-weight constant name. It's like a label that references a commit, and unlike a branch, it does not move when new commits are added. It contains:

- Name
- Commit ID
- Other metadata

Listing your tags:

```shell
git tag
```

When you checkout to a commit that isn't a branch tip, you enter **detached HEAD mode**. You can't make commits in this state, but if you want to make changes from this version of the codebase, you can checkout to a new branch off this commit as you normally would:

```shell
git checkout -b mybranch
```

Tags aren't affected by this.

Tags tend to be used for releases.

For ordinary (unsigned) tags, you can simply create one with:

```shell
git tag v38  # Tag HEAD with the ref 'v38'
```

These tags however are *local* to your repository. To *publish* this tag to the outside world...

`git push` won't work because what `push` actually does is look at current `HEAD` and propagate that head and the branches associated with that head and pushes those changes upstream. In other words, it pushes gthe *current branch* upstream. Tags are just names for commits, so they're not part of any branches.

Developers may also be in the habit of using many tags, and with many such developers at the same time, automatically pushing tags with every `git push` would very quickly pollute the upstream repository. Thus, no pushing tags by default is by design.

You must manually push tags:

```shell
git push --tags
```


## Signed Tags

In a large codebase, chances are there will be many commits and tags that are in a bad state.

Signing is like cryptographically "approving" a commit.

```shell
git tag -s v37 -m "Good version 37"
```

The `-s` flag signs the ref named `v37` with a message `Good version 37`. The signing creates a **GPG (GNU Privacy Guard) key**.

Other people can then check the tag:

```shell
git tag -v v37
```


### Cryptography Aside

The simplest system is a **private key system**. Sender and recipient share some key `K`.

You take your message `{M}` encrypted with a key `K`, and that's what's published. Attackers can't directly see the contents of the data.

The recipient then uses their same `K` to decrypt `{M}`.

```
[ {M}K ] ===> [ {M}K ]K = M
```

A **public key system** is when you have a pair of keys `(U, K)`, a public key and private key respectively.

The sender keeps its private key `K` secret and doesn't even reveal it to the receipient. The recipient publishes its public key `U`.

The sender encrypts the message with the recipient's public key `{M}U` and publishes it. The recipient then uses their private key `K` to decrypt the message.

```
[ {M}U ] ===> [ {M}U ]K = M
```

This also works vice versa. The sender can encrypt the message with their private key while the recipient uses the sender's public key to decrypt it.

---

So we can take a Git tag, say `v37`, which references some commit with ID `09cf`... What signing a tag does it taking the tag and signing it with a private key `K` and then publishing the result, which looks like a random bit string. Any other user can then use the signer's public key to decrypt the tag.

A weakness is that if the private key `K` is leaked out, attackers can put out a bad version of the software that looks like it was signed by authority.

Tag sigining is used for security-sensitive software.


# Reflog

```shell
git reflog
```

This inspects the `reflog` file. It is not maintained as an object in the object database. It is a *separate* log file that logs the *changes* that you made to the object database (most recent first).

`git reflog` lists the changes you make to a repository. It keeps track of things like moving `HEAD` as a result of committing, checking out, etc.

You can think of it as a "second order derivative" of your repository with respect to time, with the first order being the commit history. Reflog is like changes *about* changes.


## Object Naming Algebra

If you read the `man` pages for any Git commands, chances are you will encounter some recurring vocabulary.

Firstly, names fall into two major categories:

**pathspec**

This refers to working file names, stuff like `src/main.c`.

Example of using a pathspec in a command:

```shell
git diff foo.c
```

**commit names**

This refers to commits and their aliases, stuff like `HEAD^`, etc.

Example of using a commit name in a command:

```shell
git diff main
```

---

These two sets have different naming conventions. Notice that for the `git diff` examples, what if there's an ambiguity, like a file named `main`? Git commands support a special argument `--` that separates the part of a Git command that talks about *commit names* and parts that talk about *pathspecs.*. For example:

```shell
git diff main -- foo.c
```

This says, "show the difference between `foo.c` in the index and the one that's in the commit referenced by `main`."

If the `--` delimiter is missing, Git will try to guess. It general, you should try to avoid ambiguity by explicitly supplying the `--` token:

The equivalent commands from the prior examples:

```shell
git diff -- foo.c
git diff main --
```


### Commit Name Notation

The `^` suffix on a commit name denotes a *parent* of that commit:

```
HEAD
N^    # parent of N
N^2   # second parent of N
```

You can also use `~` to automatically take the first option when the ancestors of commits happen to branch into different parents:

```
N~2  # N's grandparent
```

You can combine the notations (evaluating left-to-right):

```
N^^~2  # The grandparent of the second parent of N
```

You can also put the `^` in front, which means is used as logical negation":

```
^N    # "not N"
```

This is a name for all commits that are *unreachable* fromm N.

You can combine such conditions, as if `&&`:

```
^M N   # reachable from N but not from M
```

This pattern is so common that it has a shorthand:

```
M..N
```

We've seen this before with a linear history. In such cases, it's like the notion of a "range" of commits. This can be generalized to non-linear histories, where they technically mean "reachable from N but not from M".

Another notation that's like an XOR:

```
M...N
```

This means reachable from M or from N, but not both. This is useful in things like `git log`:

```shell
git log main...maint
```

This tells us the "symmetric difference" between the two branches: commits that are in `main` but not `maint`, commits that are in `maint` but not `main`, but not commits common to both.


# Submodules

**SCENARIOS:**

- Your project needs some other project's source code. For example, perhaps you want your system to be buildable even on platforms even if another system is absent.
- There are some software packages that are intended be used only in source code form.

**Case study:**

In `grep`, there's a submodule called `gnulib`. This is intended to be a **portability library** (source code only). It helps "emulate" Linux atop other operating systems like MacOS and Windows by implementing certain features that are absent on those systems.

You can create a submodule from some source:

```shell
git submodule add https://github.com/a/b/c
```

This is similar to cloning, but instead of creating a new repository complete with its own `.git`, it:

- Creates an empty subdirectory
- Create a `.gitmodules` file in the main project that establishes the relationship between the main project and the subdirectory

Then, you run this, which initializes the submodule and then gets the source code from the provided link.

```
git submodule init
git submodule update
```

The rationale behind this feature is due to the fact that the other project is evolving. This sort of setup gives you the ability to update to the latest version of the submodule at your convenience. You want a *controlled update*.

You can also provide the commit ID to `git submodule update` to update to a specific version of the submodule.

Furthermore, updating a submodule is very simple. Instead of making edits to possibly to many files if you kept the dependency as part of your repository, the changes you make to the main project are simply which commit ID to use for the submodule.

Also, you don't want to be changing the subproject directly. That's the responsibility of their maintainers.


# How Programs Run

We can divide how we execute programs into two main categories: **interpreters** and **compilers**. There also exists a compromise between these two approaches that uses **bytecode**.


## Interpreters

The **interpreter** itself is some program (compiled from some language like C in the case of CPython).

**BASIC IDEA:** Take the source code, check it a bit (validating syntax, etc.), then translate it as quickly as possible into an intermediate form on RAM.

```python
def dist(x, y)
    return sqrt(x*x + y*y)
```

One approach for the translated form can be some symbol table with symbols pointing to tree structures, which are representations of the source code divided into elementary parts, like the function time, the arguments, the return expression, etc.

The interpreter will then walk through the data structure and evaluate the nodes. It's as if the source code of the interpreter has some kind of function like:

```c
value_t evalexpr(tree_t t) {
  switch (t->type) {
    case PRINT_STMT:
      value_t v = evalexpr(t->printargs);
      printf("%s", v);
    /* ... */
  }
}
```

Notice that the interpreter needs to be highly recursive, evaluating subexpressions before completing a given expression.

Many interpreters use this approach, but due its recursive nature, it's a lot slower compared to machine code.


## Bytecode

With this approach, the program (typically still called an "interpreter") translates the source code into bytecode form. The `dist` symbol representing the function above then becomes like a pointer to some stream of operations:

```
push x
dup
*
push y
dup
*
*
+
sqrt
```

Because this is simply some byte stream, the interpreter can maintain some instruction pointer and *iterate* through the stream, executing the operations sequentially in similar fashion to true machine code. The C code now looks something like:

```c
switch (*ip++) {
  case DUP:
    top = *sp;
    *--sp = top;
    break;
  /* ... */
}
```

We've seen this exact concept back when we learned about Emacs Lisp bytecode compilation. The tree structure is like the `list` and `cons` model, and the bytecode is a sequential byte stream.

Other languages that use this approach include Python and, although not required by the spec, JavaScript on most browsers. Bash may also be another one.

**ADVANTAGES**

This form also produces *smaller code*, which fits into the data cache better. Tree data structures take up more space because they have a bunch of pointers.

This form is also more *performant*. There's no recursion, simply iteratively referencing/derefencing pointers.

**DISADVANTAGES**

Bytecode takes longer to produce compared to tree structures in fully interpreted languages.

The tree structure approach is also more debuggable because you can trace through the tree to find what went wrong.


## Compilers

Abstractly, like a *function* that converts source code to machine code such that if you execute the machine code, the behavior will be identical to what is expected by the source code language spec.

**DISADVANTAGES**

Translation time is large.

Much less debuggable. Machine code is not human-readable, so we typically have to resort to debuggers like GDB.

**ADVANTAGES**

Much more performant at runtime. Compiled languages skip the necessity of `switch`-like code or even a data stack;they can keep their data in registers instead of plain old RAM.


## JIT Compilation

A typical optimization that's done these days are like a compromise between bytecode and compilers, the concept of **runtime compilers** (aka **just-in-time (JIT) compilers**).

What it does is produce bytecode and/or possibly even tree structures, but the compiler will also have a subroutine that can compile that bytecode into machine code.

The basic idea is to compile only the code that gets run a lot. The code "speeds up" as it runs, compiling as it goes. The subroutine can even run in a different thread.

A disadvantage is that the you need to write a subroutine for every machine the code will be run on because machine code is machine-dependent.

**Then why bother with traditional compilers like those for C/C++?**

One catch is, who's going to write the subroutine. Another catch is, who's going to run that code.

JIT interpreters thus have some software overhead because they need to embed a compiler like GCC inside themselves.


# GCC Internals


## Compiler Portability

There are three "platforms" of concern, with specific vocabulary:

(1) **"Host":** What platforms can GCC **run** on?

x86-64, ARM, RISC-V, etc.

(2) **"Target":** What platforms can GCC generate code **for**?

You can run GCC on one architecture to generate code for another platform.

(3) **"Build":** What platforms do you **build** GCC itself on?

---

If one wanted to build Emacs/Python, one needs to write source code in C that then translates it to machine code.

At the lower level, GCC can compile itself. The most popular compiler to build GCC is GCC itself.

**Chicken or the egg? Circularity?**

The first GCC compiler was compiled with an earlier C compiler, which in turn was compiled with assembly language. If you keep going back in time, people bootstrapped from machine code, working at the level of opcodes while toggling physical switches.

## GCC Target Assumptions

- Flat address space, where every pointer is the same width and can address any part of memory
- Support for 8-bit bytes and 16-bit words. 32-bit, 64-bit, etc. are technically not required.


## GCC Source Code

You want to modularize your compiler. You don't want to have a separate compiler for each platform. The GCC source code can be split up into machine-independent and machine-dependent parts. The latter would have separate modules for RISC-V, ARM, x86-64, etc. The majority of the code is shared code that can be consulted on any architecture.

There's also the concern of language support. GCC can compile C, C++, Java etc., so within the machine-independent part of the code, a portion is modularized into language-dependent code. 

The machine-independent, language-independent part of the source code can be thought of as the "heart"/"core" of GCC. Ideally, you want as much of the source code as possible to be in this category.

```
+------+-------+--------+
|  C   |       | RISC-V |
+------+  GCC  +--------+
| C++  | heart |  ARM   |
+------+       +--------+
| Java |       | x86-64 |
+------+-------+--------+
^              ^        ^
|--------------|--------|
(machine-indep) (machine-dep)
```


### Files

The heart of GCC are a bunch of `.cc` files:

```
foo.cc
fob.cc
...
```

Then there a bunch of **machine description files**:

```
x86-64.md
arm.md
...
```

These describe the machine's registers, instructions that operate on the registers, what the instructions do, etc.
The descriptions are Lisp-ish but also kind of a hybrid between ASM and C++.

For each platform, there's a `.cc` file like `x86-64.cc` that in effect *implements* the machine description so that the heart of GCC can consult the classes/methods defined by the `.cc` to figure out some hardware details.

The `.cc` specifies a bunch of subroutines to be written. There's a program written by the GCC maintainers that translates the `.md` files to `.cc` files.

This is an example of **project-specific tooling**.
