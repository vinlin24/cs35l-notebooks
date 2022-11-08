<!-- week7.md -->

**Table of Contents**

- [Version Control (Continued)](#version-control-continued)
  - [Merging (Continued)](#merging-continued)
  - [Rebasing](#rebasing)
  - [Stashing](#stashing)
  - [Bisecting](#bisecting)
- [Build Tools](#build-tools)
  - [No Build Tools?](#no-build-tools)
  - [Make and Makefiles](#make-and-makefiles)
    - [Common Mistakes](#common-mistakes)
    - [Parallel Makes](#parallel-makes)
  - [Building Makefiles](#building-makefiles)
    - [Autoconf and Automake](#autoconf-and-automake)
- [C Programming](#c-programming)
  - [Missing C++ Features](#missing-c-features)

---


# Version Control (Continued)

There are three major ways to combine multiple lines of development, **merging**, **rebasing**, and **stashing**.


## Merging (Continued)

Suppose you want to merge a commit tagged `my` with the `main` branch tip.

One may be tempted to just use this as an alternative to merging:

```shell
git diff main..my
```

This sounds about right on paper, but the changes compared would include prior commits that aren't shared by both commits.

Instead, we would want to compare at the point of common ancestry, say the commit tagged `common`:

```shell
git diff common..my
```

This is the basis of the first method we will discuss, **merging**.

Merging creates a commit.


## Rebasing

Alternatively, one can **rebase** a commit onto another branch. This takes away the problem where reviewers have to worry about common ancestry and a bunch of diffs. They only need to examine a linear history.

```
( )<--(common)<--( )<--( )<--(main)
          |
          +--( )<--(my)
          Δ1     Δ2
```

Often times, the main branch undergoes complex changes since the branch last diverged, so it may be easier to just move the commits of the merging branch right to the tip of the branch it's merging into.

```
( )<--(common)<--( )<--( )<--(main)
                               |
                               +--( )<--(my)
                               Δ'1    Δ'2
```

Git does this by taking the deltas Δ1 and Δ2 and then combining them with changes at `main` since `common` to compute new deltas Δ'1 and Δ'2. From the user's perspective, it's like we plucked off the `my` branch and attached it to the tip of `main`.

The **downside** of rebasing is that it requires more commits. `N` commits on the branch to merge rebases `N` commits.

Another downside is that you may repeatedly rebase if progress resumes on the `main` branch while review was still pending.

The final picture is a more linear, manually perfected history. This is a point of philosophical concern. Some may see this as an upside because it makes it easier to review, but others may see rewriting history as a bad thing.

> **ASIDE:** Often times, project managers use Git history to see which programmers are better and who deserve bonuses. This is not always the best approach. The history does not tell you the full story. There is a very tenuous relation between number of commits and the value of a programmer to a project. A glaring example at this time of writing is the mass layoffs at Twitter where *genius* Elon Musk thought to fire people who have written the least lines of code.

> The *essence* of Git and any version control system is that you are editing *changes* to source code, not the source code itself. Understanding this is what sets a **software developer** apart from an ordinary "programmer".

<!-- TODO: might need to brush up more on merging and rebasing -->

> If you find yourself in an environment where people are lying on purpose about commits in order to improve their job prospects or something, you're in the wrong company. - Dr. Eggert


## Stashing

Rebasing is less "formal" than merging. Changes are looser, but are recorded as a sequence of commit nonetheless. Stashing is even less formal than rebasing. Changes are only floating around in your repository, waiting to be reapplied.

The scenario looks something like:

1. You're working on the next change in your branch.
2. You want to switch to some other branch NOW. You *could* commit what you have right now, but that is bad practice because you're essentially committing junk. "You want your repository to be in good shape at all times."
3. Instead, you could save your changes in an external file:

   ```shell
   git diff > mywork.diff
   git checkout -f  # Discard working files
   ```

4. Checkout to the other branch and do work on it

  ```shell
  git checkout main
  ```

5. Checkout back your original branch and patch it.

  ```shell
  patch < mywork.diff -r1
  ```

Git actually provides a way to do this within Git itself, using the `stash` command. At step 3, you would do something like:

```shell
git stash push
```

This saves the state of your working files in some part of the index. When you want to retrieve this state, you can get it from the stash stack with:

```shell
git stash apply
```


## Bisecting

Suppose you have a linear piece of history where somewhere between a stable version and the most recent commit, something went wrong. You can think of this problem of finding the first faulty commit as partitioning the timeline into OK and NG ("not good") sections, hence *bisecting*.

The timeline is "sorted" in that if you think of OK=0 and NG=1, the history will always be such that all NGs follow OKs.

```
                    |
(v4.3)<--( )<--( )<---( )<--( )<--(main)
OK        OK    OK  |  NG    NG     NG
                    |
```

This then becomes a classic *binary search* problem, where we can identify the first NG commit in O(logN) time.

Starting a bisect in Git:

```shell
#                 NG   OK
git bisect start HEAD v4.3
```

Then we tell Git to run your check script on each commit and use the exit status to determine if the commit is OK or NG:

```shell
#              vvvvvvvvvv any shell command
git bisect run make check
```

In this case, we use a Makefile with a `check` target that defines some test cases for the program

Of course, this also introduces the problem that if your test cases are buggy, then you may get false alarms. If you know ahead of time that a commit, say `v3`, will produce unreliable test results, you can skip it with:

```shell
git bisect skip v3
```


# Build Tools

Audiences for build tools:

- **Coders:** These people write the actual source code.
- **Builders:** These people are in charge of assembling the pieces of the program into a coherent application. They "run make".
- **Distributors:** These people take the built products and ship them out to the users. They "run Debian".
- **Installers:** These people take the output of the distributors and install them on the machines.
- **Testers:** These people try to find problems in the code or any of the steps after it like distribution and installation. These are often done by people lower in the chain, like interns.
- **Users:** These people are the intended consumers of the product.


## No Build Tools?

You write your own project-specific build tools. Suppose you have a simple example of multiple C files that you need to link together. You could write a build script `build.sh`:

```shell
gcc -c x.c
gcc -c y.c
gcc -c z.c
gcc x.o y.o z.o -o foo
```

> Your goal as a software developer is to automate yourself out of a job. You constantly want to take the things that you're doing and automate it.


## Make and Makefiles

A simple build tool intended to be a level up from a shell script is **Make**. This was motivated from scripts taking too long to execute. They were inefficient because they would compile the entire program even if only one file was edited, so it does a lot of unnecessary work.

Thus, the essence of the **Makefile** is that you write a set of **recipes** in it, each of which instructing how to build a file. When you run `make`, the program does the *minimum amount of work* necessary to build the file. It does this with the concept of **prerequisites**:

```makefile
CC = gcc
x.o: x.c
  $(CC) -c x.c
y.o: y.c
  $(CC) -c y.c
z.o: z.c
  $(CC) -c z.c
foo: x.o y.o z.o
  $(CC) $< -o $@
  # Escaping the $ so the shell can see it
  echo $$PATH
```

Make is smart enough to see the dependencies (prerequisites) and avoid doing work that has already been done. When you run:

```shell
make foo
```

It looks at the timestamp of `foo` and the timestamps of its prerequisites, `x.o`, `y.o`, `z.o`. to determine what files have changed since last run (last modified time of each file). If a file is up-to-date, it doesn't need to touch it.

Make is like a hybrid language. The `target: prerequisites` lines are its own "Makefile" language. The recipes are in the shell language, command that you would write at the terminal.

You can run a specific rule to run by specifying the target at the command line:

```shell
make z.o
```

You can override macros at the command line with similar `=` syntax:

```shell
make CC=clang
```


### Common Mistakes

Often times, bugs arise from specifying the *dependencies* of the targets.

A *missing* dependency means the product may be wrong. Make will falsely assume its job is done if it does not know if needs to act on a certain file.

An *extraneous* dependency is a more benign mistake. Files are still compiled the same way as before, but it may cause Make to do extra work.


### Parallel Makes

Suppose you have a big project.

```
project/
  Makefile
  sub1/
    Makefile
  sub2/
    Makefile
  sub3/
    Makefile
```

A common approach is to have one big Makefile at the project root will all the rules, including tests for anything under subdirectories.

Another approach is to have Makefiles in each subdirectory. In this case, the root Makefile *delegate* work to the subsidiary Makefiles:

```makefile
test:
  # not sure if this is right syntax
  for i in a b c; do; (cd $$ && make); done
```

The **upside** is that it modularizes the build process. The **downside** is that the parent Makefile is **sequential** code, and you don't have the opportunity to run multiple rules in parallel.

Thus, nowadays, the single large Makefile is the more common pattern. It is a performance bonus, and it is usually automatically generated anyway.

There is however a problem with parallel Makes. Extraneous/incorrect dependencies may slow down the process because there's less opportunity for parallelism. A missing dependency is even worse because they can cause different builds affecting different files, resulting in crashes.


## Building Makefiles

Suppose you have code that you want to tailor to a certain implementation. In this particular example, the C header `<stdckdint.h>` is not on SEASnet yet, but suppose you want to be able to compile code that uses it anyway.

A common convention is to have a shell script `./configure`, which *generates* Makefiles from a template, similar to the concept of a *compiler*.

```shell
# This syntax here is called a heredoc
cat > checkfile.c << EOF
<-- code that uses the feature -->
EOF

if gcc checkfile.c; then
  echo "#define HAVE_STDCKDINT_H 1" >> config.h
fi

# Yo I'm not sure I caught everything and/or
# understand everything from the whiteboard
```

Then in your source code:

```c
#include <config.h>
int main(void)
{
  #if HAVE_STDCKDINT_H
    use the features for stdckdint.h
  #endif
}
```

Then you define the `builder` script to first *make* the Makefile and cause other configuration side effects, and then run the Makefile:

```shell
./configure
make
```

### Autoconf and Automake

**Autoconf** is a program that generates `./configure` files. Just as how Makefiles are automatically generated by `./configure`, the `./configure` file is generated by Autoconf. We see that build tools often build on top of each other, each generated by the next. This is a common pattern in software construction.

**Automake** is like a front-end abstraction for Autoconf, so like yet another level up from Autoconf.


# C Programming


## Missing C++ Features

- Classes, objects, inheritance, polymorphism, etc.
- Structs having static data members and functions. Structs in C are strictly collections of data members (plain old data).
- Namespace control. C has a single top-level namespace.
- Overloading. There is a feature called `_Generic` that attempts a way around this, but this is esoteric.
- Exception handling. C has a system with `<setjump.h>` but it has all sort of trouble.
- Memory allocation. There is no `new` operator in C. Instead, you use the *library functions* `malloc()` and `free()`; they aren't built into the language.
- No `cin`/`cout`. You use IO with the library functions under the `<stdio.h>` header.
