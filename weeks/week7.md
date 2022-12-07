<!-- Week 7 Lecture Notes -->

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
  - [Namespace Visibility](#namespace-visibility)
- [Low Level Debugging](#low-level-debugging)
  - [Memory Allocation](#memory-allocation)
    - [Static Allocation](#static-allocation)
    - [Stack Allocation](#stack-allocation)
    - [Heap Allocation](#heap-allocation)
  - [Pointer Dangers](#pointer-dangers)
    - [Why Deal with malloc and Pointers?](#why-deal-with-malloc-and-pointers)
    - [Common Problems](#common-problems)
- [Security Improvement](#security-improvement)
- [Performance Improvement](#performance-improvement)
  - [GCC-Compatible Source Code Techniques](#gcc-compatible-source-code-techniques)
    - [Optimizing a Case Away](#optimizing-a-case-away)
    - [Maximizing Cache Efficiency](#maximizing-cache-efficiency)
  - [GCC Options](#gcc-options)
    - [Automating Hot/Cold with Profiling](#automating-hotcold-with-profiling)
    - [Inlining and Link-Time Optimization](#inlining-and-link-time-optimization)
- [Static Checking](#static-checking)

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


## Namespace Visibility

C has a very primitive version of namespace visibility compared to C++, and it does so with the keywords `static` and `extern`.

In C, **static allocation** is completely different from the `static` keyword. `static` defines a variable with **static lifetime**. This means the variable will have **internal linkage** (the identifier is visible only in the source file it is in).

Variables defined without `static` like `int x;` have **external linkage**, meaning another fine is allowed to declare it. They would do so with the `extern` keyword:

```c
// s.c
int x;
static int a[1000];
```

```c
// t.c
extern int x;
```

The two `x`'s are one and the same. On the other hand, there is no way for `t.c` to access the identifier the array named `a` (by *identifier*; if the array is passed by reference to a function, the memory can still be accessed).

**TL;DR:** `static` is like "private". If you want your code to be *modular*, you can use `static` to define symbols as part of a private API that other parts of the code need not worry about. If you prefer the approach where all parts of the program know about every other part, you can avoid using `static` afltogether.

C gets pretty hairy when it comes to linking. Suppose you try to define a variable with the same name in yet another source file:

```c
// v.c
int x;
```

Depending on the flags used, you would either get (1) and error or (2) the situation where the x's refer to the same variable.

Every file that `#include`s a file gets their own private copy of every `static` symbol. This is usually the cause of "defined but never used" warnings.


# Low Level Debugging


## Memory Allocation

There are 3 main allocation strategies:

1. **Static:** exists before the program starts and will always exist until the program terminates.
2. **Auto:** exists on a special segment of memory called the **stack**. LIFO policy makes it cheap and simple to push and pop values as needed.
3. **Dynamic:** exists on a special part(s) of memory called the **heap**. This lets you request memory at runtime instead of at compile-time using a memory manager implementation like `malloc`.


### Static Allocation

```c
// Statically allocate an integer
int x;

// Statically allocate an array of 1000 integers
int a[1000];

// Statically allocate an array of 1000 integers
// The 'static' keyword just modifies the identifier's visibility
static int b[1000];
```

There's a performance/lifetime argument against static allocation. Typically, a program runs faster if you use stack allocation instead of static allocation (where a part of memory is always allocated for something for the entire duration of the program).


### Stack Allocation

Very cheap. The behavior of the stack allocation can be implemented at the *machine level*:

```c
int f(int n) {
  char buf[512]; // allocate 512B on the stack
  buf[0] = n;
}
```

```as
subq $512,%rsp
; ...
addq $512,%rsp
```


### Heap Allocation

In C, you typically use the `malloc` library function defined under that `<stdlib.h>` header to dynamically allocate data:

```c
// p is a pointer to a block of contiguous memory
// that is the size of 5 ints
int *p = (int *)malloc(5 * sizeof(int));

/* code that uses that memory */

// De-allocate that memory when finished
free(p);
```

The special type `void *` refers to a **generic pointer**. You are allowed to cast any pointer type to a void pointer. Void pointers give you a lot of freedom, at the cost of checking. For example, attempting to return something dereferenced by the pointer is an error because C does not know what type it is:

```c
int bad_stuff(int *p) {
   void *q = (void *)p;
   return *q; // bad!
   return *((int *)q); // you gotta give it a type
}
```

You can think of a void pointer as the most free type of pointer. Its value is just the value of the memory address and nothing more. Attempting to use increment/decrement operators would modify it by a unit's worth of memory (typically 1 byte). I guess this could be useful when you know something meaningful is at a memory address, but do not know the type associated with it. An example of this is the `malloc` function, which returns a pointer to a chunk of memory you allocated, but it doesn't care what type you treat that memory as, so it returns a generic `void *`.


## Pointer Dangers

Of course, the fact that you must `free()` your memory after using it means that using heap allocation is potentially dangerous. It's much easier to get **memory leaks** and/or **segmentation faults** with improper use of pointers and memory management.

Once you have a pointer to something that doesn't exist anymore (**dangling pointer**), you cannot look at the pointer. That leads to **undefined behavior**. If you're lucky, the program will crash. If you're unlucky, the error passes silently and possibly messes with memory that isn't related to the operation.


### Why Deal with malloc and Pointers?

Suppose we just *pre-allocate* all objects statically and hand them out as the program runs:

```c
obj_t table[10000];
```

Firstly, how do you know how many objects to allocate? What if you run out?

Secondly, what if you never need that much memory? Your code would be bloated, consuming more resources than it needs to. Program startup would also be slower.

Thus, we use the heap to *dynamically* allocate resources *as needed*.


### Common Problems

**Dangling pointers**

The pattern looks something like:

```c
p = malloc(5); // allocate
free(p); // free
*p; // using freed memory!
```

**Memory leaks**

The pattern looks something like:

```c
p = malloc(5); // allocate
*p; // use
// but never freed!
```

Memory leaks are notoriously hard to trace. To catch them more reliably than with plain eyes, you use tools or compiler options.


# Security Improvement

Attempt to protect against **stack overflow attacks**:

```shell
gcc -fstack-protector
```

This generates extra code in your function. It is as if you added something like:

```c
int f() {
  // added check
  int i = randomvalue; // some macro probably

  char buf[512];
  read(fd, buf, 512);
  x = buf[12];

  // added check
  if (i != randomvalue)
    abort(); // reliably crash, dump core

  return x;
}
```

This means that attackers would have to guess the random value, meaning they'd have to guess from the possible values `int` can take on, and even more if something like `long` is used.

This works because the attacker would have to overwrite the `randomvalue` to get to the bottom of the call frame to reach the return address, which is typically what they need to overwrite to continue their attack.

There is debate whether `-fstack-protector` is a default option. Some Linux distros like Debian and Ubuntu have it on by default. This is off by default on SEASnet. There's also the opposite option called `-fno-stack-protector` that turns the setting off.

> **ASIDE:** The **halting problem** states that there is no algorithm for looking at a program and figuring out if it would halt. There's no way to look at a program to figure out if it would stack overflow (Alan Turing).


# Performance Improvement

The `-O`, `-O2`, `-O3`, etc. flags to specify the level of *optimization for CPU usage*.

```shell
gcc -O foo.c
```

This is off by default because it slows down the compiler. The compiler spends more time finding ways to optimize the machine code to make the *runtime performance* faster. It's a trade off between compile time and runtime performance.

Additionally, the higher the level of optimization, the harder it becomes to debug the machine code because the compiler may choose to take certain liberties for the sake of making the executable more performant, making the machine code less parallel with the original source code.

By default, it cares about runtime performance, not executable size. To *optimize for size*, you use the `-Os` flag.

There is another optimization option that is still under construction `-Og`. This means to *optimize for debugging* i.e. make the code as "debuggable" as possible without affecting performance.

The `-O0` flag specifies to *not optimize at all*.

---

**ASIDE:** Consider this case where turning optimization on breaks the code:

```c
double *p;
*--p = *p++ * *p++;
// "pop pop multiply push"
```

If you have competing side effects in the same statement, this is *undefined behavior*. The compiler is allowed to perform the actions in whatever order.

The compiler "activated" the bug by relying on undefined behavior in the name of optimization.

---


## GCC-Compatible Source Code Techniques


### Optimizing a Case Away

In GCC, there's a built-in function called `__builtin_unreachable()`. Your code should never call this function. The purpose of this is to mark a certain condition in your code as something that will never happen, so the compiler can optimize it away.

**ASIDE:** Upcoming version **C23** has the function `unreachable()` which means the exact same, and this can be used in any compiler.

```c
int sqrt(int n) {
  // Suppose you know by precondition n is non-negative
  if (n < 0)
    __builtin_unreachable();
  /* code */
}
```

This is a message from you, the developer, to the compiler telling it to never worry about the case of `n` being negative. This unlocks some optimization techniques, like something like `n >> 3` for division, which wouldn't work for negative numbers. Of course, if you don't ensure the precondition or abuse `__builtin_unreachable()`, you could cause undefined behavior. **The compiler trusts you know what you're doing.**

Notice how there's no need for conditional branching that could slow down the code. You use an `if` statement, but the compiler optimize it away because of what it means. No code gets generated for that block.

You could also transform this into a macro:

```c
#define assume(x) (x ? 0 : __builtin_unreachable())
```


### Maximizing Cache Efficiency

A technique for fitting variables into *cache lines* to maximize computations per cycle:

```c
// This means allocate x at a multiple of 16
// Arg to aligned() must be a power of 2
long x __attribute__((aligned(16)));
```

You can also mark functions based on how often they are likely to be called at runtime:

```c
// This function likely won't be called
int f(int) __attribute__((cold));
// The function likely WILL be called
int g(int) __attribute__((hot));
```

We want to divide code into two regions, "hot" and "cold". We want to make the hot region small, consisting of code we will execute a lot. Cold can be as big as we want. The main point is that the code in the hot region will be *cached*. Cold code would sit in DRAM. This makes efficient use of our **instruction cache**.


## GCC Options


### Automating Hot/Cold with Profiling

1. You compile the program with a special flag that inserts extra code to count the number of times each instruction is executed. This understandably slows down execution.
2. You then run the program to gather the statistics.
3. Then, you recompile the program *with* the statistics. That way, the compiler knows which instructions are hot and cold and can decided how to mark them itself.

As long as your test runs are representative of how your program will actually be run, this is a great way of improving the performance of your application.

---

**Other GCC options**

```shell
gcc --coverage
gprof
```

---


### Inlining and Link-Time Optimization

<!-- TODO: group with other GCC options -->

```shell
gcc -flto
```

Normally, GCC optimizes one module at a time.

```c
// m.c
static int f(int x) {
  return -x;
}
```

Then suppose in the same module you call:

```c
// m.c
f(3);
```

The compiler can deduce at compile-time that `f(3)` will always resolve to `-3`, so it can just expand it to `-3`. This is called **inlining**.

Then suppose you have definition and call in different modules:


```c
// m.c
int f(int x) {
  return -x;
}
```

```c
// n.c
extern int f(int x);
```

```c
// n.c
f(3);
```

By default, the compiler can no longer make this expansion. It instead has to go through the overhead of a normal function call: putting `$3` into a register, etc.

The point of `gcc -flto` is to instruct the compiler to perform the optimization *across module boundaries* - whole program optimization.

```shell
gcc -flto foo.c bar.c baz.c
```

This creates a `foo.o`, `bar.o`, etc. that contains machine code, but within the file is also a copy of the source code. Thus, by the time you link them together:

```shell
gcc -flto foo.o bar.o baz.o
```

GCC has a copy of the entire source code, so it can perform the inline optimization to generate better code. The *downside* is that this command takes a long time. The optimization algorithms that GCC uses are around $O(N^2)$. It could take literal days with a large program.


# Static Checking

Debugging your program *before* the program runs. "Static" here has nothing to do with static allocation/static lifetime; it just means when the program isn't running. This is the opposite of **dynamic checking**.

An example of dynamic checking, where you check at *runtime* and abort or similar if something fails:

```c
#include <assert.h>
int f() {
  /* code */
  assert(0 <= n);
}
```

Static checking:

```c
static_assert(0 <= n);
```

The implication however is that the expression e.g. `0 <= n` MUST be computable at compile-time. If it isn't, it's a compiler error.

**EXAMPLE:** Suppose you have a program that assumes a `long` is 64 bits:

```c
#include <limits.h>
static_assert(LONG_MAX >> 31 >> 31 == 1);
```

This is once again like a *message from you to the compiler*. The compiler then checks beforehand for this condition before attempting to compile the program.


<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({ tex2jax: {inlineMath: [['$', '$']]}, messageStyle: "none" });
</script>
