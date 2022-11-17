<!-- TODO: These notes aren't organized yet. -->

# Low Level Debugging (Continued)


## Static Checking (Continued)

Advantages:
- No runtime overhead
- Covered bugs are 100% prevented by the time the program runs

Disadvantages:
- Some bugs can not be statically checked for 100% reliably


### GCC Useful Checks

In general, the flags beginning with `W` mean "warning". `-Wall` means "turn on all warnings":

```shell
gcc -Wall
```

However, people found that this tends to output extraneous/unnecessary warnings. Nowadays, `-Wall` has come to mean "turn on all warnings that most people will find useful."

`-Wall` implies:

- `-Wcomment`: warn about valid but bad comments like `/* a /* bad comment */`
- `-Wparentheses`: warn about style considered to be bad because something probably forgot parentheses in situations where operator precedence may be less familiar, like in  `i << j + k` or `i < j || k < l && m < n`. The latter is common knowledge, but GCC disagrees, so this flag can be controversial.
- `-Waddress`: warn about making pointer comparisons when you probably didn't mean to, like `p = strchr("ab", "b"); if (p == "b") ...`
- `-Wstrict-aliasing`: warn about trying to "cheat" with pointers.
  ```c
  int i;
  long *p = (long *)&i;
  ```
  The C/C++ standard states that this results in undefined behavior, even if `int` and `long` happen to be the same size. This is controversial because a lot of low-level programs like the Linux kernel uses this all the time, *very carefully*. This is also why **casting** in general is risky because optimizing compilers may not do what you expect.
- `-Wmaybe-uninitialized`: warn about possible paths through a function where you use an uninitialized variable.
  ```c
  int f(int i)
  {
    int n;
    if (i < 0)
      n = -i;
    return n; // if i >= 0, uninitialized n is returned
  }
  ```
  If you do something that checks out with arithmetic reasoning, the compiler may or may not be find with it, depending on other flags you pass it.
  ```c
  int f(int i)
  {
    int n;
    if (i < 100)
      n = -1;
    if(i <= -1)
      return n;
  }
  ```
  Thus, there could be false positives if the compiler is not smart enough to deduce that the combination of things you've done always returns an initialized variable.


### GCC Extra Checks

`-Wextra` is a collection of warnings like `-Wall` that are more controversial/less useful.

Some flags it includes are:

`-Wtype-limits`: warn about comparisons where the answer is obvious due to the type.

```c
unsigned u;
if (u < 0)
  /* This code would never run! */
```

This is not always trivial. For example, if you're trying to run portable code, some `typedef`s might be different:

```c
#include <time.h>
time_t t;  // could be signed or unsigned
if (t < 0)
  /* This code may or may not run! */
```

In recent versions of GCC:

```shell
gcc -fanalyzer
```

This turns on **interprocedural warnings**. This looks at all callers and callees of a function to come up with a better picture of, say, if a variable is uninitialized or not. In other words, it looks through all *all paths through all calls* of every function (in current .c file, as that is what is deducible at compile-time). This is disabled by default because it slows the compiler down.

Specifying both of these flags would theoretically be rewarding but very expensive:

```shell
gcc -fanalyzer -flto
```


### Helping the Compiler with Source Code

You can modify your source code in minor ways to help the compiler do better checking.

```c
// Prototype for some function that will never return
_Noreturn void fatal_error(char const *);

size_t size_sum(size_t a, size_t b)
{
  if (a <= SIZE_MAX - b)
    return a + b;
  fatal_error("size overflow");
}
```

Without the special `_Noreturn` signature, GCC will complain because it thinks size_sum is not always returning `size_t` even though it always does because `fatal_error` exits the program.

This will be standardized in C23 as `[[noreturn]]`.

`_Noreturn` also lets the compiler check whether a function declared to not return actually doesn't:

```c
void fatal_error(char const *x)
{
  puts(x);
  exit(1);
}
```

`exit` is a `<stdlib.h>` function declared as something like:

```c
_Noreturn void exit(int);
```

So `fatal_error` as it is defined above is fine. If we were to leave off `exit(1)`, then GCC would know that something is wrong.

---

With GCC, you can declare a **pure function** with the signature, telling the compiler that this function should not modify the state of the machine:

```c
int hash(char *buf, size_t bufsize) __attribute__ ((pure));
```

This is stronger than merely declaring the parameters as `const`. A pointer to `const` just promises to not modify an object via its pointer, not that its a pointer to an actual constant.

This way, if you do something like:

```c
char buf[100];
char c = buf[0];
i = hash(buf, sizeof buf);
// c == buf[0] must be true
// This gives the compiler opportunities to cache in register, etc.
```

This will be standardized in C23 as `[[reproducible]]`;

---

```c
int square(int) __attribute__ ((const));
```

This is an even stronger flavor of pure functions. The return value can not even depend on the current state of the function, only its arguments.

A benefit is that if you call a `const` function multiple times with the same arguments, it could optimize knowing they would return the same value.

This will be standardized in C23 as `[[unsequenced]]`.

---

An example with multiple attributes:

```c
// ... is a syntax feature for variable number of arguments
int my_printf(char const *fmt, ...) __attribute__
  ((nonnull(1) | format(printf, 1, 2)));
  // nonnull(1) means the first argument cannot be NULL
  // The format attribute does some type checking inside fmt arg:
  // my_printf("a=%s", 1024); // not okay!
```

The names inside the `__attribute__` label, like `nonnull` and `format`, are not actual functions. They are *keywords* to the `__attribute__` syntax.


## Dynamic (Runtime) Checking

Checks you put in yourself. This is very flexible because it's your own code.

```c
#include <stdckint.h> // C23 only
if (ckd_add(r, a, b))
  return EOVERFLOW;
// Not sure what this is but yeah
```


### GCC -fsanitize

Alternatively, you can let the compiler *insert* runtime checking.

```shell
gcc -fsanitize=undefined
```

Generate extra code to make the program reliably crash if it attempts undefined behavior (except for addresses). This causes the program to be slightly slower.

At the machine level, the adding overflow may be implemented like:

```assembly
addl %eax,%ebx
jo   ouch       ; jump if overflow

ouch:
    call abort
```

This is the same as `-fsanitize=undefined`, but for addressing errors, such as subscripting an array. Due to technical reasons, this actually only catches *most* addressing errors, not all.

```shell
gcc -fsanitize=addresss
```

This attempts to insert runtime checks for **memory leaks**. Memory leaks are technically not undefined behavior nor errors.

```shell
gcc -fsanitize=leak
```

This attempts to insert runtime checks for **race conditions**, where there may be multiple threads that attempt to access the same I/O resource.

```
gcc -fsanitize=thread
```

Most of the time, these flags are useful for development only. They could be left in for production if the application prioritizes safety over performance.


## Valgrind

You can run Valgrind on any program, no special compilation flags:

```shell
valgrind emacs
```

This runs the program in a special environment and allows for more runtime checking.

The summary screen displays information about memory leaks.

**UPSIDE:** No special flags needed when compiling
**DOWNSIDES:** Much slower. Checking is also less extensive because Valgrind only has access to the machine code and not the underlying source code.

> **BIG POINT:** Runtime checking is dicey. Even with all these tools, bugs can still slip through.

**ASIDE:** You can manually raise a compiler error with preproccesor directives in C:

```c
#if INT_SIZE == LONG_SIZE
  #error "ouch"
#endif
```


## Portability Checking

32 vs. 64-bit platforms?

```shell
gcc       # default is 64-bit
gcc -m32  # check for 32-bit
```

Linux vs. MacOS vs. Androids vs. ...?

Chromium vs. FIrefox vs. ...? (JS)

Chromium v102 vs. v107 vs ...?

> **Portability is a large part of software construction issues.**

You need a good strategy to tackle this. One strategy is to have a comprehensive testing system. But you also need to consider the source code.

The way it's done is defining a **portability layer**, a level of abstraction:

```
main code
    |
(some API)
    |
portability module
    |
Chromium, etc.
```

> The compiler should be your servant, not your master. If you get warnings, look at what they're actually saying. If they're false positive, shut them off. - Dr. Eggert


# Debugging Strategies

1. You're better off NOT debugging. Often times projects are stuck in **debugging hell**, spending more time debugging than actually developing. Debugging is an inefficient way of finding and fixing bugs. If you're spending a lot of time debugging, you should probably change your software construction approach such that you don't get so many bugs. *Be proactive* e.g. use static checking!
2. Write test cases. **Test-driven development (TDD)** is a theory of development that states that test cases are higher priority than code; one should write test cases first, the idea being you can use the test cases to debug the specifications of the code before actually writing the code.
3. Use a better platform.
4. **Defensive programming**. "When you're driving, assume that everyone is an idiot, drunk or both." Assume the other modules are broken.
   1. You can use traces and logs (`print()` statements!).
   2. Checkpoint/restart. Have one primitive in your program that saves the state of your entire application, probably to some file where it persists. Then have another primitive that restores the state:
    ```python
    save_state("foo")  # do this periodically
    restore_state("foo")
    ```
    3. Assertions: crash when something that should never happen occurs.
    4. Exception handling.
    5. **Barricades**.
    ```
    +----------------+----------------+
    |    possibly    |   clean data   |
    |    bad code    |    structs     |
    +----------------+----------------+
                     ^
         some barricade that processes
         code from the outide world, which
         you assume to be bad, before it
         makes it into your code
    ```
    6. **Interpreters** (such as Valgrind) that execute code in a special environment.
    7. **Virtual machines** that run a program in its own special sandbox, isolated from the outside system.


> **TDD Aside:** If you write some test cases, and your program passes all the test cases, then you screwed up because you haven't found the bug you wanted to find. When you're writing test cases, you're trying to be imperfect. You're trying to think "how do I make this program crash?" Often times, tests are written by another class of developers because the self-interest of coders causes them to write bad test cases. - Dr. Eggert


# Debugging Tools

Some examples are **GDB**, the GNU debugger, and **Valgrind**.

**strace** is a command that outputs the system calls a program uses, as if there were many `printf` logging calls in the source code.

```shell
#      vvvvvvvvvvvvvvvvvvv any shell command
strace cat /etc/os-release
```

Other commands we've seen before like `ps` and `top` are also *DevOps tools*. System administrators use this to monitor server activity, but they can also be useful for debugging.


## How Debuggers Work

GDB is actually a separate process from the process being debugged. It uses special system calls to exert some control over the process being debugged. Model:

```
(gdb process)--+
               | special system calls
               v
        (your process)
```

These special system calls include, at any point in execution:

- Starting/stopping/continuing
- Accessing memory
- Accessing registers

These do have security restrictions and cannot be used arbitrarily. These restrictions depend on the OS, but typically the rule is that the debugger must control a process with *same user ID*. Other OS may have a more restrictive rule, stating it can only debug a *child process*.


## Getting Started with Debugging

You can use a `-g` flag to specify debug info level for a C program:

```shell
gcc -g3 program.c
```

This conflicts with optimization because code that is optimized by the compiler tends to become harder to understand.

```shell
gcc -g3 -O2 program.c
```

This typically results in **inlining**, where calls to functions can be optimized away by substituting their body into places where it was called. The produced machine code would then be functionally equivalent but have lost the information that a function was even called.

What the `-g` flag does is bloat the resulting object and executable files with debugging tables. This data isn't visible when the program runs normally, but debuggers will be able to access them.

**ASIDE:** `_FORTIFY_SOURCE` is a standard technique used by GCC to make stack overflows less likely to succeed. For technical reasons, this is incompatible with no optimization i.e. attempting `gcc -O0` will cause a compiler error.

Dropping a program into GDB:

```shell
#   vvvv program to debug
gdb diff
```


## GDB Commands

Setting the working directory of the program when it starts up:

```console
(gdb) set cwd /etc
```

Setting environment variables for the debugging session:

```console
(gdb) set env TZ America/New_York
```

A defense technique against buffer overflow attacks is to have the program run at randomized locations in memory (CS 33). By default, Linux executes programs in an environment with randomized addresses for the stack, heap, C library, etc. and many even the `main()` function.

The downside of this program is that it will run differently every time. This means that if there's a bug that depends on stack addresses for example, then it may appear sometimes and not for others. This makes debugging harder, so by default, this option is already on:

```console
(gdb) set disable_randomization on
```

Actually running the program. The arguments you supply after `run` are in shell syntax and forwarded to the executable being debugged:

```console
(gdb) run -u /etc/os-release - < /dev/null
```

*Alternatively*, you can make GDB be in charge of another program using the PID of running process, effectively suspending it.

```console
(gdb) attach 986317
```

Releasing the program:

```console
(gdb) detach
```


## Stopping Your Program

`^C` stops the program. GDB takes control.

`*(int *)0=27` crashes the program and falls under GDB's control.

Continue running the code:

```console
(gdb) # c
(gdb) continue
```

Single step through the source code. Similarly, single step through the machine code.

```
(gdb) # s
(gdb) step
(gdb) # si
(gdb) stepi
```

Stepping can be tricky because there isn't always a sequential mapping of source code lines to machine code lines. Stepping through some machine code lines may make it look like the program is jumping back and forth between source code lines instead of running one-by-one in order.

A courser-grained variant of the `step` command. Advancing to the next line of source code at the current function call level i.e. a single step but without worrying about function calls, stepping "over" them. Similarly, it has a machine code version.

```console
(gdb) # n
(gdb) next
(gdb) # ni
(gdb) nexti
```

Finish the current function and then stop:

```console
(gdb) fin
```

You can use **breakpoints** to stop the program at a certain instruction, typically a function name. Creating a breakpoint:

```console
(gdb) # b
(gdb) break analyze
(gdb) break diff.c:19
```

Listing your current breakpoints and their numbers:

```console
(gdb) info break
```

Deleting a breakpoint by number:

```console
(gdb) del 19
```

All these commands control the **instruction pointer**, `%rip` on x86-64.

You can use **watchpoints** to tell the program to run until the specified variable `p` changes value:

```console
(gdb) watch p
```

**How does GDB implement breakpoints?**

GDB takes the process being debugged and modifies its machine code. It stomps on the machine code of the specified function/line/instruction by zapping the first byte with a special instruction that is guaranteed to cause the program to trap, allowing GDB to take control.

**How does GDB implement watchpoints?**

Single step through the code, and after each instruction, see if `p` has changed. This can be really slow unless youh have special hardware support for watchpoints. Many CPUs, including x86-64, have this support.


## Other GDB Commands

Printing a C expression (or register values):

```console
(gdb) # p
(gdb) print expr
(gdb) print $rax
(gdb) print a[5]
(gdb) print cos(3.0)
```

It does more than just allow you to look at data. It lets you run a subroutine like `cos` in the program, which can modify the data and/or call arbitrary code from other parts of the program.

Disassembling a function to get the assembly code:

```console
(gdb) disas cos
```

You can set a **checkpoint** and then run the code from the checkpoint by its number:

```console
(gdb) checkpoint
(gdb) restart 42
```

The inverse of `continue` is `reverse-continue` (`rc`). This means to start running the program backwards until it hits the most recent breakpoint that it passed. This tends to be *very expensive* because GDB has to set a bunch of checkpoints under the hood.

```console
(gdb) # rc
(gdb) reverse-continue
```

For **cross-debugging**, you can specify what target you want to run in

```console
(gdb) target
```

This makes GDB run on some virtual machine or something.


# Git Internals


## Plumbing vs Porcelain

The **plumbing** part of a program is the low-level part, where the actual work is done. It is not easy to use and not meant to be easy.

The **porcelain** is the high-level commands for the end users.

Git actually violates a common software engineering rule, which is "Just show the porcelain to the outside world. Don't expose your plumbing."

Instead, Git wants to have two levels. Even at the low-level, it want to expose its basic model for how Git works if the user is willing to understand how it works.

**Why is cloning so fast on a local machine?**

If you explore your files with `ls -l`, you'll notice that the link count of some files are greater than expected.

We see that a Git clone is created quickly partly because it did not have to make copies of the objects, and rather it used **hard links**.

More specifically, it makes hard links to read-only files. It knows it's not going to cause a problem because people cannot modify them, so they're safe to share.

A consequence is that most of the stuff in `.git` is stuff you cannot change. If you could, you can no longer clone it. If you try to be a troublemaker and use `chmod` and try editing such files, it could mess up other repositories using it (if that's even allowed at all).


## The .git Directory

The entire state of the repository sits in the special `.git` directory.

There's an issue of compatibility. When a new version of Git comes out, it needs to be able to work with old repositories (**backwards compatibility**). The converse is not always true; repositories created by recent versions of Git may not necessarily work with older versions of Git. Thus, there are some files in the `.git` folder that are archaic,

Some important files and directories in the `.git` subdirectory:

- `branches` - obsolescent
- `config` - repository-specific configuration
- `description` - used for GitWeb, an attempt to put Git on the web
- `HEAD` - where the current branch is
- `hooks/*` - executable scripts that Git will invoke at certain "pressure points" (important triggers, like making a commit). By default, there are no working hooks; default ones all end with `.sample`, which illustrate what you might want to put in such hooks.
- `index` - Git's plan for the future, a list of planned changes for the next commit. This is in binary data.
- `info/exclude` - addition to `.gitignore`
- `logs` - keeps track of where the branches have been (histories of branch tip locations)
- `objects` - where the actual "repository" is, where the object database is stored
- `refs` - where the branch tips and tags are (where all the "pointers" in the repository are)
- `packed-refs` - optimized version of `refs`

**Emacs Hooks ASIDE:** There is a script in the Emacs source code called `autogen.sh`. This script creates a bunch of Git hooks that tailor the repository to be the way the Emacs developers want it to be tailored. This is a nice "gatekeeper" approach that ensures your development is relatively clean.
