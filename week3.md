**Week 3 Lecture Notes**

- [Emacs Lisp](#emacs-lisp)
  - [Namespace and Security](#namespace-and-security)
  - [Fundamental Data Structures](#fundamental-data-structures)
  - [Emacs Byte-code](#emacs-byte-code)
  - [The Emacs Interpreter](#the-emacs-interpreter)
  - [Customizing Key Binds](#customizing-key-binds)
- [Python](#python)
  - [Why Python?](#why-python)
  - [Python Internals](#python-internals)
    - [Typing](#typing)
  - [Python vs. Shell vs. Emacs Scripting](#python-vs-shell-vs-emacs-scripting)
- [Assorted Discussion Notes](#assorted-discussion-notes)

---


# Emacs Lisp

An example of **app-specific language (ASL)**. In some extent, it's an example of the *little languages philosophy*. Creators of Emacs took an existing language and mutated it to suit their particular problem.


## Namespace and Security

Emacs is very traditional - a flat, top-level namespace that can be prone to clashing. You have a lot of liberty in making problematic changes. This is allowed for example:

```lisp
emacs-version
; "28.2"
(setq emacs-version "100.0")
; "100.0"
(setq emacs-version "100.0")

; You can also mess around safely with scoped variables:
(let ((emacs-version "19.2"))
  (message "A %s" emacs-version)
  )
; "A 19.2"
```

Trying to evaluate a variable that's never been assigned, you get a *runtime error*, very reliably. There is *well-defined behavior*, as opposed to *undefined behavior* like in C/C++, the former which is obviously *safer*. The Emacs debugger also provides a traceback, but it reads bottom to top. To exit from the debugger, enter `C-]`

> Scripting languages thus tend to be used for exploratory programs, student codes, one-off programs, etc.

```lisp
(message "You can %s like printf!" "interpolate")
; "You can interpolate like printf!"
```


## Fundamental Data Structures

Emacs uses the same notation for programs and data. In other words, we use **data notation** to write our programs. The fundamental data structure in Lisp is the **list**, which is built from **cons**, which is just a pair of values. A list is singly-linked list of cons.

```
# This is a cons
[A|B]
(A . B)

# This is a list
[A| ]->[B| ]->[C| ]->[D| ]->[E|/]
(A B C D E)

# You can use . to write at the cons level
[A| ]->[B| ]->[C|D]
(A B C . D)

# The empty list; also used as the null terminator
()

# Thus this is equivalent to (A B C D E)
(A B C D E . ())

# Nested lists
[A| ]->[ | ]->[D|/]
        v
       [B| ]->[C|/]
(A (B C) D)
```

In Emacs, the *empty list* is like the *null pointer* - its byte representation is all 0s as well.


## Emacs Byte-code

You can load external source code into the current namespace with:

```
M-x load-file RET filename RET
```

As a solution to slow interpreting speed (due to the extensive use of pointers and dereferencing), ELisp uses **byte-codes** to compile data structures to create compact representations of a program.

Byte-code differs from machine code:

- **PRO:** byte-code is *portable* and works on any architecture as it is designed for some abstract machine that the Emacs application knows about.
- **CON:** Not as performant as machine code.

From the GNU documentation:

> Emacs Lisp has a compiler that translates functions written in Lisp into a special representation called byte-code that can be executed more efficiently. The compiler replaces Lisp function definitions with byte-code. When a byte-code function is called, its definition is evaluated by the byte-code interpreter.

The numbers are analogous to opcodes in true machine code. Each number represents a certain elementary operation, like pushing data onto stack memory, adding two values, etc.

For example, abstractly, this may be the compiled byte-code for some function:

```
  1 push a (arg #1)
 10 dup
 27 *
  2 push b (arg #2)
 10 dup
 27 *
 26 +
105 sqrt
```

Strung together they are functionally equivalent to the original Emacs function from which it was compiled, but now it can be compactly represented with a byte stream `1 10 27 2 10 27 26 105`.

This in turn is more performant than uncompiled Emacs code because it can be run directly by a byte-code interpreter, in contrast to high-level language that needs to be parsed (tokenized and semantically analyzed) before executing - going off of some scattered knowledge here, anyone feel free to correct me.

The byte-code files end with the `.elc` extension. You can compile a `.el` *source* file with:

```
M-x byte-compile-file RET filename.elc RET
```

`.elc` files can be loaded in the same way as `.el` files with `load-file`.

> Generally, you'd want to keep both the .el (so you can make future changes) and the .elc (so you get a performance gain) files on hand. But we're also talking about scripting in this class, and with the sizes of our scripts, this compiled/uncompiled speed difference is not noticeable (so bothering to make a .elc file in the first place is.. questionable. But it's good to know its purpose and that it's how Lisp works under the hood). **- Nik Brandt (Piazza)**


## The Emacs Interpreter

Is a *single-threaded interpreter*. There is only one instruction pointer (e.g. `%rip` on x86-64) at all times.

There's also a byte-code interpreter, which has some byte-code instruction pointer. Emacs is itself a C program, so under the hood it's something like:

```c
// points to current instruction in byte-code sequence
unsigned char *bcip;
```


## Customizing Key Binds

```lisp
(global-set-key "@" "abcxyz")
; Now typing "@" is automatically replaced with "abcxyz"

; This is like setting keyboad macros:
(global-set-key "@", 'what-cursor-position)
; Now typing "@" automatically shows cursor position in minibuffer
```

This could be a security hazard, because you can also include control characters. Emacs does provide an easy way to write control characters as strings:

```lisp
"\C-k" ; C-j
"^K"
```

Emacs however checks if you attempt to recurse like:

```lisp
(global-set-key "!" "!")
; After 0 kbd macro iterations: Lisp nesting exceeds ‘max-lisp\
; -eval-depth’
```


# Python

A **general purpose programming language** - designed for you to *write* an application of your own. It was originally designed to teach high school students.


## Why Python?

- **CONS:** Slow, memory hog.
- **PROS:** Easier to write (development cost vs. runtime cost). A lot of libraries are also written in C/C++ code for a performance bonus, made possible by **native method interfaces**.

Scripting languages like Python demonstrate an alternate balance between **development cost** and **runtime cost**. Often times, human time is much more valuable than computer time.

Prevalent in machine learning, although this feat is probably due to being at the right place at the right time. Python happened to be a reasonable scripting language for the job when the field was emerging in popularity.

> There's always going to be a time where you're the blind person next to the elephant. The goal of software construction is to make you a better blind person. **- Dr. Eggert**


## Python Internals

Python is **object-oriented**. Historically, it didn't actually start out that way. It started with functions but no classes. When classes were introduced, they implemented *methods* as functions that explicitly take the `self` first argument, which is similar to the invisible behavior in C++ OOP.

Anyway, every value is an object. Every object has:

- Identity - *cannot be changed*
- Type - *cannot be changed*
- Value - *can be changed, but only if the object is **mutable**


### Typing

In old Python, `int` used to have fixed size, so there was the distinction between integers and longs.

Main *categories* of types:
- None
- Numbers
- Sequences
- Callables

Underlying `list` allocation mechanism:

- Probably uses cache size to determine starting size
- After that, reallocation uses geometric resizing (approximately nine-eights according to [mCoding](https://www.youtube.com/watch?v=rdlQzhP71pQ))
- The total cost of calling `list.append` N times is $O(N)$. Because the **amortized cost** of this operation is $O(1)$.

Visualization: image that the list length is doubled for every allocation, which isn't true, but the asymptotic time is the same.

```
[e e e e e e e e e e e e e e e e e e e e e e]
  ... |<3>|<--2 ops-->|<-------1 op-------->|
```

The total cost is $\frac{1}{2}*2 + \frac{1}{4}*2 + \frac{1}{8}*3 + ...$, which converges to 2, which is $O(1)$.

**The Buffer Type**

*(Only available in Python 2)*

Like multiple strings. When you're done working with it, you can convert it to a string with `str(x)`.


## Python vs. Shell vs. Emacs Scripting

Lisp is an ASL for Emacs, like an extension language. It uses existing code, *Emacs primitives*.

Shell uses existing programs.

Python was designed to be a *general-purpose programming language*, so there are no "primitives" you bring together - you just write in the language altogether to program from scratch. However, it also converges to the same phenomenon where programmers glue together existing modules like PyTorch and SciPy.

What makes a language a scripting language is one that supports this pattern of software construction of building applications from existing code.

> The goal of a **scripting language** is you don't code from scratch. You glue together other people's code. You provide the cement, and the other people provide the bricks. **- Dr. Eggert**


# Assorted Discussion Notes

**SASS** is a stylesheet language that's compiled into CSS. It extends CSS with familiar programmatic features like variables, nested rules, mixins (styles that can be reused), functions, etc.

**Regular expressions** are often used in web apps for:

- Validating phone numbers
- Validating emails
- Extracting or processing such information

*Apparently* in EREs, you can use the OR `|` operator outside of a group `()`:

```
hello|there
```


<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({ tex2jax: {inlineMath: [['$', '$']]}, messageStyle: "none" });
</script>
