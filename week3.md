# Week 3 Lecture Notes

# Scripting Case Studies

## (Emacs) Lisp

An example of **app-specific language (ASL)**. In some extent, it's an example of the *little languages philosophy*. Creators of Emacs took an existing language and mutated it to suit their particular problem.

### Namespace and Security

Emacs is very traditional - a flat, top-level namespace that can clash with itself.

This is allowed for example:

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

### Fundamental Data Structures

Emacs uses the same notation for programs and data i.e. we use *data notation* to write our programs. The fundamental data structure in Lisp is *list*. There's another one called *cons*, which is just a pair of values. A list is singly-linked list of cons.

```
# This is a cons
[A|B]
(A . B)

# This is a list
[A| ]->[B| ]->[C| ]->[D| ]->[E|/]
(A B C D E)

# A nasty combo
[A| ]->[B| ]->[C|D]
(A B C . D)

# The empty list; also used as the null terminator
()

# Thus equivalent to (A B C D E)
(A B C D E . ())

# Nested lists
[A| ]->[ | ]->[D|/]
        v
       [B| ]->[C|/]
(A (B C) D)
```

In Emacs, the *empty list* is like the *null pointer* - it's byte representation is all 0s as well.

### Emacs byte-code

Loading source code into current namespace: `M-x load-file RET filename RET`

```lisp
(distance 3 4) ; ERROR
; M-x load-file dist.el
; Now it's as if the source code of dist.el was run
(distance 3 4)
; 5.0
```

As a solution to slow interpreting speed (due to the extensive use of pointers and dereferencing), ELisp uses **byte-codes** like in Python to compile data structures to create compact representations of a program.

byte-code differs from machine code:

- PRO: byte-code is *portable* and works on any architecture as it is designed for some abstract machine that the Emacs application knows about.
- CON: Not as performant as machine code.

For example, abstractly:

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

Gets compiled to some octal string `"\1\12\33\2..."`.

The byte-code files end with the `.elc` extension. `M-x byte-compile-file RET filename.elc RET` compiles a source code file. Such `.elc` files can also be loaded with `load-file`.

### Emacs Interpreter

Is a *single-threaded interpreter*. There is only one instruction pointer (e.g. `%rip` on x86-64) at all times.

There's also a byte-code interpreter, which has some byte-code instruction pointer. Emacs is itself a C program, so under the hood it's something like:

```c
// points to current instruction in byte-code sequence
unsigned char *bcip;
```

```lisp
(global-set-key "@" "abcxyz")
; Now typing "@" is automatically replaced with "abcxyz"

; This is like setting keyboad macros:
(global-set-key "@", 'what-cursor-position)
; Now typing "@" automatically shows cursor position in minibuffer
```

This could be a security hazard, because you can also include control characters. Emacs provides an easy way to escape control characters in strings:

```lisp
"\C-k"
; "^K"
```

Emacs however checks if you attempt to recurse like:

```lisp
(global-set-key "!" "!")
; After 0 kbd macro iterations: Lisp nesting exceeds ‘max-lisp\
; -eval-depth’
```

## Python

More of a **general purpose programming language** - designed for you to *write* an application of your own. It was originally designed to teach high school students.

### Why Python?

- CONS: Slow, memory hog.
- PROS: Easier to write (development cost vs. runtime cost). A lot of libraries are also written in C/C++ code for a performance bonus, made possible by **native method interfaces**.

> Human time is much more valuable than computer time.

Prevalence in machine learning. Right place, right time: happened to be a reasonable scripting language for the job.

As a scripting language, it's still one of the leading in performance.

> There's always going to be a time where you're the blind person next to the elephant. The goal of software construction is to make you a better blind person.

### Python Internals

Python is **object-oriented**. Historically, it didn't actually start out that way. Every value is an object.

*Every value is an object.* Every object has:
- Identity - *cannot be changed*
- Type - *cannot be changed*
- Value - *can be changed, but only if object is **mutable***

### Python vs. Shell vs. Emacs Scripting

Lisp is an ASL for Emacs, like an extension language. It uses existing code, *Emacs primitives*.

Shell uses existing programs.

Python was designed to be a *general-purpose programming language*, so there are no "primitives" you bring together - you just write in the language altogether to program from scratch. However, it also converges to the same phenomenon where programmers glue together existing modules like PyTorch and SciPy.

What makes a language a scripting language is one that supports this *kind* of software construction.

> The goal of a **scripting language** is you don't code from scratch. You glue together other people's code. You provide the cement, and the other people provide the bricks.

### Python Typing

In old Python, `int` used to have fixed size, so there was the distinction between integers and longs.

Main *categories* of types:
- None
- Numbers
- Sequences
- Callables

Underlying `list` allocation stuff:

- Probably uses cache size to determine starting size
- After that, reallocation uses geometric resizing (approximately nine-eights according to [mCoding](https://www.youtube.com/watch?v=rdlQzhP71pQ))
- The total cost of calling `list.append` N times is $O(N)$. Because the **amortized cost** of this operation is $O(1)$.

Visualization: image that the list length is doubled for every allocation, which isn't true, but the asymptotic time is the same.

```
[e e e e e e e e e e e e e e e e e e e e e e]
  ... |<3>|<--2 ops-->|<-------1 op-------->|
```

The total cost is $\frac{1}{2}*2 + \frac{1}{4}*2 + \frac{1}{8}*3 + ...$, which converges to 2, which is $O(1)$.

There's a school of thought that teaches to use tuples over lists when possible because the former are *safer*.

### The Buffer Type

Like multiple strings. When you're done working with it, you can convert it to a string with `str(x)`.

*Only available in Python 2, I think.*

Historical Note:

Python was not always object-oriented; it started with functions but no classes. When classes were introduced, they implemented *methods* as functions that explicitly take the `self` first argument, which is similar to the invisible behavior in C++ OOP.

# Discussion

## SASS

A stylesheet language that's compiled into CSS. Allows usage of variables, nested rules, mixins (styles that can be reused), functions, etc.

## Regular Expressions

- grep vs. egrep (`egrep` or `grep -E`)
- Anchors `^$`
- Quantifiers `*?` and `+{}` (extended)
- Sets, ranges, negation `[^a-z]`
- Groups `(lol|okay)`

## Common Use Cases in Web Apps

- Validating phone numbers
- Validating emails
- Extracting such information

So apparently in *extended* regular expressions, you can use the OR `|` operator outside of a group `()`.
