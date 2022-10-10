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

Emacs uses the same notation for programs and data i.e. we use *data notation* to write our programs. The fundamental data structure in Lisp is *list*. There's another one called *pair*. A list is like a linked list of pairs.

```
# This is a pair
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

### Emacs Bytecode

Loading source code into current namespace: `M-x load-file RET filename RET`

```lisp
(distance 3 4) ; ERROR
; M-x load-file dist.el
; Now it's as if the source code of dist.el was run
(distance 3 4)
; 5.0
```

As a solution to slow interpreting speed (due to the extensive use of pointers and dereferencing), ELisp uses **bytecodes** like in Python to compile data structures to create compact representations of a program.

Bytecode differs from machine code:

- PRO: Bytecode is *portable* and works on any architecture as it is designed for some abstract machine that the Emacs application knows about.
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

The bytecode files end with the `.elc` extension. `M-x byte-compile-file RET filename.elc RET` compiles a source code file. Such `.elc` files can also be loaded with `load-file`.

## Python

More of a **general purpose programming language** - designed for you to *write* an application of your own.

