# Week 2 Lecture Notes

## Layers within Emacs

- Emacs has a C interpreter inside it
- Emacs also has a Lisp interpreter inside it; the bulk of Emacs is written in Lisp

Emacs "layers":

```
+------------------+
| Lisp code        |
+------------------+
+------------------+
| Lisp interpreter |
+------------------+
+------------------+
| C interpreter    |
+------------------+
```

The I/O devices like mouse, keyboard, and diplay communicate with the application through the Lisp code.

Programs typically have their own interpreters embedded in their own executables. For example, Chromium executables come with a JavaScript interpreter included in them.

## Combination of Little Languages

```shell
wc $(find . -type f)
```

Within shell languges, the simplest commands look like

```
word0, word1, ... wordn
```

`word0` is the name of the program and `word1, ..., wordn` are the arguments to that program.

The subexpression `$()` syntax allows you to write a shell command as if you wrote the output of the subexpression word for word out as the arguments to the outer command.

This can have significant edge cases like:

```shell
mkdir empty
cd empty
wc $(find . -type f)
```

The program then looks like it's jammed because if the subexpression `$(find . -type f)` returns nothing, so it's *as if* you just typed the command:

```shell
wc
```

`wc` has defined behavior where if you d not provide any command line arguments, then it reads from stdin. Thus, the program is actually prompting the user to input something to the wc program.

Only caring about that last summary line, you can use the `tail` command:

```shell
wc $(find . -type f) | tail -n 1
```

A big part of scripting is knowing when you don't have to care about efficiency. "Your time is more important than the computer's time." This is a different paradigm then coding with algorithms and asymptotic time, etc. in mind.


## The sh Program

sh is the predecessor to bash (Bourne Again SH). sh was designed to work on 16-bit machines so it's a very little language. Bash adds some features in addition ot the original sh.

There is also a lot of other shell langauges ending in sh. Having so many distinct shell langauges becomes a problem, so the **POSIX standard** was created as a spec for shells. (?)

If the name of a program does not contain any slashes, then the shell automatically looks through PATH to find the program name. This is why when you want to run an executable in the current directory, you have to use the `./executable` syntax:

```console
$ a.out
bash: a.out: command not found
```

```console
$ ./a.out
Hello world
```

Alternatively you can *append* the current directory to the PATH:

```shell
PATH=$PATH:.
```

Now it would work because a.out is now treated like a command that's on your PATH:

```console
$ a.out
Hello world
```

> You can type the *null byte* with `^@`! Alternatively, you can use `printf '\0'` to use familiar C code and pipe that into whatever you want.

### Quoting within Shell

The shell itself has special characters, so you must use quoting to input what you intend to.

For example, hat if a file name has a space in it? Then you must quote it.

Special characters include *whitespace* as well as:

```
` ~ # $ & * ( ) = [ ] \ | ; ' " < > ?
```

There are two forms of quoting: single (`'`) and double (`"`).

Using single quotes makes the shell treat everything inside the quotes as a *single word*:

```
cat '3 o\'clock'
```
Notice that you can escape the single quote itself with backslash (`\`).

Alternatively you can use *double quotes* to enclose "most any characters". `$` is still special, used to interpolate other variables or subexpressions.

Quoting can even include newlines, but that's so controversial that it's probably going to be removed in an upcoming POSIX standard release.

## The grep Command

Normal invocation:

```console
$ grep PATTERN FILE1, ..., FILEn
```

If it finds a line with the PATTERN, it outputs it. So it is similar to `cat`, but more selective. In fact, if you use a pattern that matches every character, `grep` can behave identically to `cat`:

```console

```

### Pattern Matching: Regular Expressions

`grep` actually uses **Basic regular expressions** (BSEs), a simpler form of regular expressions (regex). It also only matches against single lines.

- Most printable characters like letters and numbers match themselves.
- Control characters like `*` need to be escaped in order to match themselves e.g. `\*` and `\\`.
- `.` as a control character matches every single character
- `^` matches the start of the line
- `$` matches the end of the line
- `[]` matches a single character but only those that are included in the set enclosed in the square brackets

CAUTION: the `*` is a **globbing pattern** to the shell, so it's interpreted with special meaning. Enclose it with *quoting* - don't let the *arguments themselves* be interpreted by the shell.

```shel
grep 'ab*c*' fo
```

### grep Pattern Trouble

Example:

```shell
grep '*' foo
```

This is a weird case where `*` actually matches itself instead of being treated like a quantifier. grep has some ambiguous edge cases/

But sometimes grep yells at you:

```console
$ grep '['
grep: Invalid regular expression
$ grep '\['     # this is well-defined
```

### Extended Regular Expressions

Historically there was another team that came up with alternate syntax to the familiar grep, called egrep. Nowadays you can use the `-E` flag to specify the pattern is in that team's syntax instead.

```shell
grep -E PATTERN FILE1, ..., FILEn
```

It provides features in addition to the BREs that are familiar in the full regular expression standard:

- The `+` quantifier to match "1 or more occurrences," so `P+` is equivalent to `PP*`.
- The range quantifier: `P{2,5}`.
- Grouping with `()` and combining patterns with `|` to mean logical OR.

### Bracket Expressions

- Available in both BREs and extended regular expressions.
- Ranges in bracket expressions like `[a-z]`. The `^` at the start negates the set e.g. `[^a-z]`.

Within a set, there is a different set of special characters:

- To match the `]`, put it at the start of the set like `[]abc]`
- To match the `^`, put it at the end of the set like `[abc^]`
- To match the `-`, put it at the end of the set like `[abc^-]`

There are also *named character sets*:

```
[[:alpha:]]     # match every alphabetic character
[[:alpha]$/]    # match every alphabetic character of $, /
[[:alpha]$/\]   # backslash isn't special inside a bracket
```

## Files and File Systems

### Tree Structure File System

- The model popularized by Unix and Linux
- **Directory**: file that maps *file name **components*** to files; they are literally just look-up tables
- **Regular files**: byte sequences (can be read from or written to, unlike directories)
- **Special files**: built into the kernel e.g. `/dev/null`.
- **Symbolic links** (aka **soft links**): single files that contain a single string that is interpreted as a file name (could be any file or directory, including another symbolic link); when using `ls -l`, you can see symbolic links with `->` pointing to their **target**
- Every file name look-up looks this: start at the root, consult the directory table, resolving any symbolic links along the way

> A **file name component** is a nonempty sequence of non-slash characters. The `/` character is *very special* in the POSIX file convention because they are interpreted as part of a path in the tree file system.

> `/dev/null` is useful for "reading nothing" or discarding output: `echo abc > /dev/null`

> TIP: The first flag for a file when using `ls -l` tells you the file type: `d` for directory, `l` for symbolic link, `c` for special files like `/dev/null`, `-` for regular files.

File names are not recursive by design. If you want to search for files that match the pattern like `/usr/*/emacs`, then use find:

```shell
find usr/ -name emacs
```

From the root, there is a `usr` folder (path: `/usr`). Then there's a `bin` folder (path: `/usr/bin`).

Every file in the POSIX standard has a unique **inum** associated with it.

```
$ ls -idl /
2 dr-xr-xr-x. 21 root root 4096 Dec 17  2021 /
```

`/usr` is actually a separate tree from the root tree. (?)

### More ls tricks:

```
-d      # Don't look inside directories
-i      # Display the ID of the file
```

### Linux File Systems

It's not *actually* a tree, but it still does not have loops. They did this with the notion of **hard links**, which are two different names for the same file. The hard rule is that hard links *must not point to directories*

```console
$ ls -al /usr/bin
total 373832
sdafhausegulaskhglkas .
dsafhsagkjsajhgjasjga ..
sdafkjdhsajsadhgklsda '['
```

Every directory includes links to themselves (`.`) and their parent directory (`..`). The special case is the root directory `/`, whose parent is also `/`.

File names starting with slash are *full paths*, paths that start at the root. File names that do not start with a slash are interpreted as *relative paths*, paths starting from the *current working directory*.
