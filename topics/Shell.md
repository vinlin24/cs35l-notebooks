# The Shell


## The `(ba)sh` Program


A shell is itself a program, and programs themselves are just files that can be executed by the operating system.

**sh** is the predecessor to **bash** (Bourne Again SH). sh was designed to work on 16-bit machines so it's a very little language. Bash adds some features in addition ot the original sh.

There is also a lot of other shell languages ending in sh. Having so many distinct shell languages becomes a problem, so the **POSIX standard** was created as a spec for shells.

Creating another instance of the shell from within the shell itself to execute a one-off command:

```shell
sh -c <command>
```

This is what you call a **subprocess**, or a **child process**. Within your running instance of `sh` (the CLI you're typing into), another *instance* of `sh` is spawned as a child of that `sh`. In fact, every time you run a command, an instance of their little program is attached to your shell as a child process. You can see this for yourself when using commands like [`ps`](#the-ps-program) to show processes and their parentage.


### ASIDE: Some Mischievous Things


**Things you can do to annoy your system administrator*:*

Telling the shell to go into an infinite loop:

```shell
sh -c 'while true; do true; done'
```

A no-op for a number of seconds, doesn't use CPU

```shell
sleep 10
```

The `truncate` command sets a certain file to a certain size, ending at the size you specify (filling with null data if the size expands I presume):

```shell
truncate --size=10TB bigfile
```

If you inspect the filesystem, you'll find that you didn't actually use up that much space, just convince the directory listings that that much space had been allocated for *something*. This is still annoying though because it will trip up the sysadmins when they perform **backups**.


## Shell Commands


### Basic Output Manipulation


Some basic commands and their most common arguments you should integrate into your workflow (and ones that will be used without warning from here on out). These are typically used as something to pipe output *into* to improve your experience:

* `wc [-cmlw]`: Output statistics about the number of bytes (`-c`), characters (`-m`), lines (`-l`), and/or words (`-w`) of the input stream or file(s).
* `head -n N`: Output only the first `N` lines of the input text.
* `tail -n N`: Output only the last `N` lines of the input text.
* `more`: Paging utility that lets you scroll through text a screenful at a time instead of having it be outputted all at once to the console. Like a read-only editor, it also supports interactive commands reminiscent of Vi keybindings and features like regex search.
* `less`: The direct upgrade to `more`, it also supports backwards navigation and more inclusive support for keybindings.


### Command Arguments


Within shell languages, the simplest commands look like:

```
word0, word1, ..., wordn
```

`word0` is the name of the program and `word1, ..., wordn` are the arguments to that program. Arguments typically follow conventions:

* Normal (**positional**) arguments, like `a`.
* **Options** (aka **flags**) `-ejh` which is also equivalent to `-e -j -h`. Jamming them together is a thing you can do on Unix, but as we know from Assignment 2, this behavior becomes tricky when there are options that take arguments.
* Options that *don't* take any arguments are sometimes referred to as **switches**, and they set some kind of configuration simply by being present or absent (think of it like a boolean option, where presence means true and absence means false).


### Command Substitution


<!-- NOTE: I previously referred to this as "sub-expression", which is easily understood in general speak, but that's not actually the formal terminology. -->

The **command substitution** `$()` syntax allows you to write a shell command *as if you wrote the output* of the sub-expression word for word out as the arguments to the outer command.

Note that this can have significant edge cases like in this example from lecture:

```shell
mkdir empty
cd empty
wc $(find . -type f)
```

The program then looks like it's jammed because if the sub-expression `$(find . -type f)` returns nothing, it's *as if* you just typed the command:

```shell
wc
```

`wc` has defined behavior where if you did not provide any command line arguments, then it reads from stdin. Thus, the program is actually prompting the user to input something to the `wc` program.

Only caring about that last summary line, you can use the `tail` command:

```shell
wc $(find . -type f) | tail -n 1
```


### Running Multiple Commands


**Commands in Sequence**

`;` is the **sequencing operator**, equivalent to a newline but allows you to automatically queue subsequent commands in one line if you're at the command line:

```shell
ls -d .; echo a
```

This forms the basis of shell **scripting** - a sequence of commands to automate certain tasks.

> A big part of scripting is knowing when you don't have to care about efficiency. "Your time is more important than the computer's time." This is a different paradigm then coding with algorithms and asymptotic time, etc. in mind.

**Commands in Parallel**

```shell
ls -d . & echo a
```

**Command Pipeline**

**Piping** output of one command as the input to another. The **pipe** is itself a buffer. Commands that write can write to it, commands that read can read from it, so you can set up a sequence of writing and reading commands to **pass content between them**:

```
ls -d . | less
```

Instead of `ls -d` running until completion and *then* running `less`, *every time* `ls -d` outputs something to the buffer, `less` can read from it and execute its program.

This is **massively useful** because it allows you to set up a sequence of little languages that each process or transform a single stream of text output as it makes its way into some final form.

Example from discussion:

1. Listing all files ended with `.html` under ~/...
2. ...sorting it...
3. ...and then outputting it into a file named `list_html.txt`:

```shell
ls ~/*html | sort > list_html.txt
```


## The `ps` Program

* Like the task manager: lists processes and their information
* For a live view, use the `top` program instead
* By default lists the processes on the local machine, but you can use some useful flags like:

```shell
ps -ejH | less
```

You can pipe the output into the [grep command](#the-grep-command-and-pattern-matching) to filter it by a keyword, as if searching for a process by name:

```shell
ps -ef | grep emacs
```


## PATH and Resolving Command Names


If the name of a program does not contain any slashes, then the shell automatically looks through directories listed in the `PATH` **environment variable** to find the program name. This is why when you want to run an executable in the current directory, you have to use the `./executable` syntax:

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

Now it would work because `a.out` is now treated like a command that's on your `PATH`:

```console
$ a.out
Hello world
```

Had to say it one last time for y'all:

> For the last time, make sure `/usr/local/cs/bin` is prepended to your `PATH`!

Y'all better understand why that's so important now. Cheers.

<!-- I kind of forgot what this aside was in reference to: -->

> You can type the *null byte* with `^@`! Alternatively, you can use `printf '\0'` to use familiar C code and pipe that into whatever you want.


## Quoting


The shell itself has special characters, so you must use **quoting** to input what you intend to. A common use case is quoting a file name that has a space in it so the shell does not treat the space as a delimiter for arguments.

Special characters include *whitespace* as well as:

```
` ~ # $ & * ( ) = [ ] \ | ; ' " < > ?
```

There are two forms of quoting: single (`'`) and double (`"`).


### Single Quoting


Using single quotes makes the shell treat everything inside the quotes as a *single word*. More importantly, *every* character inside is preserved *literally*. This means that you cannot include the single quote `'` itself because escaping is not possible.

```console
$ echo 'hello\there\n"general kenobi"'
hello\there\n"general kenobi"
```


### Double Quoting


Alternatively you can use *double quotes* to enclose "most any characters". `$` is still special, used to **interpolate** other variables or sub-expressions. Special characters are also possible with **escape sequences**, like `\"` to represent a double quote mark itself. Interestingly, `\n` does not work the way you would expect.

```console
$ echo "hey there's\n\"general kenobi\""
hey there's\n"general kenobi"
```

Quoting can even include newlines itself (RET at the command line), but that's so controversial that it's probably going to be removed in an upcoming POSIX standard release.


## The `grep` Command and Pattern Matching


Normal invocation syntax:

```shell
grep PATTERN FILE1, ..., FILEn
```

If it finds a line with the PATTERN, it outputs it. So it is similar to `cat`, but more selective. In fact, if you use a pattern that matches every character, `grep` can behave identically to `cat`:

```shell
grep '.*' file
```


### Basic Regular Expressions


`grep` actually uses **Basic regular expressions (BREs)**, a simpler form of the more familiar regex, **extended regular expressions (EREs)**. It also only matches against single lines at a time.

* Most printable characters like letters and numbers match themselves.
* Control characters like `*` need to be escaped in order to match themselves e.g. `\*` and `\\`.
* `.` as a control character matches every single character
* `^` matches the start of the line
* `$` matches the end of the line
* `[]` matches a single character but only those that are included in the set enclosed in the square brackets

**CAUTION**: the `*` is a **globbing pattern** to the shell, so it's interpreted with special meaning. Enclose it with *quoting* - don't let the *arguments themselves* be interpreted by the shell.

```shell
grep 'ab*c*' fo
```


### Meta-character Dangers


Quoting and little language commands like `grep` go hand-in-hand. Scripts are evaluated by the shell first before being shipped off as arguments to its child programs, so make sure you quote and/or escape everything keeping in mind how your initial string will be **resolved** as it makes its way through each program.

As described in [single quoting](#single-quoting), single quotes are often used for `grep` expressions because they preserve everything literally - what you see within the quotes is *exactly* what you're giving `grep`.

When your expression gets more complicated however, you will likely have to use [double quotes to be able to interpolate](#double-quoting) regex fragments into the larger expression. In those cases, you'll likely see many instances of double backslashes `\\` since `\` is still a special character within double quotes which much be **escaped** to represent themselves by the time it reaches `grep`.


### ASIDE: grep Edge Case


Example:

```shell
grep '*' foo
```

This is a weird case where `*` actually matches itself instead of being treated like a quantifier. grep has some ambiguous edge cases.

But *sometimes* grep yells at you:

```console
$ grep '['
grep: Invalid regular expression
$ grep '\['
```

So basically, just use something well-defined.


### Extended Regular Expressions


Historically there was another team that came up with alternate syntax to the familiar grep, called egrep. Nowadays you can use the `-E` flag to specify that the pattern is using the extended syntax instead:

```shell
grep -E PATTERN FILE1, ..., FILEn
```

It provides some features in addition to the BREs:

* The `+` quantifier to match "1 or more occurrences," so `P+` is equivalent to `PP*`.
* The range quantifier: `P{2,5}`. This would match the pattern `P` 2 to 5 times in a row, inclusive.
* Grouping with `()` and combining patterns with `|` to mean logical OR. For example, `(hello|foo)` matches *either* the entire string `hello` or the entire string `foo`.

Note that these features means the characters `+` `{` `}` `(` `|` `)` become **meta-characters** in EREs. In BREs, they would match themselves literally.

**NOTE:** `{}` is *also* a **globbing pattern** to the shell, where `{a,b,c,...}` expands to `a b c ...`, so remember to [quote your regex](#meta-character-dangers)!


### Both BREs and EREs


* Bracket expressions `[]` are available in both standards and specify a **character set**.
* **Ranges** in bracket expressions like `[a-z]`. The `^` at the *start* negates the set e.g. `[^a-z]`. To include the literal `^` inside the character set, put it at the end.

Within a set, these characters take on special meanings, so to match them literally:

* To match the `]`, put it at the start of the set like `[]abc]`
* To match the `^`, put it at the end of the set like `[abc^]`
* To match the `* , put it at the end of the set like `[abc^* `

There are also **named character sets** in `grep`:

```
[[:alpha:]]     # match every alphabetic character
[[:alpha]$/]    # match every alphabetic character or $, /
[[:alpha]$/\]   # backslash isn't special inside a bracket
```


## The `sed` Command


A command that is a like a generalization of `grep`, `head`, `tail`, etc.

Short for "stream editor". `sed` was designed to let you edit a file of ANY size because it does not use a buffer; rather it uses a **stream**. It's a "programmable incremental editor".

**Using sed to emulate previous commands:**

```shell
sed -n '1,10p'      # head -n 10
sed -n '10q'        # head -n 10
sed -n '$p'         # tail -n 1; can't generally because sed is incremental
```

`sed` itself is a little language that has supports its own scripting.


### sed Scripting


| Character | Command    |
| --------- | ---------- |
| p         | print      |
| q         | quit       |
| s         | substitute |
| d         | delete     |

The substitution pattern is:

```
s/PATTERN/REPLACEMENT/
```

<!-- TODO: add more information on sed usage -->

This is an example from lecture where you remove trailing whitespace (not sure if I copied it down correctly):

```shell
sed 's/[[:space:]]*$//; /^$/d'
```


## The `awk` Command


In a sense, even more general then `sed`. A scripting language designed to edit text. It's a programming language with variables, arrays, regular expressions, etc.

Here are some examples from lecture:

```shell
awk '{ x = $0; print "("x")"; }'
awk '{ if ($0 == x) print $0; x = $0 }'  # outputs duplicates
```

<!-- TODO: add more information on awk usage -->
