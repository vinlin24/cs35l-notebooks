**Week 1 Lecture Notes**

- [Software Construction](#software-construction)
  - [Concerns and Objectives](#concerns-and-objectives)
  - [Software Philosophies](#software-philosophies)
    - [Software Tools Philosophy](#software-tools-philosophy)
    - [Little Languages Philosophy](#little-languages-philosophy)
- [Operating Systems](#operating-systems)
  - [Superusers and the Concept of Privilege](#superusers-and-the-concept-of-privilege)
- [The Shell](#the-shell)
  - [The `ps` Program](#the-ps-program)
  - [The `sh` Program](#the-sh-program)
  - [Shell Commands](#shell-commands)
    - [Running Multiple Commands](#running-multiple-commands)
    - [Discussion: Shell Exercises](#discussion-shell-exercises)
    - [Shell Syntax Convention](#shell-syntax-convention)
- [Emacs](#emacs)
  - [Concept of Buffers](#concept-of-buffers)
    - [Auto-generated Files](#auto-generated-files)
  - [Basic Usage](#basic-usage)
    - [Editor Navigation: Moving the Point](#editor-navigation-moving-the-point)
    - [Fixing Mistakes](#fixing-mistakes)
    - [Help System](#help-system)
    - [Extended Commands](#extended-commands)
    - [Window and Buffer Navigation](#window-and-buffer-navigation)
    - [Shell within Emacs](#shell-within-emacs)
    - [Selection Manipulation](#selection-manipulation)
    - [Text Manipulation](#text-manipulation)
    - [Modes](#modes)

---


# Software Construction

## Concerns and Objectives

**Software Engineering Concerns**

- **Non-technical issues**
  - Fundraising
- **Technical issues**
  - Security
  - Database of contacts
  - Where to store data
  - UI/UX
  - Recording friends' contact info
  - Network connectivity
  - Deployment
  - Portability

**Software Construction Issues**

- File systems use (data)
- Scripting (programming)
- Integration
- Configuration
- Testing
- Versioning/evolution
- Low-level debugging (GDB, linking)
- Client-server model


**Application Objectives**

- Have data that **persists** (survives outages) (vs. **volatile**)
- Be fast
- Be understandable to developers
- Be understandable to users

---
**Persistent** vs. **nonvolatile**: persistent variables live on the drive. Nonvolatile variables live in RAM.

---


## Software Philosophies


### Software Tools Philosophy

**Don't write a big program intended to solve all your problems.**

Instead, write your application using a collection of tools, each of them relatively simple, each simple and tailored to solve one class of problems really well.

Languages like JavaScript fall under this category because you build programs out of smaller modules. JS itself is definitely not a little language, but instead, users put together small parts of it that individually do its job well to ultimately construct a more complex program.


### Little Languages Philosophy

**Design small languages appropriate for each tool.**

As something grows and grows, it gets too complicated and people can't figure out how it works, and as new applications come out, it becomes less appropriate for that app, so don't let your languages scale.

Basically the opposite of C++, a huge language that attempts to solve all classes of problems (*general-purpose programming languages* in general).

**Examples:** `sh`, `sed`, and `grep` are specialized programs that all come with their own little languages.

**Downside:** for each tool you want to become an expert in, you have to learn a new language.

**Upside:** each language is very simple and does its job well.


# Operating Systems

- Ubuntu is an example of a Linux distro (distribution).
- Debian is the upstream distro for Ubuntu from which it inherits thousands of *packages*. You can can check the packages with `dpkg -l`.
- Linux is an OS **kernel**.

**Software levels of abstraction:**

```
+------------------------+
|          apps          |
+------------+-----------+
|            |   libs    |
|   kernel   +-----------+
|            | C stdlib  |
+------------+-----------+
|        hardware        |
+------------------------+
```

sh and Emacs and any of their independent instances are themselves **applications**, one of many that sit atop the operating system.

**Introspection** - When a program looks at itself ("when we use tools to find out more about our tools"). Knowing how to perform introspection is a portable, universal skill that lets you explore or relearn something about an unfamiliar program.

---
Viewing information about the processor currently running:
```shell
less /proc/cpuinfo
```

`less` is a program that outputs a stream of read-only content, prompting pagination by the user. This is useful for viewing the contents of a file or output without having it be outputted all at once to the shell.

---


## Superusers and the Concept of Privilege

- **Superusers** ("root") are the only ones with permission to `kill` PID 1, `system`.
- The **sudo** command lets you run a command AS "root":

  ```shell
  sudo sh
  ```

Why have multiple users instead of just root? The concept of **minimization of privileges** aka "principle of least privilege". If a program breaks or a user makes a mistake, the damage is limited.

---
You can kill a program by PID:

```shell
kill <PID1> <PID2> ...
```

---


# The Shell


## The `ps` Program

- Like the task manager: lists processes and their information
- For a live view, use the `top` program instead
- By default lists the processes on the local machine, but you can use some useful flags like:

  ```shell
  ps -ejH | less
  ```

Filtering the output with `grep`:
```shell
ps -ef | grep emacs
```


## The `sh` Program

Creating another instance of the shell to execute a one-off command:

```shell
sh -c <command>
```

---
**Some things you can do to annoy your system administrator:**

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

---


## Shell Commands


### Running Multiple Commands

**Commands in Sequence**

`;` is the **sequencing operator**, equivalent to a newline but allows you to automatically queue subsequent commands in one line if you're at the command line:

```shell
ls -d .; echo a
```

This forms the basis of shell **scripting** - a sequence of commands to automate certain tasks.

**Commands in Parallel**

```shell
ls -d . & echo a
```

**Command Pipeline**

Piping output of one command as the input to another. The **pipe** is itself a buffer. Commands that write can write to it, commands that read can read from it, so you can set up a sequence of writing and reading commands to **pass content between them**:

```
ls -d . | less
```

Instead of `ls -d` running until completion and *then* running `less`, *every time* `ls -d` outputs something to the buffer, `less` can read from it and execute its program.


### Discussion: Shell Exercises

1. Listing all files ended with `.html` under ~/...
2. ...sorting it...
3. ...and then outputting it into a file named `list_html.txt`:

```shell
ls ~/*html | sort > list_html.txt
```


### Shell Syntax Convention

- Normal arguments like `a`.
- Options `-ejh` which is also equivalent to `-e -j -h` (jamming them together is a thing you can do on Unix.


# Emacs

**Why use Emacs for things like cloud computing even in the modern day?**

Such applications are at the mercy of networking, which are orders of magnitude slower than operations on a local file system. Thus, they care a lot about **throughput** and **latency**, and terminal interfaces can deliver because they are simple and fast.


## Concept of Buffers

- Emacs makes use of **buffers** to be *fast*. Buffers are just a bunch of text that live in RAM.
- Emacs (and editors in general) makes a clear distinction between what's *persistent* (files that are saved) and what's not (buffers).

**Buffer-related Key Binds:**
```
C-x C-b                 list buffers
C-x b <buffer name>     switch to a buffer
```


### Auto-generated Files

- By convention, file names starting with `.#` indicate a symbolic link to a file that is currently being edited by another program.
- When editing a file `F` in Emacs:
  - Emacs creates a symlink `.#F` that signals other programs that the file is being edited.
  - Emacs creates a file `#F#`, a copy of the unsaved buffer for `F` as part of its auto-save feature, a safety mechanism for in case Emacs or the computer crashes. This file disappears on write.


## Basic Usage


### Editor Navigation: Moving the Point

**NOTE:** arrow keys (and their Shift and Ctrl variants) work too, but learning the dedicated key binds ensure that you know the key binds that are guaranteed to work on all terminals and keyboards.

The current cursor position is called the **point**.

**Smaller strides visualized:**
```
                         ^ C-p
                         |
C-a <----- M-b <-- C-b <-+-> C-f --> M-f -----> C-e
                         |
                         v C-n
```

At the **character level**:
```
C-f     Move point forward one character
C-b     Move point backwards one character
```

At the **word level**:
```
M-f     Move point forward one word
M-b     Move point backwards one word
```

At the **line level**:
```
C-e     Move point to end of current line
C-a     Move point to start of current line
C-p     Move point up one line
C-n     Move point down one line
```

These are especially useful to set up subsequent strokes like `C-a C-k` to delete the entire line or starting new lines with `C-e RET` (below), `C-a C-p RET` (above), etc.


At the **sentence level**:
```
M-f     Move to end of next sentence
M-b     Move to beginning of previous sentence
```

At the **page level**:
```
C-v     Scroll down one page worth
M-v     Scroll up one page worth
```

At the **buffer level**:
```
M-<     Move point to start of buffer
M->     Move point to end of buffer
```

From **anywhere**:
```
M-g M-g NUM RET     Go to line number NUM
```

These are especially useful when setting up searches for the "first/last occurrence" of something.

At any point, you can use `C-L` to vertically center the point if possible. Use `C-L` again to bring the current line to the top of the viewport.


### Fixing Mistakes

While still learning, it's important to know these few to get back to familiar territory when something goes wrong:

```
C-/             Undo the last command (only if text was modified)
C-g             Quit current command or exit command minibuffer
ESC ESC ESC     Universal "get out" - quit command, window, etc.
C-x C-s         Save current file
C-x C-f         Open/return to a file, creating it if necessary
C-x C-c         Exit Emacs
C-z             Temporarily suspend Emacs (enter fg to re-enter)
```


### Help System

The help system makes Emacs a "self-documenting" system. `C-h` is the designated **help key**, which prefixes commands related to viewing documentation.

```
C-h b                           list key bindings
C-h k KEY                       list one binding
C-h a <regex> RET               search for a command
M-x apropos RET <query> RET     search for a command
```


### Extended Commands

`C-x` is the designated **eXtended command key**, which starts keystroke chords for common commands, like `C-x C-f` to open a file.

`M-x` is the designated **eXtended named command key**, which focuses a command **minibuffer** at the bottom of the terminal, below the mode bar. The minibuffer is itself a buffer, and in it you can write the full name of commands.

After attempting to use the full name for a command that has a `C-x` shortcut, Emacs will tell you what the shortcut is for future use.

There is a special command that allows you to send a numerical argument to a command:
```
C-u NUM KEY     perform command NUM times
```

For many commands, this simply repeats the actions, but some commands may have slightly different behavior. For example:
```
C-u 2 C-k       delete content of two lines and their newlines
C-k C-k         delete content of a line, and then its newline
```


### Window and Buffer Navigation

```
C-x 2       split (duplicate) current window into two
C-x o       switch focus to the other window
C-x 0       kill current window
C-x 1       kill all windows except the current window
```

Opening another window for the same buffer does not duplicate the buffer; any edits in one buffer will affect the other(s). You can still use this when you want to reference some other part of the buffer while typing in another region.

```
C-x C-b     list the current buffers
C-x b       switch to a specific buffer
C-x s       prompt save for some buffers
C-x 4 C-f   open file in another window
```


### Shell within Emacs

Run a command as a separate, one-off shell process:

```
M-! <command> RET
```

Same thing but taking input from a buffer and then feeds it to the shell command (takes all characters in **current region** and pipes them to the command as its stdin, and then takes the output of the command and pipes it to the *shell command output buffer* like normal):

```
M-| <command> RET
```


### Selection Manipulation

Concept of the "current region (of current buffer)":

- You can save pointers called **marks** at arbitrary positions within a buffer.
- The **current region** is all the characters between the mark and the point.

You can set a mark at the current point with `C-SPC` or `C-@`. An example of selecting a region of text:

```
M-<             go to start of buffer
C-@             set marker
C-s eggert RET  search for "eggert"
M-|             pipe buffer into a shell command
```

You can find out where your mark is with `C-x C-x`, which exchanges point and mark (selects the text between them). You can `C-g` to cancel the selection.


### Text Manipulation

Emacs distinguishes **killing** from **deleting**. When you **kill** text, it is actually saved in a special buffer called the **kill ring**, and content in here can be reinserted (aka **yanked**) at the point. Most commands that perform bulk removal of text actually *kill* the text, not *delete* it.

```
C-k     kill from point to end of line
C-w     kill current selection
M-w     copy curernt selection
C-y     yank most recent kill
M-y     cycle the text to yank through kill history
```


### Modes

Emacs is a **modeful** editor. That means the current state of Emacs not only includes the contents of the current files being edited but also what way you intend to be using the editor next. A **mode** is like a method of interacting with the editor.

- Upside: more efficient for experts
- Downside: confusing/tricky for non-experts

```
M-x MODENAME            switch to mode
```

`C-h m` brings up a buffer that describes what mode you are in. Being in "editor" mode is called **Fundamental** mode. You can also be in **Shell** mode or **dired** (directory-editing) mode. The mode you are in affects the keys you input. You can see the name of the major mode you are currently in with the **mode bar** just above the minibuffer.

```
C-x d <dirname> RET     enter dired mode for directory
```
