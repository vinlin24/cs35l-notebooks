# Lec 01 09-26-2022 M-01

## Grading

- 30% final
- 20% midterm - exams are open books + notes but closed computers>
- 35% final group project
- 13% homework
- 1.5% class participation (Piazza, etc.)
- 0.5% two feedback surveys

## Course goal: change the world via software

## Technical vs non-technical issues

- Non-technical
  - Fundraising
- Technical issues:
  - Security
  - Database of contacts
  - Where to store data
  - UI/UX
  - Recording friends' contact info
  - Network connectivity
  - Deployment
  - Portability

^ Software engineering
v Software construction

- Construction issues
  - File systems use (data)
  - Scripting (programming)
  - Integration
  - Configuration
  - Testing
  - Versioning/evolution
  - Low-level debugging (GDB, linking)
  - Client-server model

## We want our apps to:

- have data that persists (survives outages) (vs. volatile)
- be fast
- be understandable to developers
- be understandable to users
-

> Persistent vs. nonvolatile: persistent variables live in the flask. Nonvolatile variables live in RAM.

- Ubuntu is a Linux distro (distribution)
- Linus is an OS **kernel**

```
+------------------------+
|          apps          |
+------------+-----------+
|   kernel   |   libs    |
+------------+-----------+
|        hardware        |
+------------------------+
```

- Thousands of packages: `dpkg -l` supposedly
- Debian upstream - thousands of packages

## Emacs Notes

- M-! run shell command
- C-x C-o switch to other window
- C-x C-b list buffers (bunch of text in RAM inside Emacs)

- Emacs makes use of buffers to be *fast*. There's a clear distinction between what's *persistent* (files that are saved) and what's not (buffers that live on RAM).
- By convention, file names starting with `.#` indicate a symbolic link to a file that is currently being edited

- When editing a file F in Emacs:
  - Creates a symlink `.#F` that signals other programs that file is edited
  - Creates a `#F#`, a copy of the unsaved buffer for F (this happens due to the auto-save feature, a safety feature for in case Emacs or the computer crashes, disappears on save)

List processes:
```shell
ps -ef
```

Filter with grep:
```shell
ps -ef | grep emacs
```

Killing programs by PID
```shell
kill <PID1> <PID2>...
```

# Lec 02 09-28-2022 W-01

## Unix sh, emacs, file system

sh.exe and emacs and any of their independent instances are themselves applications, one of many that sits atop the operating system.

**Introspection** - When a program looks at itself ("when we use tools to find out more about our tools"). It's an important skill because everyone will forget what a certain `-i` flag does, but knowing how to perform introspection is a portable, universal skill that lets you explore or relearn something about an unfamiliar program.

The `ps` program - lists processes and their information.

`ps` by default lists the processes on the local machine.

`ps -ef` lists the processes on the server (?).

> `less` is read-only and outputs data
> `wc` is like "word count" - output the character count, etc.

```
ps -ejH | less
ps -ejG | wc
```

`ps` is like the task manager - lists the processes and sorted descending by CPU usage.

`kill <PID>` kills a process

Create another instance of the shell to execute a one-off command:

```shell
sh -c
```

Telling the shell to go into an infinite loop:

```shell
sh -c 'while true; do true; done'
```

A no-op for a number of seconds, doesn't use CPU

```
sleep 10
```

`;` is the **sequencing operator**, equivalent to a newline but allows you to automatically queue subsequent commands in one line if you're at the command line:

```shell
ls -d .; echo a
```

Running commands in parallel:

```
ls -d . & echo a
```

Piping output of one command into the other. The pipe itself is a buffer. Commands that write can write to it, commands that read can read from it, so you can set up a sequence of writing and reading commands to pass output between them

```
ls -d . | less
```

```shell

```

**Superusers** ("root") are the only ones with permission to kill PID 1, systemd.

View information about the processor currently running

```shell
less /proc/cpuinfo
```

Shell syntax convention

- Normal arguments `a`
- Options `-ejh` which is also equivalent to `-e -j -h` (jamming them together is a thing you can do on Unix`

The **sudo** command lets you run a command AS `root`.

```shell
sudo sh
```

Why have multiple users instead of just root?

The concept of **minimization of privileges** aka "principle of least privilege". If a program breaks or a user makes a mistake, the damage is limited.

## Software tools philosophy

- **Don't write a big program intended to solve all your problems**; instead, write your application using a collection of tools, each of them relatively simple, each simple and tailored to solve one class of problems really well
- Languages like JavaScript falls under this category because you build programs out of smaller modules. JS itself is definitely not a little language, but instead, users put together small parts of it that individually do its job well to ultimately construct a more complex program.

## Little languages philosophy

- Design small languages appropriate for each tool.
- As something grows and grows, it gets too complicated and people can't figure out how it works, and as new applications come out, it becomes less appropriate for that app.
- So don't let your langauges scale.
- sh, sed, and grep have little languages inside them
- Downside: for each tool you want to become an expert in, you have to learn a new language
- Upside: each langauge is very simple and does its job well
- Basically opposite of C++, a huge language that attempts to solve all problems. *General-purpose programming languages* in general.

Why use Emacs for things like Cloud stuff even in the modern day?

Terminal interfaces are simple and fast:

- **Throughput** - bits per second
- **Latency** - seconds

```
C-x o - focus other window
C-x 1 - make current window the only window
C-h b - list key bindings
C-h k <KEY> - list one binding
M-x apropos RET <query> RET - search for a command
```

The help system makes Emacs a "self-explanatory" system.

Creating a shell inside Emacs:

Run a command as a separate, one-off shell process:

```
M-! <command> RET
```

Same thing but takes input from a buffer and then feeds it to the shell command (takes all characters in **current region**, feem them to the command as its stdin, and then take the output of the command and the *shell command output buffer*):

```
M-| <command> RET
```

Concept of the "current region (of current buffer)"

- Current cursor position is called the **point**
- You can place pointers at other positions called **marks**
- The **current region** is all the characters between the mark and the point

You can set a mark with `C-@`. An example of selecting a region of text:

```
M-<  # go to start of buffer
C-@  # set marker
C-s eggert RET  # search for "eggert"
M-|  # feed buffer into a shell command
```

You can find out where your mark with `C-x C-x`, which exchanges point and mark (selects the text between them). You can `C-g` to cancel the selection.

The `truncate` command sets a certain file to a certain size, ending at the size you specify (filling with null data if the size expands I presume):

```shell
truncate --size=10TB bigfile
```

> Newline character is ^J in ASCII

Splitting windows in Emacs (that actually creates duplicate references to the same buffer, editing one edits the other

```
C-x 2  # split in 2 windows
C-x 0  # kill current window
```

Emacs is a **modeful** editor. That means the current state of Emacs not only includes the contents of the current files being edited but also what way you intend to be using the editor next. A **mode** is like a method of interacting with the editor.

- Upside: more efficient for experts
- Downside: confusing/tricky for non-experts

`C-h m` brings up a buffer that describes what mode you are in. Being in "editor" mode is called **Fundamental** mode. You can also be in **Shell** mode or **dired** mode. The mode you are in affects the keys you input.

Enter directory-editing mode:

```
C-x d <dirname> RET
```

# Discussion Week 1

Emacs go-to line:
```
M-x M-x
```

### Shell Exercises 

Listing all files ended with .html under ~/:
```shell
ls ~/*.html
```

Sort and output all the files ended with .html under ~/:
```shell
ls ~/*html | sort
```

Sort and output all files ended with .html under ~/ into a file list_html.txt:
```shell
ls ~/*html | sort > list_html.txt
```



