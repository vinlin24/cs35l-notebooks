# History & Versioning


## History of Version Control: Git

A good case study for version control, but has problems common to nearly every version control system.


### From User's Viewpoint

This is usually from the perspective of a software developer/manager, and in some cases, the end user trying to use the software.

Three things are under Git's control:

0. Keep track of the working files (ordinary files), your source code
1. Object database recording history of project development, where the project is modeled with a tree of files.
2. **Index**, recording your plans for the *future*.

The `git commit` command *appends the index to the history*.


### Getting Started with Git

```shell
# Set up empty repository with empty history and index:
git init

# Copy an existing object database for existing object
git clone URL
```

The output of a clone command looks something like:

```
Counting objects: ...
Compressing objects: ...
Total ...
Receiving objects: ...
Resolving deltas: ...
```

Cloning copies a repository AND creates corresponding working files.

Git sets up a special `.git` subdirectory, which is what makes the project directory a "repository".

We don't necessarily have a "boss and servant" relationship between upstream and downstream repositories. Often times, a clone can become more active/popular than the original, in which case, the latter will start to sync with the former instead of vice versa.


### Exploring an Existing Project

0. Look at working files, the *current state* of the repository.


### Looking at the Log

Display information about every commit leading to the current version, in reverse order by default:

```shell
git log
```

We note that each commit has a unique ID, a long string after the word "commit", which is the **checksum** of the commit contents. A checksum is like a fixed bit integer that is a function of the bytes of the content it is encoding. This function must not produce **collisions**, where different contents produce the same checksum.

Formatting the log:

```shell
git log --pretty=fuller
```

The fuller format shows that every commit has an **author** AND a **committer**, each with their own dates. Git distinguishes between these two contributors. This separation is routine for many larger projects. The author would be the person that writes the code, and the committer would be the overseer that reviews the and confirms the changes. These fields *establish responsibility* for changes, a major reason for using version control in the first place.

An interesting thing you may observe about repositories that have been developed for a long time is that you may notice commits with timestamps dating back to before Git was even around. This is because projects may have migrated from other version control systems, like RVS and CVS, and in copying over the history, the date data are all preserved.

**Displaying a subset of the log**

Displaying the history between commits with ref `A` (exclusive) and `B` (inclusive), so like `(A, B]`. This is like "show me everything that led up to B, but exclude everything that led up to A".

```shell
git log A..B   # .. is like a range
```

Getting the entire history up to commit with ID B:

```shell
git log B
```

Set up an empty repository with empty history and empty index:

**Using "version arithmetic"**

The special pointer `HEAD` references the *current version* of the repository.

You can use the `^` syntax to specify the parent of a reference, so `HEAD^` means the commit just before `HEAD`, `HEAD^^` means the grandparent commit, etc.

Example about showing the most recent commit:

```shell
git log HEAD^..HEAD
git log HEAD^!  # Shorthand
```


### Commit Messages

Commit messages are important because in essence, they help "market" your changes. They tell readers of the repository why certain commits were made and whether it was a "good" commit by explaining the *motivation* behind the changes. "Why are you making this change? Why shouldn't I just revert it?"

The rationale behind commit messages are similar to why you should comment your code.

**COMMENTS SAVE LIVES. ALWAYS COMMENT YOUR CODE. PLEASE.**

There is overlap between comments in the source code and commit messages, but the primary distinction is the *audience*. Commit messages are more historically oriented, what you would tell the "software historian," people interested in the development of the repository as a whole. Comments in the source code are for the "current developer," people interested in having to study or change your code.

There are many style guidelines for commit messages out there, but here's Eggert's (which looks pretty standard in my experience).

Example commit message from the MIT repository shown in lecture:

```
Fix issues found by ASAN and Coverity

* tests/test_driver.pl: Preserve the LSAN_OPTIONS variable.
* tests/scripts/targets/ONESHELL: Don't set a local variable.
* tests/scripts/functions/let: Test empty let variable.
```

The first line should be at most 50 characters, and this acts as the "subject line" for the commit, like the elevator pitch. This should give any readers the *gist* of the commit.

The second line should be empty, separating the subject line from the body.

The remaining lines should be at most 50 lines, each at most 72 characters per line. Here you describe the finer details of the commit.

---

Git's version of `ls`, where it displays the current *working files*.

```shell
git ls-files
```

This helps us distinguish between general files in the directory and files that currently *matter* with respect to the repository.

```shell
rm $(git ls-files)  # lol!
```

Finding content within files is such a common pattern that `grep` is built into Git:

```shell
git grep waitpid
```


## Viewing Differences

`git diff` is similar to the GNU `diff` command.

Viewing the difference between the *index* and the *working files*: `Δ(index vs working files)`

```shell
git diff
```

The algorithm is very complex and was developed by a professor at the University of Arizona who went on to work on the Human Genome Project.

This views the difference between the latest commit and the index: `Δ(latest commit vs index)`

```shell
git diff --cached
```

And this is `Δ(last commit vs working files)`

```shell
git diff HEAD
```

> The index is the planned version of the future (what you have ready for the next commit, but you have not committed yet). The working files are even further. into the future of the index.

**NOTE TO SELF:**

So what the heck is the index and working files>

Compare the grandparent commit to the latest commit:

```shell
git diff HEAD^^..HEAD
```

A common pattern for disambiguating a range argument and a file name argument:

```shell
git diff HEAD^^..HEAD -- AUTHORS
```

You can inspect the current state of the repository, like an every more summarized version of `git diff`:

```shell
git status
```


## Making Changes

1. Edit the working files.
2. Run `git add FILES...` to add the specified file contents to the index (the **staging area**, the **cache**). You can keep editing files and add any new changes to the staging area with the same command.
3. Run one of the `git diff` commands to verify that the changes are what you want.
4. Run `git commit`, which takes your index, makes a new commit, and puts it into the object database with the auto-generated checksum. In effect, it changes the commit `HEAD` references.

You're probably familiar with `git commit -m MESSAGE` that every Git crash course teaches you. This is useful for one-liners, but the default `git commit` drops you into your configured editor and allows you to write longer commit messages with the subject line and body format detailed above.

There is also `git commit -m MESSAGE FILE`, where `FILE` contains the extended message. This is useful for automating messages in scripting. Example:

```shell
git commit -m 'Fix issues from previous patch' README.git
```

Remove all **untracked files**:

```shell
git clean
```

This is useful for removing files created as part of some build process. If you're not sure, you can run a "what if" with the `-n` option:

```shell
git clean -n
```

There's also the `-x` option which cleans files that will even be ignored:

```shell
git clean -nx  # you best see what that would do first lol
git clean -x
```


## Configuration


### The .gitignore File

A special file inside the repository containing file patterns that Git should not track. The file pattern syntax is similar to the familiar **globbing pattern** as the shell.

.gitignore is like a configuration file that instructs how users run Git. It's under Git's control i.e. it'll show up in `git ls-files`.

**What files should be ignored?**

Files that we do not want to put under version control. Obvious candidates include:

- Temporary files, `\#*`
- Machine-dependent code, `*.o`
- Imported files (from other packages)
- Authentication information (passwords/keys/etc.)
- Hashes of passwords? If it's intended for authentication, this would be just as bad as raw passwords. Enables **rainbow attacks** on the passwords where attackers try to crack the checksum algorithm.


### The .git/config File

You can view the current configuration of the Git program with:

```shell
git config -l
```

This outputs the information stored in the editable `.git/config` file in the current repository. Cloning a repository also copies the configuration file.

**CAUTION:** One notable problem (which is standard across any software) is that if there is a syntax error in the configuration file, Git stops working altogether.

`.git/config` is NOT under version control because it determines how Git itself functions and because it would introduce the problem of recursion. `.gitignore` IS under version control because it's like a message from the developer and contains information about how to manage the project actually being version controlled. You also don't need to worry about what's in `.gitignore` to use Git itself.


### The ~/.gitconfig File

After resolving the configuration in the current repository, Git then falls back to this configuration file. Contains *global* configuration information for Git, like username and email.

---

`git show` is a generic command that shows a commit "object". Commit objects live in the database and are really just the recorded changes from the previous commit along with some metadata.

The ubiquitous `--pretty` option can be used here too for more verbose output:

```shell
git show --pretty=fuller
```


## Working with Remotes

A **remote**, named `origin` by convention, is the **upstream** repository from which the local repository was cloned or set to track.

The concept of upstream/downstream comes from the fact that clones may be sourced from repositories that are themselves sourced from another branch, forming a chain of origin - a "stream".


### Fetching

This consults the remote server for upstream changes and syncs the clone's "opinion" fo what upstream looks like:

```shell
git fetch
```

If it outputs nothing, it means the local clone is up-to-date with the upstream remote. `fetch` does not change the working files nor does it alter any branches.

`fetch` is incremental, only fetching the changes since the last call to fetch.


### Pulling

`git pull` is roughly equivalent to a `git fetch` followed by *merging* upstream changes into the current branch. This command is actually rarely ideal on large development projects because more often than not it may not be what you want to do. When it works, `pull` DOES change the working files.


## Recovering from Mistakes


### If *Before* the Commit

The working file is wrong, but the most recent version saved in the repository is safe.

1. Edit the file `F`
2. Update the index: `git add F`


### If *After* the Commit

The repository now has a bad version of the code in its history.

**Option A:** Commit a new, fixed version. The bad commit would still be recorded in the the old and fixed commit. This would be an honest representation of the development history, but often times this is not wanted.

```
(old)-->(broken)-->(fixed)
                      ^
                     HEAD
```

> An iron-clad rule in Git is that *you cannot change history*. This is because every commit is uniquely identified by the SHA-1 ID.

**Option B:** However, you can cheat this rule with the `git commit --amend` approach, which creates a new child from the parent commit and moves `HEAD` to it:

```
(old)-->(broken)
  |
  +---->(fixed)
           ^
          HEAD
```

This is very risky as an upstream repository. If someone happens to `fetch` when `HEAD` still points to the broken commit, then when they `fetch` with the new altered tree, Git and the users will get confused.

**Option C:** You can change the state of the repository back to another version.

Reverting to the previous commit.

```shell
git reset HEAD^
```

This would fail if there are changes in the working files, in which case, you can throw the changes away and revert anyway with:

```shell
git reset --hard HEAD^
```


## Branching

A single commit can have multiple children.

A **branch** in Git is like a lightweight, movable *name* for a commit that is the at the tip of a line of maintenance.

> At the end of the day, Git is all about pointers! :D

One branch, the "main"/"master" branch is typically reserved for *mainline development*. There may be other branches for things like *maintenance development*, *old releases*, *hot fixes*, etc.


### Patching Across Branches

Suppose a security hole was discovered in an old commit, which multiple branches share as an ancestor. You can fix the bug on the mainline branch, but that doesn't solve it for other branches.

The solution is to **cherry-pick fixes**. You manually apply the same Δ to all versions that have the same bug.

Suppose there's an alternate branch named `maint`.

```shell
git add F
git commit -m "Make an emergency fix"

# Prepare the patch to apply to other branches
git diff HEAD^! > t.diff

# t.diff is a working file, preserved across checkout
git checkout maint

# Apply patch to this branch's working files
patch < t.diff
git add F
git commit -m "Make an emergency fix"
```

The `patch` command is external to diff. It reads the output of the diff file and modifies the old file so that it looks like the new file.

```shell
diff -u A B > AvsB.diff
patch < AvsB.diff
```

This modifies `A` to look like `B`.

Attempting to apply a patch to a since edited version of a file may fail to work. It may still work if the changes to the original files does not *collide* with what the patch is attempting to change.

`diff` operates on **hunks**, batches of lines that represent a change. Patching goes through each hunk and applies the change. If the hunks do not match, then it will reject the change into an `rej` file, prompting you to fix it by hand.

**NOTE:** The output of `diff` is NOT deterministic. There is no requirement of the algorithm to modify a file in a specific way as long as the final copy is correct.


### Manipulating Branches

Creating a branch off a commit, defaulting to `HEAD`, and checking out to it:

```
git checkout -b NAME [REF=HEAD]
```

**EXAMPLE:** creating and checking out to a new branch named `newbr` off of the grandparent of the current `HEAD`:

```shell
git checkout -b newbr HEAD^^
```

Branch names must be unique. Git won't let you create or rename a branch to an existing name.

Because branches are just names, *deleting* a branch does not modify any commits, just a reference that used to point to one.

```
git branch [--delete | -d] NAME
```

You can try to do something weird like:

```shell
git branch -d master
```

The objects would still be there, but Git will lose track of where they are, so Git would warns you. You can *forcefully* delete a branch despite Git's warnings with:

```
git branch -D NAME
```

*Renaming* a specific branch, defaulting to the current branch:

```
git branch -m NEW_NAME [OLD_NAME=HEAD]
```


### Detached HEAD State

You can checkout to an arbitrary commit by ID/tag name:

```
git checkout REF
```

But this puts you in **detached HEAD state**, which is when `HEAD` is not pointing to any branch tip. Git warns you that you can look around but not make further changes. You cannot commit in this state because Git does not know how.


## Merging

**Merging** occurs when multiple lines of development come together as one. More technically, it is when you create a commit from two or more parents commits, which may or may not be named branch tips.

Suppose:

```
()-->(A)-->()-->()-->(Y)-->(Merged)
      |                       |
      +----->()---->(X)-------+
```

Git finds the common ancestor `A`, of the parent commits `X` and `Y`. Then, it runs computation on all 3. It is as if:

```shell
diff3 X A Y > combined.diff  # "3-way diff"
```

This file describes changes to change the common ancestor `A` to *either* `X` or `Y`.

The command to merge a branch named `BRANCH_NAME` into the current branch:

```
git merge BRANCH_NAME
```

What this does is:

1. Compute 3-way merges
2. Replace working files accordingly


### Merge Conflicts

More often than not, the changes *collide*, resulting in a **merge conflict**. If there's a collision, Git modifies the affected files with a "replica" of the collision with the special notation:

```
<<<<<<<
A (Current Change)
=======
B (Incoming Change)
>>>>>>>
```

The user then edits these lines in their editor of choice and then runs `git add` to resolve the conflict.


# Discussion Notes: C Programming and Makefiles


## Compiling/Linking with GCC

Separating the compilation from the linking:

```shell
# First compile without linking
gcc main.c def.h -c -o main.o
gcc def.c def.h -c o def.o
# Link the two objects into the target executable
gcc def.o main.o -o main
```

While you could do this on one line like:

```shell
gcc main.c def.c -o main
```

The former has the benefit that when defined in a Makefile, only the files that are updated are recompiled. This makes the process more efficient as we are not going through the entire compilation sequence for every file on every change.


## Makefile Basics

- A build tool designed for developers
- A layer above shell (should use the same shell as the one used to run `make`)
- Handle incremental build and enable parallelization

Special variables within a rule: `$@` is the target of the rule and `$^` is the prerequisites of the rule. For example:

```makefile
foo: foo1.o foo2.o foo3.o
  g++ $^ -o $@
```

Adding a `-` before a command means to keep going even if that command fails. This can be useful in cleaning commands, like:

```makefile
distclean: distclean-recursive
  -rm -f $(am_CONFIG_DISTCLEANFILES)
  -rm -f Makefile
```


### Macros

```makefile
XYZ = foo1.o foo2.o foo3.o  # XYZ is a macro
foo: XYZ
  g++ XYZ -o foo
```

Commonly you'll see people define `CC` for the compiler command (`gcc`) and `CFLAGS` for the compiler options:

```makefile
CC = gcc
CFLAGS = -O2 -g3 -Wall -Wextra
```


### Phony Targets

A **phony target** is a rule that has NO output file. The most common example is the `clean` pattern:

```makefile
clean:
  rm *.o
```

Then use `.PHONY` to declare a phony target:

```makefile
.PHONY: clean
```


## Assignment 5: Refactoring

We want to split up a long file `randall.c` into a more organized collection:

```
randall.c
  rand64-hw.h
  rand64-sw.h
  output.h
  options.h
```

Benefits of refactoring:

- Readability and modularity
- Easier to debug
- Reduces compilation time (parallel compilation of multiple files)


## System Calls

> A way for programs to interact with the operating system.

The concept of an **operating system** was invented to facilitate interaction between programs and the hardware. Without it, it would be much easier for programs to maliciously attack hardware or cause I/O conflicts with read/write operations.

Programs can make **system calls** *to* the operating system, and the operating system will then interact with the hardware in a well-defined way and report back with any output.


### Categories of System Calls

1. Process control `fork`
2. File management `open`/`close` a file, `read`/`write`
3. Device management
4. information maintenance
5. Communication
6. Protection


### The `write()` System Call

```c
#include <unistd.h>
ssize_t write(int fd, const void *buf, size_t count);
```

- `fd` stands for file descriptor, which could be stdout or stderr.
- `*buf` stands for buffer. This contains any data in it.
- `count` is the number of bytes to be written to a file descriptor from the buffer.
