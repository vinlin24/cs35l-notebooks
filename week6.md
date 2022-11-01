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
