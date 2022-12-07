**Week 5 Lecture Notes**

- [HTML](#html)
  - [Tree Structure](#tree-structure)
  - [Terminology](#terminology)
  - [DTD (Document Type Declaration)](#dtd-document-type-declaration)
    - [Problem with DTD](#problem-with-dtd)
  - [HTML5](#html5)
  - [DOM (Document Object Model)](#dom-document-object-model)
- [JS (JavaScript)](#js-javascript)
  - [In the DOM](#in-the-dom)
  - [JSX (JavaScript Extension)](#jsx-javascript-extension)
- [CSS (Cascading Style Sheets)](#css-cascading-style-sheets)
  - [Competing Style Sources](#competing-style-sources)
- [Browsers](#browsers)
  - [Browser Rendering Pipeline](#browser-rendering-pipeline)
  - [Rendering Right Away](#rendering-right-away)
  - [Optimization Techniques](#optimization-techniques)
- [Data Interchange Formats](#data-interchange-formats)
  - [JSON (JavaScript Object Notation)](#json-javascript-object-notation)
  - [XML (Extensible Markup Language)](#xml-extensible-markup-language)
- [Discussion Notes](#discussion-notes)
  - [Git](#git)
    - [Repositories](#repositories)
    - [Commits and Staging](#commits-and-staging)
    - [Status and Logging](#status-and-logging)
    - [Viewing Differences](#viewing-differences)
    - [Branches](#branches)
    - [Merging](#merging)
    - [Merge Conflicts](#merge-conflicts)
    - [Patching and Cherry-Picking](#patching-and-cherry-picking)
    - [Collaborative Best Practices](#collaborative-best-practices)
    - [Configuring Git](#configuring-git)
  - [C Programming](#c-programming)
    - [Syntax Overview](#syntax-overview)
    - [Pointers](#pointers)
    - [IO](#io)
    - [Header and Source Files](#header-and-source-files)
  - [Makefile](#makefile)
    - [Implicit Rules](#implicit-rules)
    - [Phony Targets](#phony-targets)
    - [Running the Makefile](#running-the-makefile)
  - [Tips for Assignment 4](#tips-for-assignment-4)

---


# HTML


## Tree Structure

Designed for documents accessed remotely, intended to be used by browsers - sort of like "SGML on wheels". They are both the same in that they are an ***attributed** tree of text*.

The nodes in the tree have **attributes**. The leaves of the tree are text strings.

```html
<p align="right">Hello</p>
<!-- ^ name of the ATTRIBUTE -->
<!--        ^ value of the ATTRIBUTE -->
<!--               ^ text CONTENT of the node -->
```

The HTML tree is an example of **serialization** - a standard way to convert a data structure into a string of bytes. A standard should also specify how to **deserialize** the bytes back into the original data structure.


## Terminology

An **element** is a node in the tree. This whole thing is an element:

```html
<head><!-- things here --></head>
```

The words enclosed in angle brackets are **tags**:

```html
<head><!-- things here --></head>
<!-- ^ opening tag           ^ closing tag-->
```

A **self-closing element** does not have any content, and it can be abbreviated like so:

```html
<br />
```

A **void element** is an element that never has any subtrees. These do not have to have a slash in the tag:

```html
<meta charset="UTF-8">
```

A **raw text element** is an element that can contain only text:

```html
<verbatim>lorem ipsum</verbatim>
```

A **normal element** can have sub-elements:

```html
<div>
  <p>hello there</p>
</div>
```


## DTD (Document Type Declaration)

**How do you know the context of an element's syntax?**

Some points of concern regarding the syntax of an element include:

- void, normal, raw text?
- what attributes are allowed?
- what restrictions on sub-elements?

This context is supplied by a **Document Type Definition (DTD)**, an idea inherited from SGML.


### Problem with DTD

The rapid evolution of browsers prompts the continued creation of new DTDs for new classes of browsers.

The client-server model makes this a mess because the client and server need to agree on the interface between the two. DTDs need to be contracts/protocols for *interoperability* between the browser writer and the server writer.

Historically, there grew versions 1 to 4 that followed the **Internet RFC**. This process eventually broke down because there was too much bureaucracy in putting out a new DTD version.


## HTML5

A new, *evolving* standard was created, **HTML5**. This standard is edited sporadically/constantly overseen by a committee.

HTML5 does not just cover text. It also covers the intended meaning of the element, not just the layout/presentation.

It's as if it's written:

```
<p> ... </p>
"This denotes a paragraph."
```

The exact same HTML document can appear differently on different screens depending on how the browser chooses to **render** it. This is advantageous because it allows pages to have **responsive** format depending on the device they are rendered on. HTML5 separates the presentation from the styling.

**ASIDE:** Many big companies have an incentive to join the consortium overseeing the development of projects like HTML5 because that gives them say in what happens and prevents a single entity from changing things in a direction that only benefits them.


## DOM (Document Object Model)

HTML5 also specifies the **Document Object Model (DOM)**, the standard way to access the tree representing HTML in an object-oriented program. It is like the API for tree manipulation.

The DOM unifies the stack:

- It is most often it is used inside the browser. Code inside the browser can look inside the tree.
- The server can also use the DOM to figure out what tree to build before serializing it and sending it to the browser.
- Intervening networks like routers between the client and server can also use the DOM.


# JS (JavaScript)

By default, there is a DOM **binding** for JavaScript, meaning you can traverse the tree within the programming language.

**PROBLEM:** JavaScript is a pain because it is *too powerful*. Any code can be put into a browser and introduce bugs, portability issues, etc. The debugging becomes difficult. This led to the introduction of [CSS](#css-cascading-style-sheets).

**HISTORICALLY:** JavaScript is simple and interpreted, and was chosen primarily because it happened to be a suitable programming language during a time where one was much needed to support the growing Internet. It was originally written in about a week. Had Python or another language been ready and available at the time, it may be a different story today.


## In the DOM

JavaScript's key notion is that it is hooked into HTML. JavaScript can be written directly in the DOM:

```html
<script>console.log("hello world")</script>
<script src="myscript.js"></script>
```

The `src` attribute specifies the file to fetch. If it does not begin with a protocol (e.g. `http`/`https`), then it is resolved as a file local to the website domain.

The code within the `<script>` element has full access to the DOM, including:

- Examining the DOM
- Modifying the DOM
- Performing actions (e.g. `alert`)

This entails writing a lot of code, which makes it error-prone.


## JSX (JavaScript Extension)

Thus, JSX was created as a way to simplify the generation of DOM from JavaScript code. Used in frameworks like React.

Uses a **syntax extension** to JavaScript. JSX code needs to be mechanically transformed to vanilla JavaScript before it can be interpreted.

This is a common pattern in software called **preprocessing**,where code is first written in a more understood fashion before being converted to its functional equivalent.

An angle bracket `<` at the start of an expression is invalid in vanilla JavaScript, so `<expression>` denotes the departure from normal JavaScript and the start of a JSX expression:

```jsx
const language = "en";
const class_ = "CS 35L";
const header = (
  <h1 lang={language}>
    {class_} assignment {n+1}
  </h1>
);
```

Likewise, curly braces `{}` are invalid in vanilla HTML, so encountering an expression like `{expression}` denotes an embedded return to the JavaScript world. The expression enclosed in the braces can be any valid JavaScript, with the same access to the file namespace and DOM like vanilla JavaScript.


# CSS (Cascading Style Sheets)

Designed as a **declarative** spec for what appears on the screen. **Declarative** means we specify what we want, not how to get it. Generally designed more for the **web designers** on a team than the **developers**.

CSS is a compromise between the presentational layer of HTML and the interactive layer of JavaScript. With these three languages we now have the modern **separation of concerns**:

- **HTML:** Presentation (Structure)
- **CSS:** Styling (Appearance)
- **JavaScript:** Interactivity (Functionality)

CSS statements are put into DOM style elements. The styles are *cascading*, meaning by default they are inherited by subtrees.


## Competing Style Sources

There are competing sources for putting styles into the DOM tree:

1. Author of the webpage.
2. The user of the browser.
3. The browser configuration (phone, laptop, etc.).

CSS combines these sources by specifying rules for combination. In general, it is hierarchical, with the author's styles overriding the user's, which overrides the browser's, etc.


# Browsers


## Browser Rendering Pipeline

Abstract model:

```
HTML doc --> Browser[..., parse, ..., execute, ...] --> screen pixels
```

The browser will:

1. Parse the incoming HTML document.
2. Build up the DOM.
3. **Render** (turn into pixels) the DOM onto the screen.


By default, this is very slow, so browser writers employ various techniques to make rendering as fast as possible.


## Rendering Right Away

One solution is to make the browser start rendering before having complete information about the webpage.

In practice the HTML document is received in packets, so the browser can already read part of the document while the rest is still incoming. The root tends to arrive first, which can be parsed right away to load metadata the browser can use to deduce many details about the document.


## Optimization Techniques

**(1)** The browser can guess whether an element will be rendered onto the screen (as opposed to being hidden at the bottom of the page outside the viewport). If not, skip it for now.

This means that as a JavaScript developer, you cannot assume that code will always be executed immediately because the part of the document containing the script might not be loaded yet if ever.

**(2)** The browser may also decide that some elements are low priority, in which case, defer execution.

**(3)** The browser can guess the overall layout of an element (like a `<table>`) and render it based on its guess. If the guess is wrong, re-render.


# Data Interchange Formats


These are standards to represent a tree structure in a compact format suitable for transmitting over networks.


## JSON (JavaScript Object Notation)

The standard way to represent a tree structure over the Internet.

**EXAMPLE:** The JSON representation of a popup menu:

```json
{
  "menu": {
    "id": "file",
    "value": "File",
    "popup": {
      "menuItems": [
        {
          "value": "New",
          "onClick": "CreateNewDoc()"
        },
        {
          "value": "Close",
          "onClick": "CloseDoc()"
        }
      ]
    }
  }
}
```


## XML (Extensible Markup Language)

Like HTML but designed for data.

**EXAMPLE:** The same popup menu from the JSON example:

```xml
<menu id="file" value="file">
  <popup>
    <menuitem value="New" onClick="CreateNewDoc()">
    <menuitem value="click" onClick="CloseDoc()">
  </popup>
</menu>
```

---

Different network protocols can use different formats.

In general, JSON tends to be more popular. With JavaScript code, it's easier to parse strings in JSON fields than in XML attributes because JSON by design is already valid JavaScript. JSON is also slightly smaller in terms of file sizes.


# Discussion Notes


## Git


### Repositories

1. Create one remotely on a server (like GitHub) and clone it

  ```shell
  git clone URL
  ```

2. Create one locally and then link it to a remote

  ```shell
  git init
  git remote add NAME URL
  ```

### Commits and Staging

**Commits** are like checkpoints for your code, snapshots that are saved in the repository.

You know how it goes:

```shell
git add PATHSPEC
git commit -m MESSAGE
git push
```

There is an intermediate phase between modified/unmodified files and commit called the **staging area**. You can add files to this phase with `git add`

**Unstaging** a file:

```shell
git restore ---staged FILE
```

Syncing your local repository with the remote one:

```shell
git fetch
git pull  # effectively fetching + merging
```


### Status and Logging

Checking the status of your repository:

```shell
git status
```

Each file can be in the following states:

- Staged
- Not staged but modified
- Untracked


In general, files go through these states:

```
Untracked Modified Modified Staged
--+----------+---------+--------+
  |-----------(add)------------>|
  |          |-(edit)->|        |
  |<(remove)-|         |(stage)>|
  |          | <-----(commit)---|
```

**Viewing commit history:**

```shell
git log --stat
git log --oneline
git log --pretty=format:"%h - %an, %ar: %s"
git log --oneline --decorate --graph --all

# Look for differences that change the occurrences of specified string
git log -S<string>
```


### Viewing Differences

Viewing the changes in your repository for files that you have modified but not yet staged:

```shell
# git diff [REF]
git diff
git diff HEAD

# There's also (and these are equivalent):
git diff --staged
git diff --cached
```

Viewing difference between two commits:

```shell
# Typically with SHAs of the specific commits you want
git diff REF..REF

# But you can also abbreviate the hashes:
git diff 5c6cb30..53bf6bd
git diff 5c6c..54bf

# But this has a limit. This fails:
git diff 5c6..53b

# As usual you can use the HEAD ref to reference commits relative to
# the last commit:
git diff HEAD~..HEAD
git diff HEAD~4..HEAD
git diff HEAD^..HEAD
```

<!-- Oooh this is actually wrong, it's the other way around. Fixed in my topic-organized notes. -->

The special syntax around the `HEAD` ref:

- The tilde `~` denotes ancestry. `HEAD~`/`HEAD~1` is `HEAD`'s parent, `HEAD~2` is the the grandparent, stc.
- The caret `^` denotes the first parent of the tip of the current branch. TL;DR it helps resolve multiple ancestry, which is not something I will be bothering with any time soon.

<!-- Above is wrong. Should be the other way around. -->


### Branches

Allow you to develop different features in parallel. "Branches" are pointer nodes to different parts of the commit tree. Switching between branches is equivalent to moving the special pointer, `HEAD` (kind of like a "you are here"), to point to these branch nodes.

```shell
git branch [-a] [-v]  # listing (all) branches (verbosely)
git branch [-D | --delete] BRANCH
```

**Switching between branches:**

```shell
git checkout REF
git checkout -b NAME  # create and switch
# create if doesn't exist, else reset existing, then switch
git checkout -B NAME
```


### Merging

```shell
git merge REF
git merge --squash
```


There are several merging techniques:

**(1) Fast-Forward**

This is the default behavior. Move the branch head up to the merge commit of the merging branch.


**(2) Three-way merge**

Necessary when the branch to merge to has changed.

Git will find the *common ancestor* of the two branch heads and then create a new commit.

---

A more advanced way of managing your commit history:

```shell
git rebase  # append the new commits to the leaf node
```

---

Two commits can share more than one **common ancestor**. There's also a command called `git merge-base` that makes this happen.


### Merge Conflicts

**Merge conflicts** prompt you to edit the conflicted files. You can also use a special tool like the Merge Editor in VS Code to use a GUI to more intuitively select which version (*current* vs. *incoming*) to retain.

Without a GUI, conflicts in their raw form actually look like:

```
this text doesn't have any conflict
<<<<<<< HEAD
conflicting text already in current file
=======
conflicting text from file being merged
>>>>>>> SHA-of-the-incoming-commit
```

Git actually modifies the content of the conflicting file with this pattern, conflicting text separated by special barriers. The one with `<` brackets shows the **CURRENT** content, and the one with `>` brackets shows the **INCOMING** content. To resolve the conflict, you need to edit this block to only include one version of this content ("accept current change" or "accept incoming change"). You can also accept both changes. You could also leave the file in this conflicted state with the barriers, but that's stupid practice because if you do this on a source file, it will almost definitely be a syntax error.

**Resolving conflicts:** usually you would edit the conflicting files in your editor of choice and regularly check for further instructions with `git status`. You can just conclude the merge with:

```shell
git merge --continue  # but not recommended
```

### Patching and Cherry-Picking

Create a patch file(s) for a commit

```shell
git format-patch [-o DIR] REF
```

```shell
git am FILES...
git apply FILES...
git cherry-pick REF
```


### Collaborative Best Practices

**Branches:**

- master or main: stable branch
- develop: for development
- each team member may create ther branches for individual features/bugs

Protect the master branch. DON'T force-push; it could destroy the commit history. Don't be that guy.

On GitHub, use **pull requests** to merge changes into other branches. Pull requests may undergo a **review**. You can also use **issues** to assign bugs or features to team members.

Avoid merging temporary files or debugging code.


### Configuring Git

You can use:

```shell
git config --global KEY VALUE
```

This writes to the `~/.gitconfig` file, which you could also edit manually with your editor of choice.

**Setting up Git on New Machines:**

```shell
git config --global user.name "Vincent Lin"
git config --global user.email vinlin24@outlook.com
```

User name is not that important. It's mostly used for identifying contributors at a glance with things like `git log` I assume. The email however is critical because remote services like GitHub use that to identify the account of the contributor.


## C Programming

### Syntax Overview

You know how it goes:

- `#include` for header files.
- `int main() {}` for the main function.
- A variable is declared starting with its type.
- Use `{}` to contain the body of functions/loops/if.
- The `bool` data type is not actually built in. You have to `#include <stdbool.h>`, which really just defines `typedef unsigned char bool`.


### Pointers

You are forced to use **pointers** whether you like them or not!

- `&name` makes a pointer.
- `*ptr` dereferences it.

You can use `malloc`, defined under `<stdlib.h>`, to dynamically allocate memory at runtime.

You can treat pointers and arrays interchangeably in many sitations... if you know what you're doing.


### IO

<!-- TODO -->


### Header and Source Files

**Header files** (*.h) contain declarations and macro definitions.

**Source files** (*c) implementations of the declared names.


## Makefile

When you want to automate tasks like the tedious compilation statements:

```shell
gcc -o foo foo.c
```

A Makefile consists of **rules**, organized into blocks like so:

```makefile
target1: prereq1 prereq2 ...
  recipe1

target2: prereq1 prereq2 ...
  recipe2
```

The **target** is the file you want to produce. The **prerequisites** are the rules required for this rule. The **recipe** is the sequence of shell commands you run to produce your target file.


You can define variables with a similar syntax to shell scripting:

```makefile
CC = gcc
CFLAGS = -O3

# Then you can reference with $(CC) or ${CC}
```

### Implicit Rules

If you don't define a rule for a prerequisite, an implicit rule may be used in its place. For example, `foo.o` is included as a prerequisite to the `foo` target, there is no explicit `foo.o` rule defined elsewhere in the file. Thus, its rule is written for it.

From the GNU documentation:

> Compiling C programs
>
> 'n.o' is made automatically from 'n.c' with a command of the form `$(CC) -c $(CPPFLAGS) $(CFLAGS)`.

```makefile
foo: foo.o bar.o
  cc -o foo foo.o bar.o $(CFLAGS) $(LDFLAGS)
```


### Phony Targets

Using not the name of a file, but an action.

Example:

```makefile
clean:
  rm -f *.o *.$(TAREXT) randall
```

### Running the Makefile

The command is `make`.

Specifying a target:

```shell
make TARGET
make clean  # example
```


## Tips for Assignment 4

1. `git blame` and how to use it on GitHub
2. Tags
3. The shell command `diff` and how to **patch**

Helpful commands (if opting to use shell scripting instead of making your life easier with Python):

1. `awk -F`
2. `$1`, `$2` in shell scripting
3. `git stash`
4. Hot to do `git stash` before using `git` (by `diff` and `patch`)
