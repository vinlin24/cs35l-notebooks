**Week 9 Lecture Notes**

- [Git Internals (Continued)](#git-internals-continued)
  - [Comparison to File Systems](#comparison-to-file-systems)
  - [Creating Git Objects](#creating-git-objects)
    - [Object Types](#object-types)
  - [Commit Objects](#commit-objects)
  - [Internal Form for Blobs](#internal-form-for-blobs)
- [Compression](#compression)
  - [(1) Huffman Coding](#1-huffman-coding)
    - [Decompressing](#decompressing)
    - [Table Generation Algorithm](#table-generation-algorithm)
  - [(2) Dictionary Coding](#2-dictionary-coding)
    - [Decompressing](#decompressing-1)
  - [zlib](#zlib)

---


# Git Internals (Continued)


## Comparison to File Systems

It seems that Git uses a collection of files to represent a repository (and the index). In reality, Git actually uses a combination of secondary storage and RAM (cache).

A local Git repository and the index are made up of **objects** and "other stuff". Git objects are like a tree of files in the file system.

**REMINDER:** Every file has a unique index, namely the **inode number**, which you can see with `ls -`.

Analogously, SHA-1 checksums for Git objects have the role that inode numbers have in file systems. They are comparable to pointers in C/C++, values that uniquely identify the actual objects they reference.

A crucial difference is that files in the file system can be mutable. inode numbers thus exist *independently* of the contents of their files. However, checksums uniquely identify objects *by their content*. Therefore, you **cannot** change objects' contents.

A similarity is that they are both directed acyclic graphs (DAGs). In the file system, it's guaranteed by the OS that you cannot have cycles. For Git objects, you cannot create a cycle because you can only *add* to history, not change it.


## Creating Git Objects

Generate a checksum from string content:

```console
$ echo 'Arma virumque cano.' | git hash-object --stdin
24b390b0e3489b71977f5c7242a4679287349242
```

You can also supply a file name as a positional argument instead of using `--stdin`.

Computing the checksum *and* writing it to the repository:

```console
$ echo 'Arma virumque cano.' | git hash-object --stdin -w
24b390b0e3489b71977f5c7242a4679287349242
$ # This object now exists in the file system
$ ls -l .git/objects/24/b390b0e3489b71977f5c7242a4679287349242
-r--r--r-- 1 vinlin 197609 36 Nov 22 03:46 .git/objects/24/b390b0e3489b71977f5c7242a4679287349242
```

Notice that the file has 444 permissions. *No one* is allowed to write to this file.

*There's always a troublemaker!*

```shell
chmod u+w .git/objects/24/b390b0e3489b71977f5c7242a4679287349242
```

You're technically *allowed* to do this, but in doing so, you're violating an invariant that Git trusts in order to properly function, so live your with your consequences I guess.

Decoding the content of the created Git object:

```console
$ git cat-file -p 24b390b0e3489b71977f5c7242a4679287349242
Arma virumque cano.
```

You can check the *type* of the file with:

```console
$ # !$ is shorthand for the last arg of prev cmd!
$ git cat-file -t !$
blob
```


### Object Types

- **blob**: represents any bytes sequence, like regular files in a file system
- **tree**: represents a node in a tree of objects; maps names to SHA-1 checksums of blobs or other tree

You can see the organization in action with something like:

```console
$ git cat-file -p 'main^{tree}'
100644 blob fa86c55e687fef76cb5801776756a6cb204cd2f9  .gitignore
100644 blob 630e72d7faef82103be5f27139030b67ef942ca0  README.md
100644 blob 1c6781665caca7fa3350bdfcb4a820fbec2dab05  week1.md
100644 blob f1f8f3890378332cffb6ddfb3dbc84c20e3a9470  week2.md
100644 blob d94ba050bf8045d989617e2fdf75206b2d7d97a4  week3.md
100644 blob 1131c977a2f8db9031e081a16c028bf493dfd02a  week4.md
100644 blob 4923796df144352f4784e7a90cf58a98e64d7e04  week5.md
100644 blob 8017ea5aaff41016d6813e329f75a008d2b8cd0a  week6.md
100644 blob e34d49dc0e8754b9de4e1a46449541c15e31af86  week7.md
100644 blob 8110b211ea75e284e8ad5a55b8e97c9927a0dc8a  week8.md
```

Each commit points to a tree. The tree is like a directory in a file system. Above, `main^{tree}` refers to the tree object referenced by the commit object referenced by the branch named `main`.

Examining the output, we see 4 columns:

```
100644 blob fa86c55e687fef76cb5801776756a6cb204cd2f9  .gitignore
```
```
(mode) (type) (SHA-1 checksum) (name)
```

The first column is octal digits, the last three of which represent the Linux permissions of the file in question (so `644` means `rw-r--r--`).

The second column is the type, **blob**, another **tree**, etc.

The third column is the SHA-1 checksum of the *referred* object. After all, a tree just represents a pointer to a bunch of other objects.

The fourth column is the name of the file.


## Commit Objects

Every commit object points to two things. First is the tree the commit represents. The second is the parent commit object(s).

```console
$ git cat-file -p main
tree aa3ca55b785ab21cfbbca0a89843151d389dcac8
parent 65422241d84b3087142adcf0906b5f86e69230e3
author Vincent Lin <vinlin24@outlook.com> 1668666720 -0800
committer Vincent Lin <vinlin24@outlook.com> 1668666720 -0800

Add 11/16 lecture notes
```

We see that every commit object contains information about its parent, author and committer.

You can think of `git log` (a *porcelain* command) as pretty-printing what `cat-file -p REF` (a *plumbing* command) tells us.

Three levels going on with every commit object:

```
(commit object)...
       |
       |                  +---------> (other objects)
       v                  |
(commit object) --> (tree object) --> (other objects)
       |                  |
       |                  +---------> (other objects)
       v
(commit object)...
```

Every commit points to a different tree, but the tree can share the objects they reference. When you make a commit for small changes, you don't have to rebuild a bunch of objects. Unchanged objects are *reused*.

However, because changing a file would update the tree containing it, changing a deeply nested file would require new tree objects all the way up to the project root for the new commit.

If a file is extremely large, Git can store a *diff* instead of a full copy and just remember how to restore the full content when the blob is needed.


## Internal Form for Blobs

Represents some byte stream `B`, say 47 bytes long. The form has a header that tells the type of the object, some control information like how large it is, and then the actual content.

```
+-+-+-+-+-+-+-+--+------------------------------+
|b|l|o|b| |4|7|\0|<--------------B------------->|
+-+-+-+-+-+-+-+--+------------------------------+
```

This string is then compressed using zlib, and that's the form it is stored in on the file system.

This string is also what is fed to the SHA-1 checksum algorithm.

**ASIDE:** Mathematicians in the past decade have been working to try and crack SHA-1 and have succeeded for the most part. SHA-1 is no longer "reliable", but the chance of a *random* collision (not one with file contents engineered to hash a collision) is still astronomically small.


# Compression

There are two big ideas used in compression programs like zlib, etc.


## (1) Huffman Coding

The best possible algorithm under its constraints:

1. Input is a sequence of symbols taken from a known alphabet.
3. We know the *popularity* of each input symbol.
4. Each input symbol corresponds to some bit stream.

When you have these constraints, you can assemble a table mapping input symbols to output bit strings, with lengths chosen based on how frequent they are. With ASCII characters in the context of the English language as an example:

| Symbol  | Bit String   |
| ------- | ------------ |
| (space) | 00           |
| e       | 010          |
| t       | 011          |
| a       | 100          |
| o       | 1010         |
| ...     | ...          |
| ^Q      | 111011011101 |

Notice that the least popular symbols could map to a representation that takes up even more space than their original form, but that's okay because they occur so scarcely that on average it does not hinder the total length by much.

Then, compression is simply a matter of iterating over the input symbols and performing a lookup.


### Decompressing

1. You have a pre-computed table shared by the compressor and de-compressor.
2. Sender computes the table and sends it first. This introduces some overhead in transmission, but more importantly, it has to read all of the input first.
3. **Dynamic Hoffman tables** - both sender and recipient start off with a table in their head that's perfectly balanced (for example, every character standing for itself). Sender then sends the first byte and updates its Hoffman table according to the byte that it already sent. The recipient does the same, and because both sides can detect the same popularity of the data they are sending/receiving, their tables are kept in sync. This is a little slower because they have to update the tables dynamically as they go but not by much. You also have to agree on what to do when there's a tie in popularity. Both sides need to be able to stay in lock step with each other.


### Table Generation Algorithm

Suppose you computed a table of probabilities (possibly by counting the frequencies of each symbol in some sample text):

| Symbol   | Probability |
| -------- | ----------- |
| (space)  | 0.1         |
| e        | 0.07        |
| t        | 0.05        |
| ...      | ...         |
| **SUM:** | 1.0         |

You build a **Huffman tree** to determine the bit string to assign. Review your MATH 61 notes lol.


## (2) Dictionary Coding

An approach that can be more optimal than Huffman coding by relaxing a constraint. Instead of mapping individual symbols to bit strings, you map sequences of symbols to bit strings.

For example:

| Sequence | Encoding |
| -------- | -------- |
| the      | 1        |
| a        | 2        |
| an       | 3        |
| or       | 4        |
| ...      | ...      |

You read the text, divide it into *words* (hence "dictionary" coding), and then for each word, you assign a number to it. Of course, *words* here refer to any byte streams, so they likely include punctuation marks or spaces for efficiency.

If the sender and recipient know this table, then a string can be compressed much more efficiently than when using Huffman coding.


### Decompressing

There is also **dynamic dictionary coding.** Start with a trivial dictionary with 256 entries, where each word is length 1 and represented with some number from 0-255. As data is transmitted, the tables on both side are updated dynamically and in the same way such that by the end, they can use dictionary coding.

Suppose you already sent a substantial chunk of data. You can send data in terms of what you already sent. For example, if you're about to send the word "French", and you've sent "French" 97500 bytes ago, you can send a pair of integers, offset and length, like (97500, 6), which the recipient can decode with their copy of the data.

There is also the **sliding window algorithm** where you have a moving range of the sent data that you can use for this approach. This is necessary because RAM has a limit.


## zlib

zlib uses both of these approaches. It uses dictionary coding to output a sequence of numbers, and then it uses Huffman coding on that sequence.

**Does compression guarantee you get something smaller than what you started off with?**

No. If this were true, than you can compress an indefinite number of times, all the way until 1 byte, which would then have to be compressed to 0 bytes under this assumption. This is obviously impossible to do without losing information.

**ASIDE:** A **general purpose compression algorithm** has to be able to work with *any* byte sequence. Certain algorithms can be specialized for certain types of input, but software like Git needs something general purpose like zlib because it has to be able to work with any blob.
