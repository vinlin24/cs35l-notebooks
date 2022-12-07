<!-- Week 10 Lecture Notes -->
<!-- I switched to '*' for bullet points instead of '-' ehe -->

**Table of Contents**

- [Backups](#backups)
  - [Failure modes](#failure-modes)
  - [Performing Backups](#performing-backups)
    - [Simple Version](#simple-version)
  - [What to Back Up?](#what-to-back-up)
  - [How to Back Up Cheaply](#how-to-back-up-cheaply)
    - [Incremental Backups](#incremental-backups)
- [Backup Optimization](#backup-optimization)
  - [Deduplication](#deduplication)
  - [Compression](#compression)
  - [Multiplexing](#multiplexing)
  - [Staging](#staging)
  - [Data Grooming](#data-grooming)
  - [Encryption](#encryption)
  - [Checksumming](#checksumming)
- [File Systems with Backups Built-in](#file-systems-with-backups-built-in)
  - [Versioning Approach](#versioning-approach)
  - [Snapshot Approach](#snapshot-approach)
- [Dependencies](#dependencies)
  - [Build-time Dependency](#build-time-dependency)
  - [Packaging Dependency](#packaging-dependency)
- [Cloud Computing](#cloud-computing)
  - [As a Marketing Term](#as-a-marketing-term)
    - [Problems](#problems)
  - [Other Marketing Terms](#other-marketing-terms)
    - [IAAS (Infrastructure as a Service)](#iaas-infrastructure-as-a-service)
    - [PaaS (Platform as a Service)](#paas-platform-as-a-service)
    - [SaaS (Software as a Service)](#saas-software-as-a-service)
    - [Serverless Computing](#serverless-computing)
- [Virtual Machines (VM)](#virtual-machines-vm)
  - [Method 0](#method-0)
  - [Method 1](#method-1)
  - [Method 2 (Containerization)](#method-2-containerization)
- [Brief Docker Introduction](#brief-docker-introduction)
  - [dockerd](#dockerd)
  - [Docker Objects](#docker-objects)
  - [Registries](#registries)
  - [How to Manage](#how-to-manage)
  - [General Issues](#general-issues)
- [Legal Issues of Software](#legal-issues-of-software)
  - [(1) Copyright](#1-copyright)
    - [Copyright License Types](#copyright-license-types)
  - [(2) Patent](#2-patent)
  - [(3) Trade Secrets](#3-trade-secrets)
  - [(4) Trademarks](#4-trademarks)
  - [(5) Personal Data](#5-personal-data)
  - [Enforcement Rules](#enforcement-rules)
    - [Infringement](#infringement)
    - [Technical Protection](#technical-protection)
  - [Software Licensing](#software-licensing)

---


# Backups

**DevOps** - "Development and Operations". The idea is that you have a single staff that does both development and the logistic stuff.

The philosophy of DevOps is that the same person should be responsible of the development and the operations of the system. Historically, the communication between the two are often not that good. If the same person knows both, they'll know when and how best to optimize.

A simple type of version control is the concept of **backups**.


## Failure modes

Backups provide ways to recover from **failure modes**:

* **Drive failure**: the physical hardware malfunctions.
  * Total failure: the entire drive is dead.
  * Partial failure: a few blocks go bad.
  * Corrupted data with no indication (a silent and very serious error!).
* **Operational failure**: someone responsible for maintaining the software makes a mistake, like erasing. **data**
* **External attackers**: hackers that break into your system, encrypt all your data, and hold it ransom.
* **Data corruption**: due to power outage
* **Cosmic rays**: can cause a bit flip in the RAM or even flash an open computer.
* etc.

> **ASIDE:** You can protect against bit flips by using **ECC RAM**. For every amount of bits, you have several **check bits** that you can use to test if a bit was flipped and correct it. All SEASnet servers and high-end work computers in general have ECC RAM. Of course, this increases the cost due to more testing required to ensure reliability.

Something to keep in mind for failure modes is not simply *what* can go wrong, but *how often* it can go wrong. This will dictate how often you should perform backups and what strategy to use.

One way to check for this is the **annualized failure rate (AFR)**. An AFR of 1% means the drive-maker thinks that if you use the drive in "the usual way" (some number of read/write operations), then on average 1% of the drives you use will fail in a year. These numbers are *estimates* based on similar drives in the past.

> **S.M.A.R.T.** is an interface you can use to interrogate a drive to determine things like how much it's been used and lifetime estimates.


## Performing Backups


**IMPORTANT:** Note that you every backup procedure should have a recovery procedure. You must also TEST your recovery procedures, or you might not be able to even restore your backup!!


### Simple Version

**Backup procedure**

Periodically, make a copy of your whole *state*. Of course, this is inefficient and *doesn't scale*.

**Recovery procedure**

Simply go to the most recent copy you made and copy it *into* your state.

You could also hold onto more than one copy at a time and go to copies even earlier than the most recent one when recovering. For example, if an attacker broke in several days ago, you should probably back up to a copy *before* that and be wary of any changes after the break-in event.


## What to Back Up?

RAM + registers vs. persistent storage? Ideally you want to recover both because that's the entire *state* of your system.

In reality, RAM + registers are hard to backup because they rapidly change and because there often aren't good software interfaces for doing this. Thus, usually we focus more on persistent storage.

You can perform backups at multiple "levels", with the higher ones being more *efficient* and the lower ones being simpler and more *general*.

**Application-level Backup**

Each application knows how to back up its own data and can tailor it to its own needs.

**File-level Backup**

You save:

* Contents of the files
* Metadata of the files
* Partitioning (file system layout)

**Block-level Backup**

All this is built on top of a lower-level system that's **block** based. You want to save all the blocks representing the above.

All files are typically stored as arrays of blocks, with each blocks representing things like directory listing, actual file contents, etc. You don't have to worry about what each block represents - simply back up all the blocks and you will have backed up the entire system!

---

Practically, most systems use a combination of these levels.

**ASIDE: Why split a drive into multiple areas?**

Example 1-drive system:

```
+---+------+-------+------
| / | /usr | /home | etc.
+---+------+-------+------
```

Maybe `/` has stuff needed to boot the system, `/usr` has non-essential system files, and then `/home` has the user's documents. This way, even if there were a problem with the `/usr` segment, the system would still be able to boot, and you can then recover `/usr` from there.


The blocks are physically partitioned on the drive, with each block corresponding to a specific part of the file system.

Within the file system, there'll be a low-level representation of the file contents like metadata about who owns it, etc.

---

**A problem**

**Do you bring the system down/idle to do backups?**

If **YES**, this greatly simplifies your backup strategy. This ensures that no one's changing the blocks *while* you're doing the backup.

If **NO**, your backup might not match any actual state because it'll be a *mixture* of some blocks from the old state and some blocks from the new state. This is sometimes called **live backups**, and this can be a real issue.

Suppose you did something like:

```shell
cd /home
tar -cf /backup/file1 *
# sometime later...
tar -cf /backup/file2 *
```

This creates a huge file `/backup/file1` containing every file under the `/home` directory. But if you do work *while* tar is working, the image that `/backup/file1` represents would be a mixture.

**Another consideration:**

How often should you reclaim storage from backups? How many backup files do you retain?

Obviously at least one. If the system crashes while backing up, then you just lost your only backup. Thus, you usually strive for at least two copies.


## How to Back Up Cheaply

* Do them less often.
* Do them to a cheaper device, like a hard drive or optical device.
* Do them to a remote service (this would involve some *trust* in the backup service).
* Instead of making an entire copy of your data, you do **incremental backups**


### Incremental Backups

**Backup procedure**

Only backing up the *changes* between the old and new changes.

* The first backup is everything
* Subsequent backups are only the differences

A block-oriented backup would only need to back up the changed blocks

**Recovery procedure**

* 1st copy: recover the full backup.
* Subsequent copies: apply the differences.

A limitation is that applying differences may take a while, so typically you would backup differences and periodically backup whole copies.

You could also *update* the backup with the differences, but this is like changing history, which introduces more concerns.

**How do you keep track of which blocks are changes?**

One approach is that with each block, you record the timestamp of when each block was changed. You'd have to record these timestamps somewhere, which means your data would be more complicated than a simple array of blocks. Some software does this anyway, where it sets aside some space for some metadata, checksums, etc.

Another option is to use the output of `diff` instead of the contents of the entire block. This is typically done at a higher level than the block level though.


# Backup Optimization


## Deduplication

Suppose you execute:

```shell
cp bigfile bigcopy
```

At the block level:

```
    bigfile         bigcopy
    v               v
+---+---+---+---+---+---+---+---+
|   | b | f |   |   | b | c |   |
+---+---+---+---+---+---+---+---+
```

You could cheat by just making `bigcopy` point to the same file object as `bigfile`:

```
    bigcopy
    bigfile
    v
+---+---+---+---+---+---+---+---+
|   | b | f |   |   | b | c |   |
+---+---+---+---+---+---+---+---+
```

From the user's POV, it *looks* like a copy even if you don't in reality.

When you modify `bigcopy`, your `bigcopy` just "remembers" (in its underlying data structure) which additional parts are part of the copy.

```
    bigcopy --+
    bigfile   |
    v         v
+---+---+---+---+---+---+---+---+
|   | b | f | m |   | b | c |   |
+---+---+---+---+---+---+---+---+
```

This has recently become the *default* behavior of the GNU `cp` command

This is known as **lazy copying**. Eventually, when you modify every block in your copy, *then* you make a copy of every block. Until, then you do it **lazily**, that is, *only as needed*.

This strategy has a name: **copy on write (CoW)**.

* (+) Speed
* (+) Less space
* (-) Not good for *backups* because if a block gets corrupted, it affects both the original and copy.
* (+) But this also means less underlying data when performing an actual backup.


`git clone` uses this strategy! An example of a backup at the *application level* too.


## Compression

Often done at the block level or the file level.

While the `-l` switch includes how many *bytes* used, the `-s` switch includes how many *blocks* are used to store the underlying file:

```shell
ls -ls file
```


## Multiplexing

Back up many different systems to a single backup device.


## Staging

Fast devices back up to slow devices:

```
flash --> disk --> optical tape
(fast)    (slow)   (even slower)
```


## Data Grooming

Remove old data that you don't need anymore before backing up.

It's tedious to determine which data isn't needed anymore, so a lot of this is now done automatically.


## Encryption

Guard against problems like attackers from the inside stealing and/or selling data to other agents.


## Checksumming

**What if your backup software is flakey?**

You make checksums of your data and then back up those checksums somewhere else. When you recover, you can check if the recovered data matches the checksusms.


# File Systems with Backups Built-in

Where old versions of files are always available.


## Versioning Approach

The file system keeps track of the old version of every file you create. It's as if:

```console
$ echo x > file
$ ls -l
---------------- file;1
$ echo y >> file
$ ls -1
---------------- file;1
---------------- file;2
```

**When to automatically create a new version?**

* Every time the file is opened for writing, differences made, and then closed
* Maybe there's some `newversion()` system call
* In the end, the *applications* decide when to version.

Still used in some software like OpenVMS, etc.


## Snapshot Approach

Every now and then, the underlying system takes a consistent snapshot of the entire file system.

It doesn't actually copy all the blocks because that would be too expensive. Instead, it uses CoW.

This is used in software like ZFS, btrfs, WAFL (SEASnet).


# Dependencies

You can think of them in terms of **dependency graphs**, which are typically (and ideally) *acyclic*.

Two ways dependencies come up in software construction:


## Build-time Dependency

"File x depends on file y." This can be expressed simply in Makefile syntax:

```makefile
x: y
    # create x; y is input
```

As a project scales, the dependency lists can get much longer.

```makefile
foo.o: foo.c x.h y.h z.h
    gcc -c foo.c $^
```

Maybe you extend the list based on what you see from the `#include` lines in the source files.

This can be problematic because you need to make sure that the dependencies in the Makefile *match* those of the source code, else it would slow down your building at best and break your it if a dependency is missing.

Thus, instead of maintaining dependencies *by hand*, you should *calculate* them with some `make` preprocessor - a tool to automatically generate your Makefiles for you.


## Packaging Dependency

Each package can be installed independently, BUT:

* Packages can depend on other packages
* Pnd every package can be version-specific.

This can get complicated, so ideally you'd want a **package manager**.

You can view the dependencies of a program with:

<!-- Special thanks to Victor Chinnappan for telling me what were the commands Dr. Eggert used. I didn't attend lecture that day and Bruincast didn't raise the camera. -->

```console
$ dnf deplist grep
Not root, Subscription Management repositories not updated
Last metadata expiration check: 1:16:41 ago on Mon 05 Dec 2022 04:57:02 PM PST.
package: grep-3.1-6.el8.x86_64
  dependency: /bin/sh
   provider: bash-4.4.20-4.el8_6.x86_64
  dependency: /sbin/install-info
   provider: info-6.5-7.el8.x86_64
  dependency: libc.so.6()(64bit)
   provider: glibc-2.28-211.el8.x86_64
  dependency: libc.so.6(GLIBC_2.14)(64bit)
   provider: glibc-2.28-211.el8.x86_64
  dependency: libc.so.6(GLIBC_2.2.5)(64bit)
   provider: glibc-2.28-211.el8.x86_64
  dependency: libc.so.6(GLIBC_2.3)(64bit)
   provider: glibc-2.28-211.el8.x86_64
  dependency: libc.so.6(GLIBC_2.3.4)(64bit)
   provider: glibc-2.28-211.el8.x86_64
  dependency: libc.so.6(GLIBC_2.4)(64bit)
   provider: glibc-2.28-211.el8.x86_64
  dependency: libc.so.6(GLIBC_2.5)(64bit)
   provider: glibc-2.28-211.el8.x86_64
  dependency: libpcre.so.1()(64bit)
   provider: pcre-8.42-6.el8.x86_64
  dependency: rtld(GNU_HASH)
   provider: glibc-2.28-211.el8.i686
   provider: glibc-2.28-211.el8.x86_64
```

And the dynamically linked libraries:

```console
$ ldd $(which grep)
linux-vdso.so.1 (0x00007fff5ab88000)
        libpcre2-8.so.0 => /lib64/libpcre2-8.so.0 (0x00007f365a1fd000)
        libc.so.6 => /lib64/libc.so.6 (0x00007f3659e38000)
        libpthread.so.0 => /lib64/libpthread.so.0 (0x00007f3659c18000)
        /lib64/ld-linux-x86-64.so.2 (0x00007f365a481000)
```

We notice that for example, `grep` requires `sh` because `egrep` utilizes a *shell script*. `grep` also needs the C library.


# Cloud Computing

> **ASIDE:** It's called "the cloud" because from the user/front-end's perspective, the "server" is some hazy concept that is no concern to them. They simply send requests to some back-end that dutifully processes them and respond with the desired information - it doesn't matter where or how they did it.


## As a Marketing Term

One standard definition is from the **NIST**. The capabilities of cloud computing include:

1. On-demand self-service: let you run stuff on their computers. This sets a **low barrier to entry**.
2. Rapid elasticity: you can immediately ask for more capacity. This provides **scalability**.
3. Resource pooling: share hardware resources among different projects. This **lowers cost**.
4. Measured service: the provider will bill users based on their usage. This instructs users how to **control their usage**.
5. Broad network access.

### Problems

* You have to *rely* (*trust*) that the cloud service provider fulfills their promises.
* Security and privacy.
* Providers have to make profit off you! If you're doing big computations, it's better for you to do them yourself on your own machines.


## Other Marketing Terms

From lowest level up:


### IAAS (Infrastructure as a Service)

The lowest level cloud service approach. The provider gives you ["access"](#virtual-machines) to some hardware and the network to connect to the hardware.

The hardware will be some server with some architecture (x86-64, ARM64, etc.). You can pick the operating system, and from there, you have a lot of control.

An example is the original AWS.


### PaaS (Platform as a Service)

The provider gives you access to:

* An operating system
* Architecture
* Middleware
* Database servers

An example is the Google App Engine.


### SaaS (Software as a Service)

The provider will give you access to all the above as well as all the software. You get the **full stack**.


### Serverless Computing

**SCENARIO:** You don't have to worry about the hardware or software. You don't *care* about what machine your application is running on - a level of abstraction.

"Serverless" comes from the notion of *not* maintaining one's state in RAM. Everything must be stored in the provider's persistent storage.

All the provider gives you is APIs you can interact with in **stateless programs**. These are applications that run short, fast computations when triggered by some external request, immediately save their state in the provider's database, and exit.

> In some sense, serverless computing is to server-side applications as event-driven programming is to client-side applications. - Dr. Eggert


# Virtual Machines (VM)

Building blocks for the *aaS systems.

Virtual servers are slower, but a lot cheaper. There are a few ways to build a VM:


## Method 0

Write a program, typically in C, that implements the instruction set of the architecture you're simulating.

```c
char *ip;
switch (*ip++) {
    // (these are made-up numbers)
    case 0x72:
        /* ADD instruction */
        break;
    case 0x73:
        /* SUB instruction */
        break;
    /* etc. */
}
```


## Method 1

You assume that the virtual machine and physical machine have the same instruction set. You let your program run on the actual hardware.

This can be problematic because the program can reach into the physical machine and modify it however it likes. We solve this by separating the instructions into two classes:

1. **User mode** instructions: the most popular instructions like `add`, `mul`, `mov`, `call`, etc.
2. **Kernel mode** instructions: more rare ones forbidden to the user, sensitive operations like I/O, permissions, etc.

Make it so that if the virtual machine attempts to execute a kernel mode instruction, it *crashes*. When it crashes, the physical machine takes over and figures out what to do and substitutes the action with something else.

The virtual machine could be running at full speed, but when it tries a kernel instruction, it **traps** into the kernel, and the kernel decides how to handle it and returns back to the VM.

A common technique is **virtual memory**, which maps virtual addresses to physical addresses. One scenario is when a virtual address doesn't map to a valid physical address, or one that doesn't belong to you, in which case the kernel takes over and most likely gives you the infamous `SIGSEGV` :D

With this approach, you can have different systems, like a MacOS VM atop a Linux PM.

Cloud service providers can do this too.

Each VM *thinks* it has the full capability of a real machine.

Of course, there's a performance problem. The entire VM OS needs to boot. You'll need the resources to run every individual VM. Also, if they're all running computations, they'd be independently consuming physical RAM.


## Method 2 (Containerization)

This is known as **containerization**.

You assume that the virtual and physical machine has the same instruction set like before, but also that the virtual OS is the same as the physical OS, say Linux.

Consider the read-only file `/usr/bin/sh` sitting in the physical Linux system. All Linux VM instances can share this same shell instead of having identical copies. The VMs can even share the cached copy of the shell. This works because the file is read-only, and this applies to all other read-only files like the C library.

With this approach, you should be able to boot up a VM in less than a second.

---

Virtual machines are designed to make it very difficult to tell programmatically if you're in a VM. This guards against attackers attempting to check if they're running in a VM. One way to do it anyway is to use performance checking.


# Brief Docker Introduction

One of the most popular ways to containerize [(Method 2)](#method-2-containerization).


## dockerd

You start up a program called **dockerd**, the Docker daemon, which runs as `root`.

**ASIDE:** A **daemon** is like a behind-the-scenes server. The name is taken from Maxwell's daemon, a tiny creature that reverses entropy. According to Wikipedia:

> a daemon is a computer program that runs as a background process, rather than being under the direct control of an interactive user. Traditionally, the process names of a daemon end with the letter *d*...

The dockerd manages Docker **containers**, which are processes that correspond to some lightweight VM.

You make requests via the Docker Engine API. You can do so through the **command-line interface (CLI)**:

```shell
docker # options here
```


## Docker Objects

* **Containers** - environments to run applications.

* **Image** - a template to *build* a container, which contains an instructions on how to set up and run the container, including what files to be visible, what files to execute, etc.

* **Service** - lets applications run across container boundaries.


## Registries

Like repositories for Docker images.

You can **push** an image into a registry. You can then **pull** an image from a registry.


## How to Manage

Docker itself has a set of tools for managing containers, images, etc. like Docker Swarm.

There's also **Kubernetes (K8s)** - a software generalized for other containerization.

Amazon has its own service with ECS.

RedHat has one too, OpenShift.


## General Issues

* **Load balancing**: distributing computation across many Docker instances running across multiple physical machines

* **Resource constraints**: making sure your collection of containers doesn't go over your budget or your machines' physical limitations

* Logging, monitoring, and debugging. You can even run tools like `gdb` and `strace` (both in and out of the VM) on your bad processes.

* Security

* Updates
  * There's a technique called **blue green deployment**. As you update from "blue" to "green", you first run some green containers. You then have for some time blue containers and green containers running at the same time, and eventually you kill off the blue containers such that only green containers remain.


# Legal Issues of Software

Software has 2 roles:

1. A set of instructions for a computer.
2. A way of collaborating to solve problems.

There are 5 major categories of software law:


## (1) Copyright

Originally designed for books + text, it gives the creator an *exclusive* right over making copies for some time. Typically it's the lifetime of an author plus about 75 years.

Copyright protects *form*, not the *ideas*. You could create a social media platform similar to Facebook as long as its *details* aren't exactly like it.

### Copyright License Types

0. **Public domain**: you have permissions to do whatever you want with the work. When a copyright expires, it goes into public domain.

1. **Academic**: require that you give credit. Classic examples include:
   1. **BSD** (originating from Berkeley)
   2. **MIT** (originating from MIT)

2. **Reciprocal**: require that you be as generous as the original author. A classic example is **GPL**, the GNU Public License.

3. **Corporate**: "sharing but public". Some examples are Apple, Eclipse. You can copy the source code, but if you make changes, you have to give them back to the owner.

4. **Secret**: the source code is not shown. You have the right to run the program and nothing else. This is the most common license in the commercial world.

When building an application, your individual modules may come with a mixture of licenses.


## (2) Patent

Originally applied to mechanical inventions, then chemical, and in general, *practical, novel* ideas.

Patents cover not just form but also ideas. Gives a monopoly for typically about 7 years.


## (3) Trade Secrets

Applies to ideas that you *don't* publish, things a company keeps secret. A popular example is the formula for Coca-Cola!

These are formalized with **nondisclosure agreements (NDAs)**. Industry workers have to sign such agreements before being hired.

> [You should *always*] read the fine print - there's no standard NDA. If someone tells you there's a standard, don't believe it - Dr. Eggert


## (4) Trademarks

Trademarks aim to avoid confusion among consumers.

You don't want a certain brand name to be confused for another.


## (5) Personal Data

For example, HIPPA protects the personal information of medical patients.


## Enforcement Rules


### Infringement

*Determining* the infringement is a matter of its own.

The enforcement mechanism is a **civil suit**. This is **legal protection**, meaning it's slow and expensive.


### Technical Protection

Thus, you also want **technical protection**, technical means to *prevent* infringement before it happens. For example:

* You keep the source code secret. That makes it harder for people to copy your system.
* Use SaaS such that the service provider has the software and runs it, with the source code and executables hidden from the end user.
* **Obfuscation** - deliberately use a terribly efficient/mangled version of the executable so it's very hard to reverse-engineer.


## Software Licensing

**License**

* It's NOT a contract. A contract is an *exchange*.
* It's a *grant* of permission.
* It's often *part* of a contract.
* The grant often has strings attached (read the fine print!).


<!-- TODO: NOTES NOT POLISHED YET -->
