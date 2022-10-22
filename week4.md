# Python (Continued)

## Python OOP - Classes

Class hierarchies (especially apparent with *multiple inheritance*) are **directed acyclic graphs (DAG)**.

**Method Resolution Order (MRO):** Depth-first, left-to-right. So for example, if you define a class that inherits like so:

```python
class C(A, B):
    def some_method(self, arg):
        pass
```

With the DAG model, this design makes it so that if `A` and `B` disagree, `A` will always take priority.

The decision to explicitly include `self` in all method definitions was to not abstract a fundamental mechanism of OOP: every method is *bound* to the class and *acts on* the instance. If you examine the machine code of similar OOP languages like C++, you'll see that there's a hidden first argument to every method, that is the pointer to the object that the method is acting *on behalf of*.

## Dunders and "Operator Overloading"

Besides the ones you already know...

To redefine the comparison operators with one method:

```python
def __cmp__(self, other: object) -> int:
    # negative for <, 0 for equal, positive for >
    return num
```

This is still supported but it is however an anachronistic approach because you can run into hardware problems, like comparing floating point numbers. Thus, we have the familiar `__lt__`, etc.

This is the Python 2 predecessor to the familiar `__bool__` method:

```python
def __nonzero__(self) -> bool:
    # Return whether the object is considered to be "not zero"
    return b
```

## Namespaces in Classes

**Namespaces** are just dictionaries. Classes have a special method `__dict__` that returns `dict` that maps names to values. This gives rise to opportunities to write "clever" Python code, where you can programmatically alter the definition of an existing class:

```python
c = C()
c.__dict__["m"] is c.m
```

This is (probably) how *metaclasses* are implemented.

## Modules

Analogous to a compiled C++ source file.

### The Finer Details of the `import` Statement

1. Creates a namespace for the module.
2. Executes the contents of the module *in the context of that namespace*, *if haven't already* (modules are only run once).
3. Adds a name, the module name, to the current namespace.

## Packages

Organize source code in a familiar tree structure. Allows importing to be analogous to the file system.

The special `__init__.py` turns a directory into a proper package, and it is automatically run upon import.

### The `PYTHONPATH` Environment Variable

Just as how `PATH` instructs the shell program where to search for commands, `PYTHONPATH` instructs Python where to search for code.

Determines the behavior of the `import` statement. Python will search through the sequence of paths, delimited by colons (Unix) or semicolons (Windows), to search for names of packages or modules to import. The path to the standard library is included in `PYTHONPATH` by default.

Official documentation: https://docs.python.org/3/using/cmdline.html#envvar-PYTHONPATH.

This variable is stored in and can be modified with `sys.path`, which is a `list[str]` containing the individual string paths.

### Why all this complexity? Packages vs Classes?

> Packages are oriented towards developers (compile-time notion). The tree is structured so that different developers can work on different parts of the code.
>
> Classes are about runtime behavior (runtime notion). You want inheritance to be independent of package hierarchy. Classes are only concerned with their own behavior, "what to do next", so it should be able to pull code from anywhere in the codebase. How developers *organize* that codebase is made possible with packages.

# Client-Server Computing

**Common Concepts between Node+React and POSIX:**

- Quoting
- Configuration

## Alternatives to the Client-Server Model

Definition of Client-Server:

```
+----------+         +------------+
| browsers | <-----> | web server |
+----------+         +------+-----+
                            |
                         +--+--+
                         | DB  |
                         +-----+
```

0. **Single computer** (CS 31 assignments - "you assume it's a solved problem")

1. **Peer-to-peer (P2P)**: Decentralized approach. If a peer doesn't have a resource, the request is redirected to another resource. The main advantage of this is that it is more fault-tolerant: a single peer going down doesn't bring the system down. The downside is that it is more involved to maintain a consistent state across every peer. This is in contrast to the less fault-tolerant but more state-consistent client-server model.

2. **Primary secondary**: One primary machine that serves as the "overseer" - it keeps track of how the application is split up among numerous secondary servers. The secondary servers receive a small "subproblem" of the application from the primary server and return any results.

## Performance Issues

Traditional programming performance metrics:

- CPU time: how many CPU instructions executed (roughly proportional to the amount of energy consumed)
- Real time: amount of time elapsed
- RAM
- I/O

Networking performance metrics:

- **Throughput**: Number of client requests per second that the system can handle (assuming individual requests and responses are reasonably small and approximately equally sized); bigger is better.
- **Latency**: Delay between a request to the server and the response back from the server; smaller is better.

To improve **throughput**:

- You can perform actions "out of order" (compared to "request order").
- You can perform actions *in parallel*.

To improve **latency**:

- You can use **caching**.

## Client-Server Correctness Issues

Throughput fixes:

- Out-of-order execution, can "mis-order" transactions.

Latency fixes:

- **Stale caches**, requires **cache validation**, which could be an expensive operation.

## Task Networking Styles

### Circuit Switching

- System is connected to the nearest central office, which can connect to other central offices. In the end, you get a path between the one computer to the other.
- However, you have temporary ownership of a wire during the transaction.

### Packet Switching

- Connected to a little computer that breaks the signal into a bunch of small messages called **packets**. Each packet is sent to a local **router** that sends the packets along the network to the destination. Each packet travels independently and possible along different paths, and they do so very quickly, so it does not back up the network

# The Internet

## Packet Switching

Idea originally proposed by Paul Baran (1960s, RAND).

Sold to Department of Defense as a reliable way to communicate in the event of nuclear war. *Rerouting* in the event of an office goes down gives it an edge over traditional **circuit switching**.

Packet switching is a best effort transmission/no guarantees (circuit-switching is a *guarantee* between the two machines)

**Problems:**
- Packets can be lost - if some router along the way gets overloaded, some packets may be discarded
- Packets can be received out of order
- Packets can be duplicated via a mechanism called **bridges**
- All routing decisions are local to the individual routers, so packets may be directed into a loop

## Internet Protocol (IP) Suite

A *protocol* is simply a set of rules. This set of rules forms the foundation of the Internet.

Basic idea: **layering**. A layer of protocols build on top of each other.

Working from bottom to top:

### Link Layer

Point-to-point.

The bits and bytes you send on a single link from one node to another (no routers in between). Very hardware oriented. Each technology used has its own link layer protocol.

### Internet Layer

Packets.

This is where the [IP](#ip-continued) is specified.

### Transport Layer

Data channels.

For large data *streams*, like a TB of data, that cannot be individual packets. This layer oversees how a *stream of packets* is transmitted.

### Application Layer

Application dependent: web, voice, etc.

## IP (Continued)

Software almost never operate directly at this level because it is too low level.

IP comes in different versions:

### IPv4

- Created in 1983. specified by Jon Pastel (UCLA), etc. Involves packets, connection.
- We are running out of IPv4 addresses because there are only about 4 billion, and the US has most of them.

#### Packet Anatomy

A packet is just a sequence of bytes. The **payload** is prefixed by a **header** that stores *metadata* like:
- Length
- Protocol number (to support any protocol that ends up being built atop IP) tells you the *type* of packet that's being transported so algorithms can determine their priority. For example, a single video frame is much less important than part of a image file.
- Source IP address (a 32-bit number often expressed in Base 256)
- Destination IP address (ibid)
- A **checksum** (16-bit)
- A time-to-live (TTL) field (8-bit "hop count" that keeps track of how many routers it goes through; packets with abnormally high TTL values get dropped to prevent packet loops)

When routers receive packets, they look at the header, especially the destination address, to determine what to do with it.

### IPv6

- Created in 1996.
- 128 bit IP addresses.
- The headers are also longer.
- A superset of IPv4; the 32 bits of IPv4 can be mapped to 32 bits of an IPv6 address, so IPv4 users can communicate with IPv6 users. The converse is not as simple but made possible with complicated translation techniques.
- Less efficient because the extra length is overhead for the link layer

### UDP: User Datagram Protocol

- Created by David Reed at MIT.
- Designed as a thin layer over IP, but still at the Internet layer.
- You use UDP if your application sends single short messages over the Internet without much care if it is lost, duplicated, etc. Intended for apps that *want* to deal with packets.

## Transport Layer: TCP (Transmission Control Protocol)

- Vint Cerf from Stanford, Bob Kahn from Princeton

Looks like a *stream of data* that:
- Is reliable (via **acknowledgements**)
- Have **sequence numbers** for packets
- Is ordered (the recipient reassembles the packets that may be out of order in the lower layers)
- Is **end-to-end error-checked**

The protocol has:
- Flow control; sends packets at the correct rate to not overload the network
- Retransmission
- Reassembly

A single machine can listen to multiple TCP channels on different **ports**.

## Application Layer

### RTP (Realtime Transmission Protocol)

Runs atop UDP because TCP is not suited for sending realtime data. TCP would cause **jitter**.

### HTTP (HyperText Transfer Protocol)

Runs atop TCP because reliability is a must.

Tim Berners-Lee at CERN in 1991.
- Invented the Web, HTTP and its **request-response protocol**:
  - Create a TCP connection (default port 80)
  - Client sends the server a GET message
  - Server responds with the contents of the webpage
- Invented **HTML (HyperText Markup Language)**: a way to express the contents of a webpage in a machine-independent format.

The Internet fundamentally is just HTML and HTTP combined together. HTML is like the content of the Internet, HTTP is how it gets around.

You can use the `telnet` command to open a 2-way stream where you can send raw HTTP requests. Run `telnet` with the IP address and the port to connect to. THen enter the `GET` command with the resource you want to get and the protocol version to use. Then specify the `Host`:

```console
$ host www.cs.ucla.edu
WWW.cs.ucla.edu has address 164.67.100.182
$ telnet 164.67.100.182 80
Trying 164.67.100.182...
Connected to 164.67.100.182.
Escape character is '^]'.
GET / HTTP/1.1
Host: www.cs.ucla.edu

HTTP/1.1 301 Moved Permanently
Date: Wed, 19 Oct 2022 22:21:10 GMT
Server: Apache
X-Frame-Options: SAMEORIGIN
Location: https://www.cs.ucla.edu/
Content-Length: 232
Content-Type: text/html; charset=iso-8859-1

<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
<html><head>
<title>301 Moved Permanently</title>
</head><body>
<h1>Moved Permanently</h1>
<p>The document has moved <a href="https://www.cs.ucla.edu/">here</a>.</p>
</body></html>
```

The actual HTML data is prefixed with a response **header**, detailing metadata like the response code, content length, etc.

### HTTPS (HTTP Secure)

With plain HTTP, every router between the source and destination can see the raw data that is transmitted.

Uses shared private keys or something.

### HTTP/2

Came out in 2015.

Added extra features to HTTP:
1. Header compression
2. Server push (lets the server send a response without a request)
3. Pipelining (allows client to send multiple requests so the server can respond in batches, allows more *parallel* communication instead of having the client wait for a response every single time)
4. Multiplexing (talk to multiple websites over one TCP channel)

### HTTP/3

Not released yet.

1. Now based on UDP instead of TCP (motivated by the increase in voice/video services)
2. Uses even more multiplexing
3. Avoids jitter by avoiding **head-of-line** blocking delays; allows content after a dropped packet to be delivered (?)

---

# Data Languages

HTML is an example of a data language.

## SGML (Standard General Markup Language)

Markup language for documents (1980s).

**Declarative** (vs. **imperative**). **Markup** specifies the structure and attributes of a document.

<!-- HTML for highlighting, SGML doesn't seem to be supported -->
```html
<QUOTE TYPE="example">
    OK <ITALICS>This text</ITALICS> is
    part of a block quote intended for example.
</QUOTE>
```

Extensible bracket types allow you to define new features without making breaking changes to the language. `<QUOTE>` is one type of bracket with its closing tag `</QUOTE>`, `<ITALIC>` is another, etc.

The structure of the document forms a tree structure: the content inside `<ITALICS>` is a child node to `<QUOTE>`.

# Week 4 Discussion: Midterm Review

## Pattern Matching

### Glob (Wildcards)

Used in file management: `man 7 glob`

- `?` - match one, any character
- `*` - match any number of characters, including the empty string
- `{pdf, jpeg}` - match multiple literals
- `[]` - character set that supports *ranges*, similar to regex
- `[!]` - complement of a character set if `!` is the first character
- `\` - escape character


## Permissions

- 3 permissions: read (r), write (w), execute (x)
- 3 user groups: user (**owner)**) (u), group (g), others (o)
- Each 0-7 in numerical modes (one octal digit), for a total of 3 octal digits for a file (e.g. 755 for `rwxr-xr-x`)

A set of rwx permissions on a file is called "**sensible**" if the owner has al the permissions of the group and the group has all the permission of others. For example:

- 551 (r-x|r-x|--x) is sensible. The owner's permissions are a *superset* of those of the group.
- 467 (r--|rw-|rwx) is not. The group has `w` permission while the owner doesn't.

Non-sensible permissions don't make sense because the *owner* is considered to be the most closely related to the file, so they should have the most access.

**How many distinct sensible permissions are there?**

Solution:

Consider r, w, x permissions separately. For each of then, there are 4 ways to gst sensible permissions for u, g, and o (in binary, 000, 100, 110, 111).

Total: $4^3=64$

## Links

- **Soft link** (**symbolic link**) is only a pointer
- **Hard link** creates a copy of the target file
- **Dangling link**: a soft link that points to a nonexistent file

**Give an example of how renaming a dangling symbolic link can transform it into a non-dangling symbolic link.**

Soft links can be linked to a relative path. Suppose `c` has content `b`, but `b` only exists as `temp/b` relative to the current working directory. We can use `mv c temp` to make the relative path `b` now work since `c` is now in the same directory as `b`.

## Emacs

**Explain how to arrange for Emacs to treat C-t as a command that causes Emacs to issue a message like this:**

*some time string I missed it lol*

**In the echo area.**

1. Define a function.
2. Use `current-time-string` to get current time, and use `concat` to concetatne time with other text
3. Make the function `interactive`
4. Use `message` for output in the echo area
   ```lisp
   (defun print-time ()
       (interactive)
       (message (concat "It is now" (current-time-string) ".")))
   ```
5. Create the `C-t` key binding:
   ```
   M-x global-set-key C-t print-time RET
   ```

## Shell

- **grep**: search for text that matches the give pattern
- **sed**: streaming text editor
- **shuf**: randomly shuffle the input
- Maybe also **awk**: a scripting language for editing text

### Shell Script

Looping:

```sh
for i in {1..100}
do
    echo $i
done
```

You can use the `expr` command to evaluate an expression. You can also use *compound expansion* with `((expression))` syntax.

Conditionals

```sh
if [ condition ]
then
    # body
fi
```

**What does this script do?**

```sh
#!/bin/sh
atom='[a-zA-Z0-9]+'         # at least 1 alphanumeric character
string='\\"([^"\]|\\.)*\\"' # what the fuck
word="($atom|$string)"
words="$word(\\.$word)*"
grep -E "$words" | grep ' '
```

This script wil print any line in the standard input that satisfies both of the following:
- Contains a space
- Contains a sequence of words specified by `$word`

For `$word`, it matches:
1. An atom character specified by `$atom`, which is just one or more alphanumeric characters
2. A sequence of non-double-quote characters that are enclosed with `\"` and if backslash appears, it must be followed by another character (to form an escape sequence)

**What if the `-E` flag is removed?**

BRE is used instead of ERE. Many meta-characters including `(` `|` `)` will then be treated as normal characters.

**How to modify script to only use grep once?**

Use `sed`:

```sh
grep -E "$words" | sed -n '/ /p'
```

The captive space means match the space and the `p` means print.

## Practice Midterms

**What is the main difference between a hard link and a symbolic link?**

A hard link increments the file object's reference count. A symbolic link does not increment the file's reference count.

Deleting all hard links to a file (and ceasing all operations that use the file) deletes the file. Deleting symbolic links do not affect the underlying file. A symbolic link can become dangling if the underlying file is deleted.

**Consider the sell command being executed on a device named ubun3 by user groot:**

```console
ubun3:~/zoo-wee-mama groot$ a b < c | ./d > e
```

This executes the command named a with b as an argument and the file c as its input. The output of a is then piped into the d file in the current directory. The standard output of the d file is redirected to a file named e in the current directory, creating it if necessary, every time it receives pipeline input.

The programs being executed are a, which is presumably on the user's `PATH`, and d, which is in the current directory.

**Write an ERE that only matches numbers between 0 and 255.**

```
^(25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9][0-9]|[0-9])$
```

Make sure to remember to include `^$`! That constrains the number of digits you can match, and more importantly, it ensures no partial matching.

<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({ tex2jax: {inlineMath: [['$', '$']]}, messageStyle: "none" });
</script>
