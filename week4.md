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

**Circuit switching**

- System is connected to the nearest central office, which can connect to other central offices. In the end, you get a path between the one computer to the other.
- However, you have temporary ownership of a wire during the transaction.

**Packet switching**

- Connected to a little computer that breaks the signal into a bunch of small messages called **packets**. Each packet is sent to a local **router** that sends the packets along the network to the destination. Each packet travels independently and possible along different paths, and they do so very quickly, so it does not back up the network
