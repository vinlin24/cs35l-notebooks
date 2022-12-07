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

> In some sense, serverless computing is to server-side applications as event-driven programming is to client-side applications. **- Dr. Eggert**


## Virtual Machines (VMs)


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

Each VM *thinks* it has the full capability of a real machine. Of course, there's a performance problem

* The entire VM OS needs to boot.
* You'll need the resources to run every individual VM.
* If they're all running computations, they'd be independently consuming physical RAM.


## Method 2 (Containerization)


You assume that the virtual and physical machine have the same instruction set like before, but also that the virtual OS is the same as the physical OS, say Linux.

Consider the read-only file `/usr/bin/sh` sitting in the physical Linux system. All Linux VM instances can share this same shell instead of having identical copies. The VMs can even share the cached copy of the shell. This works because the file is read-only, and this applies to all other read-only files like the C library.

With this approach, you should be able to boot up a VM in less than a second.

> **Spy vs. Spy ASIDE:** Virtual machines are designed to make it very difficult to tell programmatically if you're in a VM. This guards against attackers attempting to check if they're running in a VM. One way to do it anyway is to use performance checking.


## Brief Docker Introduction


One of the most popular ways to containerize [(Method 2)](#method-2-containerization).


### dockerd


You start up a program called **dockerd**, the Docker daemon, which runs as `root`.

**ASIDE:** A **daemon** is like a behind-the-scenes server. The name is taken from Maxwell's daemon, a tiny creature that reverses entropy. According to Wikipedia:

> a daemon is a computer program that runs as a background process, rather than being under the direct control of an interactive user. Traditionally, the process names of a daemon end with the letter *d*...

The dockerd manages Docker **containers**, which are processes that correspond to some lightweight VM.

You make requests via the Docker Engine API. You can do so through the **command-line interface (CLI)**:

```shell
docker # options here
```


### Docker Objects


* **Containers** - environments to run applications.

* **Image** - a template to *build* a container, which contains an instructions on how to set up and run the container, including what files to be visible, what files to execute, etc.

* **Service** - lets applications run across container boundaries.


### Registries


Like repositories for Docker images.

You can **push** an image into a registry. You can then **pull** an image from a registry.


### Managing Docker


* Docker itself has a set of tools for managing containers, images, etc. like Docker Swarm.
* There's also **Kubernetes (K8s)** - a software generalized for other containerization.
* Amazon has its own service with ECS.
* RedHat has one too, OpenShift.


### General Issues

* **Load balancing**: distributing computation across many Docker instances running across multiple physical machines.

* **Resource constraints**: making sure your collection of containers doesn't go over your budget or your machines' physical limitations.

* Logging, monitoring, and debugging. You can even run tools like `gdb` and `strace` (both in and out of the VM) on your bad processes.

* Security, as usual.

* Deploying updates.

  * There's a technique called **blue green deployment**. As you update from "blue" to "green", you first run some green containers. You then have for some time blue containers and green containers running at the same time, and eventually you kill off the blue containers such that only green containers remain.
