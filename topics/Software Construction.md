# Software Construction


## Software Engineering Concerns


* **Non-technical issues**
  * Fundraising
* **Technical issues**
  * Security
  * Database of contacts
  * Where to store data
  * UI/UX
  * Recording friends' contact info
  * Network connectivity
  * Deployment
  * Portability


## Software Construction Issues


* File systems use (data)
* Scripting (programming)
* Integration
* Configuration
* Testing
* Versioning/evolution
* Low-level debugging (GDB, linking)
* Client-server model


## Application Objectives


One should strive for applications that:

* Make the reasonable distinction between what is **persistent** and what is **volatile**
* Are fast
* Are understandable to developers
* Are understandable to users

> * **Persistent** (aka **nonvolatile**): Describing data that is stored in the flash, drive, etc. (secondary storage). This data continues to exist even when the machine loses power through means like encoding itself on magnetic tape.
> * **Volatile**: Describing data that is stored in RAM. This data can be processed very quickly, but because its state is encoded in the circuitry, this data is lost when power is lost.


## Software Philosophies


### Software Tools Philosophy


**Don't write a big program intended to solve all your problems.**

Instead, write your application using a collection of tools, each of them relatively simple, and each tailored to solve one class of problems really well.

One can argue that languages like JavaScript fall under this category because you build programs out of smaller modules. JS itself is definitely not a little language, but the design philosophy has users put together small parts of it that individually do its job well to ultimately construct a more complex program.


### Little Languages Philosophy


**Design small languages appropriate for each tool.**

As something grows and grows, it gets too complicated and people can't figure out how it works, and as new applications come out, it becomes less appropriate for that app, so don't *let* your languages scale.

Basically the opposite of C++, a huge language that attempts to solve all classes of problems (*general-purpose programming languages* in general).

**Examples:** `sh`, `sed`, and `grep` are specialized programs that all come with their own little languages.

* **Downside:** for each tool you want to become an expert in, you have to learn a new language.
* **Upside:** each language is very simple and does its job well.
