## Roan

Roan is a libary of Common Lisp code for writing applications related to
[change ringing](http://www.ringing.org/change-ringing "A Brief Description of Change Ringing")
It is roughly comparable to the
[Ringing Class Library](http://ringing-lib.sourceforge.net/)
although that is for the C++ programming language, and the two libraries differ in many
other ways.

Roan provides

* facilities for representing rows and changes as Lisp objects, and permuting them, etc.

* functions for reading and writing place notation, extended to support jump changes as well
  as conveniently representing palindromic sequences of changes

* a set data structure suitable for collecting and proving sets of rows, or sets of sets of rows

* a pattern language for matching rows, for example, for identifying ones with properties
  considered musically desirable; and that includes the ability to match pairs of rows,
  which enables identifying wraps

* a data structure for describing methods, which can include jump changes,

* a searchable library of method definitions, together with a mechanism for
  updating that database from the [CCCBR Methods Library](https://cccbr.github.io/methods-library/)

* a function for drawing blue lines of methods as Scalable Vector Graphics (SVG) images

* a function for extracting false course heads from common kinds of methods

* a representation of calls, allowing replacing, deleting or adding one or more changes to a
  plain lead of a method, at any point within that lead, and possibly spanning two leads as
  is done in doubles variations.


Roan is distributed under an MIT open source license, which mostly means that you can just use Roan for nearly anything you like.
See the file `LICENSE` for details.

### Installing Roan

While [Quicklisp](http://quicklisp.org) is not required to run Roan,
it is recommended.
With Quicklisp installed and configured, you can download and install Roan by simply
executing in Lisp `(ql:quickload :roan)`.

Quicklisp's `quickload` function, above, will also pull in all the other libraries
upon which Roan depends; if you don't use Quicklisp you will have to ensure that those
libraries are available and loaded. If you don't want to use Quicklisp, and prefer to load
Roan by hand, the repository for Roan itself is at
[https://bitbucket.org/dfmorrison/roan](https://bitbucket.org/dfmorrison/roan), and
both current and previous versions can be downloaded from the tags pane of the Downloads
page, [https://bitbucket.org/dfmorrison/roan/downloads/?tab=tags](https://bitbucket.org/dfmorrison/roan/downloads/?tab=tags).

Note that Quicklisp creates a new distribution about once a month, so there may be a log
of that duration between when a new version is available in the Bitbucket repository and
when that version is available in Quicklisp. If you need it sooner, you may need to
download it yourself from Bitbucket.

Roan has been tested with

* [CCL](http:ccl.clozure.com "Clozure Common LIsp") version 1.11.5 (64 bit), on Ubuntu Linux 18.04.3

* [SBCL](http://sbcl.org "Steel Bank Common Lisp) version 1.5.5 (64 bit) on Ubuntu Linux 18.04.3

but it should also work in other, modern Common Lisp implementations that
support the libraries on which Roan depends.

### Documentation

The [Roan manual](https://ringing.org/roan-manual.pdf "Roan manual PDF version") is available online.
It can also be downloaded in four different formats:

* [a PDF file](https://bitbucket.org/dfmorrison/roan/downloads/roan-manual.pdf)

* [a single HTML page](https://bitbucket.org/dfmorrison/roan/downloads/roan-manual-single-page.html.bz2)

* [multiple HTML pages](https://bitbucket.org/dfmorrison/roan/downloads/roan-manual-multiple-pages-html.tar.bz2)

* [an Info file](https://bitbucket.org/dfmorrison/roan/downloads/roan-manual.info.bz2)

### Reporting Bugs

The best way to report bugs is to submit them with
[Roan's Bitbucket issue tracker](https://bitbucket.org/dfmorrison/roan/issues)
If that doesn't work for you you can also send mail to Don Morrison <dfm@ringing.org>.

