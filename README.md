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

* a searchable database of method definitions, together with a mechanism for
  updating that database from the [ringing.org web site](http://www.ringing.org/)

* a function for extracting false course heads from common kinds of methods

* there's more in the works, just not ready for public release yet

Roan is distributed under an MIT open source license. which means that you can just use Roan for nearly anything you like.
See the file `LICENSE` for details.

### Installing Roan

While [Quicklisp](http://quicklisp.org) is not required to run Roan,
it is recommended. Quicklisp's `quickload` function will also pull in
all the other libraries upon which Roan depends; if you don't use
Quicklisp you will have to ensure that those libraries are available
and loaded. The following assumes that you have Quicklisp installed
and configured in the usual way.

After installing Quicklisp
either clone this Bitbucket repository or download and unzip
[the zip file of its contents](https://bitbucket.org/dfmorrison/roan/downloads/latest-version.zip "Latest Version").
Move the resulting directory somewhere Quicklisp has been configured to look for systems. By default it looks in

`~/common-lisp/`

`~/quicklisp/local-projects/`

`~/.local/share/common-lisp/source/`

After that's done executing in Lisp `(ql:quickload :roan)` will download and make available all the other libraries
on which Roan depends, and then load Roan itself.

Roan has been tested with

* [CCL](http:ccl.clozure.com "Clozure Common LIsp") version 1.11 (64 bit), on Ubuntu Linux 16.04 and macOS 10.13.5

* [SBCL](http://sbcl.org "Steel Bank Common Lisp) (64 bit) version 1.3.18 on Ubuntu Linux 16.04 and version 1.2.11 on macOS 10.13.5

* [CLISP](http://clisp.org)  version 2.49 on Ubuntu Linux 16.04

but it should also work in other, modern Common Lisp implementations that
support the libraries on which Roan depends.

### Documentation

The [Roan manual](https://bytebucket.org/dfmorrison/roan/wiki/roan.pdf "Roan manual PDF version") is available online.
It can also be downloaded in four different formats:

* [a PDF file](https://bitbucket.org/dfmorrison/roan/downloads/roan.pdf)

* [a single HTML page](https://bitbucket.org/dfmorrison/roan/downloads/roan-html-single-page.zip)

* [multiple HTML pages](https://bitbucket.org/dfmorrison/roan/downloads/roan-html-multiple-pages.zip)

* [an Info file](https://bitbucket.org/dfmorrison/roan/downloads/roan-info.zip)

### Reporting Bugs

The best way to report bugs is to submit them with the
[BitBucket issue tracker](https://bitbucket.org/dfmorrison/roan/issues)
If that doesn't work of you you can also send mail to Don Morrison <dfm@ringing.org>.

