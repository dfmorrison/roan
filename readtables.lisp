;;; Copyright (c) 1975-2026 Donald F Morrison
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :roan)

(defreadtable :roan
  (:merge :common-lisp)
  (:macro-char #\! 'read-row t)
  (:dispatch-macro-char #\# #\! 'sharp-bang-reader))

(defreadtable :roan+interpol
  (:merge :roan :interpol-syntax))

(defmacro roan-syntax (&optional (on-off t) (modify nil))
  "Turns on or off the read macros for @samp{!} and @samp{#!}, for reading rows and place
notation.

If the generalized boolean @var{on-off} is true, the default, it turns on these read
macros. Unless the generalized boolean @var{modify} is false, the default, it first pushes
the current read table onto a stack, modifying a copy of it and making that copy the
current read table. If @var{modify} is true it makes no copy and instead modifies the
current readtable in place.

If @var{on-off} is false it restores the previous readtable by popping the stack. If the
stack is empty it sets the readtable to a new, standard one. When @var{on-off} is false
@var{modify} is ignored.

This is performed in an @code{eval-when} context to ensure it happens at compile time as
well as load and execute time.

An alternative to using @code{roan-syntax} is to use
@url{https://github.com/melisgl/named-readtables/, Named Readtables}. Roan defines two
such readtables with names @code{:roan} and @code{:roan+interpol}. The former augments the
initial Common Lisp read table with Roan's read macros, and the latter also adds the
syntax from @url{http://edicl.github.io/cl-interpol/,CL-INTERPOL}."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%roan-syntax ,on-off ,modify)))

(defparameter *previous-readtables* nil)

(defun %roan-syntax (on-off modify)
  (cond (on-off
         (unless modify
           (push *readtable* *previous-readtables*)
           (setf *readtable* (copy-readtable)))
         (set-macro-character #\! 'read-row t)
         (set-dispatch-macro-character #\# #\! 'sharp-bang-reader))
        (t (setf *readtable* (or (pop *previous-readtables*) (copy-readtable nil)))))
  *readtable*)

(defun use-roan (&key (package *package*) (syntax t) modify)
  "A convenience function for using the @code{roan} package. Causes @var{package},
which defaults to the current value of @code{*package*}, to inherit all the external
symbols of the @code{roan} package, shadowing @code{method}, @code{method-name} and
@code{class-name}.

If the generalized boolean @var{syntax} is true, the default, it also enables use of
Roan's @samp{!} and @samp{#!} read macros, by calling @ref{roan-syntax} with a true first
argument; the value of @var{modify} is passed as a second argument to @ref{roan-syntax}.

Signals a @code{type-error} if @var{package} is not a package designator. Signals a
@code{package-error} if @var{package} is the @code{keyword} package.
@example
@group
 MY-PACKAGE> *package*
 #<Package \"MY-PACKAGE\">
 MY-PACKAGE> (package-use-list *)
 (#<Package \"COMMON-LISP\">)
 MY-PACKAGE> (rowp '!13276548)
 NIL
 MY-PACKAGE> (roan:use-roan)
 T
 MY-PACKAGE> +maximum-stage+
 24
 MY-PACKAGE> (rowp '!13276548)
 T
 @end group
 @end example"
  (when (eq (find-package package) (find-package :keyword))
    (error 'simple-package-error
           :format-control "Can't import Roan symbols into the keyword package."
           :package package))
  (prog1
      (unless (member (find-package :roan) (package-use-list package))
        (shadowing-import '(roan:method roan:method-name roan:class-name) package)
        (use-package :roan package))
    (when syntax
      (roan-syntax t modify))))

(define-condition simple-package-error (package-error simple-error) ())
