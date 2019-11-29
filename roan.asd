;;; Copyright (c) 1975-2019 Donald F Morrison
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

(in-package :asdf-user)

(defsystem :roan
  :version (:read-file-form "VERSION")
  :license "MIT"
  :author "Don Morrison <dfm@ringing.org>"
  :description "A library to support change ringing applications"
  :depends-on (:alexandria :iterate :cl-interpol :cl-ppcre
                           :plump :local-time :binascii :uuid
                           :cl-fad :drakma :zip :asdf)
  :components ((:file "package")
               (:file "readtables" :depends-on ("package"))
               (:file "util" :depends-on ("package"))
               (:file "roan" :depends-on ("package" "util"))
               (:file "pattern" :depends-on ("package" "util" "roan"))
               (:file "method" :depends-on ("package" "util" "roan")))
  :in-order-to ((test-op (test-op "roan/test"))))

(defparameter *xxx* nil)

(defsystem :roan/test
  :license "MIT"
  :author "Don Morrison <dfm@ringing.org>"
  :description "Unit tests for Roan"
  :depends-on (:roan :alexandria :iterate :lisp-unit2 :cl-ppcre :cl-fad)
  :components ((:file "tests")
               (:file "util-tests" :depends-on ("tests"))
               (:file "roan-tests" :depends-on ("tests"))
               (:file "pattern-tests" :depends-on ("tests"))
               (:file "method-tests" :depends-on ("tests"))
               (:file "blueline-tests" :depends-on ("tests")))
  :perform (test-op (o s)
             (declare (ignore o s))
             (uiop:symbol-call :roan/test '#:test-roan)))

(defsystem :roan/doc
  :license "MIT"
  :author "Don Morrison <dfm@ringing.org>"
  :description "Support for building the documentation for Roan"
  :depends-on (:roan :alexandria :iterate :trivial-documentation :cl-fad :cl-ppcre :asdf)
  :components ((:file "extract-documentation")))
