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

(defsystem :roan-base
  :version (:read-file-form "VERSION")
  :license "MIT"
  :author "Don Morrison <dfm@ringing.org>"
  :description "A library to support change ringing applications, sans the method library support"
  :depends-on (:alexandria :iterate :trivial-garbage :cl-ppcre :s-base64 :asdf)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "roan" :depends-on ("package" "util"))
               (:file "pattern" :depends-on ("package" "util" "roan"))
               (:file "method" :depends-on ("package" "util" "roan"))))
