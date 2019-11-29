;;;; Copyright (c) 1975-2019 Donald F Morrison
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;;; software and associated documentation files (the "Software"), to deal in the Software
;;;; without restriction, including without limitation the rights to use, copy, modify,
;;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included in all copies
;;;; or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :roan/test
  (:shadowing-import-from :roan #:method #:method-name #:class #:class-name)
  (:shadowing-import-from :alexandria #:set-equal)
  (:use :common-lisp :alexandria :iterate :roan :lisp-unit2 :asdf)
  (:import-from :named-readtables #:in-readtable #:find-readtable)
  (:import-from :roan
                #:%method-classification
                #:%extreme-hash-set
                #:*fch-groups-by-course-head*
                #:*blueline-default-parameters*
                #:*blueline-method*
                #:*fch-groups-by-name*
                #:*method-library*
                #:*method-library-path*
                #:*pattern-cache*
                #:cache-count
                #:call-following
                #:call-fraction
                #:call-from-end
                #:call-offset
                #:call-replace
                #:clear-method-traits
                #:clrcache
                #:collapse-whitespace
                #:cross-sections
                #:get-call-changes
                #:get-newest-key
                #:get-newest-value
                #:getcache
                #:jump-allowed-path-little-p
                #:lru-cache
                #:make-lru-cache
                #:parse-method-title
                #:partition-hunt-bells
                #:path-attributes-non-jump
                #:remcache
                #:replace
                #:rotate-cycle
                #:same-type-p
                #:with-initial-format-characters
                #:with-warnings-muffled)
  (:export #:test-roan #:rerun-roan-failures #:test-quickly #:test-one))

(in-package :roan/test)

(defparameter *test-quietly* t)

(defparameter *sink-stream* (make-broadcast-stream))

(defmacro with-sink-stream (&body body)
  `(%with-sink-stream #'(lambda () ,@body)))

(defun %with-sink-stream (thunk)
  (if *test-quietly*
      (let ((*standard-output* *sink-stream*))
        (funcall thunk))
      (funcall thunk)))

(defparameter *test-results* nil)

(defun test-roan ()
  (setf *test-results* (with-summary () (run-tests :package :roan/test))))

(defvar *skip-network* nil
  "If true the tests that go out over the network to cccbr.github.io to update the method
library will not be run. This allows running the tests without a network connection, and
also speeds things up, particularly in non-SBCL implementations which are considerably
slower at updating the library.")

(defun test-quickly ()
  (let ((*skip-network* t))
    (test-system :roan)))

(defun test-one (name &optional (*skip-network* *skip-network*))
  (load-system "roan/test")
  (with-summary ()
    (run-tests :tests (list (intern (string name) :roan/test)))))

(defun rerun-roan-failures ()
  (with-failure-debugging () (rerun-failures *test-results*)))

;; (defun data-file (name &optional type)
;;   (merge-pathnames (make-pathname :name name :type type
;;                                   :directory '(:relative "test-data"))
;;                    (system-source-directory :roan)))
