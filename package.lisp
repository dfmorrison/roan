;;; Copyright (c) 1975-2017 Donald F Morrison
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

(defpackage :roan
  (:shadow #:method #:method-name)    ; Use method to refer to ringing concept instead.
  (:use :common-lisp :alexandria :iterate :trivial-garbage)
  (:export #:*cross-character*
           #:*default-stage*
           #:*print-bells-upper-case*
           #:+maximum-stage+
           #:+minimum-stage+
           #:add-pattern
           #:add-patterns
           #:alter-stage
           #:bell
           #:bell-at-position
           #:bell-from-name
           #:bell-name
           #:bells-list
           #:bells-vector
           #:call
           #:call-application-error
           #:call-apply
           #:canonicalize-method-place-notation
           #:canonicalize-place-notation
           #:cccbr-name
           #:changep
           #:copy-row
           #:cycles
           #:do-hash-set
           #:fch-group
           #:fch-group-name
           #:fch-group-parity
           #:fch-group-elements
           #:fch-groups-string
           #:format-pattern
           #:generate-rows
           #:hash-set
           #:hash-set-adjoin
           #:hash-set-clear
           #:hash-set-copy
           #:hash-set-count
           #:hash-set-delete
           #:hash-set-deletef
           #:hash-set-difference
           #:hash-set-elements
           #:hash-set-empty-p
           #:hash-set-intersection
           #:hash-set-member
           #:hash-set-nadjoin
           #:hash-set-nadjoinf
           #:hash-set-ndifference
           #:hash-set-nintersection
           #:hash-set-nunion
           #:hash-set-p
           #:hash-set-proper-subset-p
           #:hash-set-remove
           #:hash-set-subset-p
           #:hash-set-union
           #:in-course-p
           #:inappropriate-method-error
           #:inappropriate-method-error-details
           #:inappropriate-method-error-method
           #:inverse
           #:involutionp
           #:lookup-method
           #:lookup-methods-by-name
           #:lookup-methods-by-notation
           #:lookup-methods-from-changes
           #:make-hash-set
           #:make-match-counter
           #:map-hash-set
           #:match-counter
           #:match-counter-counts
           #:match-counter-handstroke-p
           #:match-counter-labels
           #:match-counter-pattern
           #:match-counter-stage
           #:method
           #:method-canonical-rotation-key
           #:method-changes
           #:method-classification
           #:method-contains-jump-changes
           #:method-course-length
           #:method-default-calls
           #:method-falseness
           #:method-hunt-bells
           #:method-lead-count
           #:method-lead-head
           #:method-lead-head-code
           #:method-lead-length
           #:method-name
           #:method-place-notation
           #:method-plain-course
           #:method-plain-lead
           #:method-principal-hunt-bells
           #:method-properties
           #:method-property
           #:method-rotations-p
           #:method-secondary-hunt-bells
           #:method-stage
           #:method-title
           #:method-true-plain-course-p
           #:method-working-bells
           #:methods-database-attributes
           #:mixed-stage-fch-groups-error
           #:named-row-pattern
           #:ngenerate-rows
           #:no-place-notation-error
           #:no-such-method-error
           #:npermute-by-collection
           #:npermute-collection
           #:order
           #:parse-method-title
           #:parse-pattern
           #:parse-place-notation
           #:parse-row
           #:pattern-parse-error
           #:permute
           #:permute-by-collection
           #:permute-by-inverse
           #:permute-collection
           #:permutef
           #:place-notation-error
           #:place-notation-string
           #:placesp
           #:plain-bob-lead-end-p
           #:position-of-bell
           #:read-place-notation
           #:read-row
           #:record-matches
           #:remove-all-patterns
           #:remove-pattern
           #:reset-match-counter
           #:rounds
           #:roundsp
           #:row
           #:row-match-error
           #:row-match-p
           #:rowp
           #:row-string
           #:set-method-classified-name
           #:stage
           #:stage-from-name
           #:stage-name
           #:tenors-fixed-p
           #:too-many-methods-error
           #:too-many-methods-error-count
           #:update-methods-database
           #:use-roan-package
           #:which-grandsire-lead-head
           #:which-plain-bob-lead-head
           #:with-methods-database
           #:write-place-notation
           #:write-row)
  (:documentation "===summary===
@cindex @code{roan} package
@cindex packages
All the symbols used by Roan to name functions, variables and so on are in the @code{roan}
package. When using them from another package, such as @code{cl-user}, they should be
prefixed with an explicit @code{roan:}.
@example
@group
 CL-USER> *package*
 #<Package \"COMMON-LISP-USER\">
 CL-USER> roan:+maximum-stage+
 24
@end group
@end example

Alternatively all the external symbols of the @code{roan} package can be imported into a
package with @code{use-package}, or the @code{:use} option to @code{defpackage}. There is
the slight complication, however, that the @code{roan} package shadows the symbols
@code{method} and @code{method-name} from the @code{common-lisp} package. This is done
because methods are an important concept in change ringing, albeit one unrelated to CLOS
methods. Typically @code{method} and @code{method-name} should be shadowed in other packages
that use the @code{roan} package. This can be done with @code{shadowing-import}, or the
@code{:shadowing-import} option to @code{defpackage}.
@example
@group
 MY-PACKAGE> *package*
 #<Package \"MY-PACKAGE\">
 MY-PACKAGE> (package-use-list *)
 (#<Package \"COMMON-LISP\">)
 MY-PACKAGE> (shadowing-import '(roan:method roan:method-name))
 T
 MY-PACKAGE> (use-package :roan)
 T
 MY-PACKAGE> +maximum-stage+
 24
@end group
@end example
===endsummary===
Contains the symbols used by Roan. The @code{roan} package shadows two symbols from the
@code{common-lisp} package: @code{method} and @code{method-name}. The functions and so on
attached to these symbols in the @code{common-lisp} package are usually only needed when
doing introspection, and the shadowing should rarely cause difficulties."))
