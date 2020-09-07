;;;; Copyright (c) 1975-2020 Donald F Morrison
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

(in-package :roan)

(in-readtable :roan+interpol)


;;; Bells

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +bell-names+ "1234567890ETABCDFGHJKLMN" :test #'string=)

  (defconstant +minimum-stage+ 2 "The smallest number of bells supported, 2.")
  (defconstant +maximum-stage+ (length +bell-names+)
    "The largest number of bells supported, 24."))

(deftype bell ()
  "===summary===
Roan supports ringing on as few as 2, or as many as 24, bells. Bells are represented as
small, non-negative integers less than this maximum stage. However, bells as the integers
used in Roan are zero-based: the treble is zero, the tenor on eight is 7, and so on. The
@code{bell} type corresponds to integers in this range. There are functions for mapping
bells to and from the characters corresponding to their usual textual representation in
change ringing.
===endsummary===
A representation of a bell. These are zero-based, small integers, so the treble is
@code{0}, the second is @code{1}, up to the tenor is one less than the stage."

  `(integer 0 (,+maximum-stage+)))

;;; We're assuming ASCII/Unicode here. It will have to be changed if support for a bizarre
;;; character set is required.
(defconstant +char-code-array-length+ 256)

(define-constant +named-bells+
    (iter (with result := (make-array +char-code-array-length+ :initial-element nil))
          (for c :in-string +bell-names+)
          (for i :from 0)
          (setf (svref result (char-code (char-downcase c))) i)
          (setf (svref result (char-code (char-upcase c))) i)
          (finally (setf (svref result (char-code #\o))
                         (setf (svref result (char-code #\O))
                               (svref result (char-code #\0))))
                   (return result)))
  :test #'equalp)

(defconstant +initial-print-bells-upper-case+ t)

(defparameter *print-bells-upper-case* +initial-print-bells-upper-case+
  "When printing bell names that are letters, whether or not to use upper case letters by
default. It is a generalized boolean, with an initial default value of @code{t}.")

(defun bell-name (bell &optional (upper-case *print-bells-upper-case*))
  "Returns a character denoting this @var{bell}, or @code{nil} if @var{bell} is not a
 @code{bell}. If the character is alphabetic, an upper case letter is return if the
 generalized boolean @var{upper-case} is true, and otherwise a lower case letter. If
 @var{upper-case} is not supplied it defaults to the current value of
 @code{*print-bells-upper-case*}.
@example
@group
 (bell-name 0) @result{} #\\1
 (map 'string #'bell-name
      (loop for i from 0 below +maximum-stage+
            collect i))
     @result{} \"1234567890ETABCDFGHJKLMN\"
 (bell-name -1) @result{} nil
 (bell-name +maximum-stage+) @result{} nil
@end group
@end example"
  (and (typep bell 'bell)
       (if upper-case
           (schar +bell-names+ bell)
           (char-downcase (schar +bell-names+ bell)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bell-from-name (char)
    "Returns the @code{bell} denoted by the character designator @var{char}, or @code{nil}
if it is not a character designator denoting a bell. The determination is case-insensitive.
@example
@group
 (bell-from-name \"8\") @result{} 7
 (bell-from-name \"E\") @result{} 10
 (map 'list #'bell-from-name \"135246\") @result{} (0 2 4 1 3 5)
 (bell-from-name \"%\") @result{} nil
@end group
@end example"
    (typecase char
      (character (let ((code (char-code char)))
                   (and (< code (length +named-bells+)) (svref +named-bells+ code))))
      (string (and (eql (length char) 1) (bell-from-name (char char 0))))
      (symbol (bell-from-name (symbol-name char)))
      (t nil))))


;;; Stages

(deftype stage ()
  "===summary===
 The @code{stage} type represents the subset of small, positive integers corresponding to
the numbers of bells Roan supports. While Roan represents stages as small, positive
integers, it is conventional in ringing to refer to them by names, such as ``Minor'' or
``Caters''. There are functions for mapping stages, the integers used by Roan, to and from
their conventional names as strings.
===endsummary===
A supported number of bells, an integer between @code{+minimum-stage+} and
@code{+maximum-stage+}, inclusive."
  `(integer ,+minimum-stage+ ,+maximum-stage+))

(define-constant +stage-names+
    (iter (with result := (make-array (+ +maximum-stage+ 1) :initial-element nil))
          (for stage :from +minimum-stage+ :to +maximum-stage+)
          (for name :in '(nil "Singles" "Minimus" "Doubles" "Minor" "Triples" "Major"
                        "Caters" "Royal" "Cinques" "Maximus" "Sextuples" nil "Septuples"
                        nil "Octuples" . #1=(nil . #1#)))
          (setf (aref result stage) (or name (format nil "~:(~R~)" stage)))
          (finally (return result)))
  :test #'equalp)

(define-constant +named-stages+
    (iter (with result :=
                (make-hash-table :test #'equalp
                                 :size (- (length +stage-names+) +minimum-stage+)))
          (for stage :from 0)
          (for name :in-vector +stage-names+)
          (when name
            (setf (gethash name result) stage))
          (finally (return result)))
  :test #'equalp)

(defun stage-name (stage)
  "Returns a string, the conventional name for this @var{stage}, capitalized, or
@code{nil} if @var{stage} is not an integer corresponding to a supported stage.
@example
@group
 (stage-name 8) @result{} \"Major\"
 (stage-name 22) @result{} \"Twenty-two\"
 (stage-name (1+ +maximum-stage+)) @result{} nil
@end group
@end example"
  ;; Always return a freshly consed copy so the original can't get smashed.
  (and (typep stage 'stage) (copy-seq (aref +stage-names+ stage))))

(defun stage-from-name (name)
  "Returns a stage, a small, positive integer, with its name the same as the string
designator @var{name}, or, if there is no stage with such a name, @code{nil}. The
determination is made case-insensitively.
@example
@group
 (stage-from-name \"cinques\") @result{} 11
 (stage-from-name \"no-such-stage\") @result{} nil
@end group
@end example"
  (typecase name
    (string (values (gethash name +named-stages+)))
    (symbol (stage-from-name (symbol-name name)))
    (t nil)))

(defvar *default-stage* 8
  "An integer, the default value for optional or keyword arguments to many functions that
must have a stage specified. @xref{write-row,,@code{write-row}},
@ref{row-string,,@code{row-string}},
@ref{write-place-notation,,@code{write-place-notation}} and
@ref{place-notation-string,,@code{place-notation-string}}.")


;;; Rows

(deftype %bells-vector () `(simple-array bell 1))

;; Make SBCL shut up about the forward reference.
(declaim (ftype function prin1-row))

;; TODO revamp this to use named-readtables

;; Rows are structures rather than instances of a subclass of standard-class because
;; equalp descends structures, allowing us to use rows as keys in CL hash-tables.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; %make-row needs to be available at compile time for some constants, below
  (defstruct (row (:constructor %make-row (bells))
                  (:predicate rowp)
                  (:print-object (lambda (row stream) (write-row row :stream stream))))
    "===summary===
@cindex immutable
The fundamental units of change ringing are rows and changes, permutations of a fixed set
of bells. A distinction between them is often made, where a row is a permutation of bells
and a change is a permutation taking one row to the next. In Roan they are both
represented by the same data type, @code{row}; @code{row}s should be treated as immutable.

@cindex Lisp reader
@cindex reader macro
@cindex bang
@cindex @code{!} reader macro
@cindex printing @code{row}s
@cindex writing @code{row}s
@cindex quote
The @url{http://www.lispworks.com/documentation/HyperSpec/Body/23_.htm,,Lisp reader} can be
augmented by Roan to read @code{row}s printed in the notation usually used by change
ringers by using the @samp{!} reader macro. For example, queens on twelve can be entered
in Lisp as @code{!13579E24680T}. When read with the @samp{!} reader macro bells
represented by alphabetic characters can be either upper or lower case; so queens on
twelve can also be entered as @code{!13579e24680t} or @code{!13579e24680T}.

To support the common case of writing lead heads of treble dominated methods if the treble
is leading it can be omitted. Thus, queens on twelve can also be entered as
@code{!3579E24680T}. Apart from a leading treble, however, if any bell is omitted from a
row written with a leading @samp{!} character an error will be signaled.

Note that @code{row}s are Lisp atoms, and thus literal values can be written using
@samp{!} notation without quoting, though quoting @code{row}s read that way will do no
harm when they are evaluated.

This @samp{!} syntax can be turned on and off by using @ref{roan-syntax}. By default it is
off when Roan is loaded. It is also possible to control this syntax by using
@url{https://github.com/melisgl/named-readtables/, Named Readtables}; see
@ref{roan-syntax} for further details.

Similarly, @code{row}s are printed using this same notation,
@url{http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_esc.htm#STprint-escapeST,,@code{*print-escape*}}
controlling whether or not they are preceded by @samp{!} characters. Note that the
characters used to represent bells in this printed representation differ from the small
integer @code{bell}s used to represent them internally, since the latter are zero based.
For example, the treble is represented internally by the integer 0, but in this printed
representation by the digit character @samp{1}. When printing rows in this way a leading
treble is not elided. And @code{*print-bells-upper-case*} can be used to control the case
of bells in the printed representation of @code{row}s that are representated by letters,
in cinques and above.
@example
@group
CL-USER> !12753468
!12753468
CL-USER> '!2753468
!12753468
CL-USER> (format t \"with:    ~S~%without:  ~:*~A~%\" !TE0987654123)
with:    !TE0987654123
without:  TE0987654123
NIL
CL-USER> (let ((roan:*print-bells-upper-case* nil))
           (format nil \"~A\" !TE0987654123))
\"te0987654123\"
CL-USER>
@end group
@end example

@cindex equality
@cindex comparing @code{row}s
@cindex @code{equalp}
@cindex @code{equal}
Rows can be compared for equality using @code{equalp} (but not @code{equal}). That is,
two different @code{row} objects that correspond to the same ordering of the same number of
bells will be @code{equalp}. Hash tables with a @code{:test} of @code{equalp} are often
useful with @code{row}s. @xref{hash-set}.
@example
@group
 (equalp !13572468 !13572468) @result{} t
 (equalp !13572468 !12753468) @result{} nil
 (equalp !13572468 !1357246) @result{} nil
 (equalp !13572468 !3572468) @result{} t
@end group
@end example
===endsummary===
A permutation of bells at a particular stage. The type @code{row} is used to represent
both change ringing rows and changes; that is, rows may be permuted by other rows.
Instances of @code{row} should normally be treated as immutable."
    (bells nil :type %bells-vector))

  (defmethod make-load-form ((r row) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots r)))

(defun write-row (row &key
                        (stream *standard-output*)
                        (escape *print-escape*)
                        (upper-case *print-bells-upper-case*))
  "Writes @var{row}, which should be a @code{row}, to the indicated @var{stream}.
The case of any bells represented by letters is controlled by @var{upper-case}, a
generalized boolean defaulting to the current value of @code{*print-bells-upper-case*}.
@var{escape}, a generalized Boolean defaulting to the current value of
@code{*print-escape*}, determines whether or not to write it in a form that read can
understand. Signals a @code{type-error} if @var{row} is not a @code{row}, and the usual
errors if @var{stream} is not open for writing, etc."
  (with-standard-io-syntax
    (when escape
      (princ #\! stream))
    (iter (for b :in-vector (row-bells row))
          (princ (bell-name b upper-case) stream))
    row))

(defun row-string (row &optional (upper-case *print-bells-upper-case*))
  "Returns a string representing the @code{row} @var{row}. The case of any bells
represented by letters is controlled by @var{upper-case}, a generalized
boolean defaulting to the current value of @code{*print-bells-upper-case*}. Signals a
@code{type-error} if @var{row} is not a @code{row}."
  (with-output-to-string (s)
    (write-row row :stream s :escape nil :upper-case upper-case)))

(defun stage (row)
  "The number of @code{bell}s of which the @code{row} @var{row} is a permutation."
  (length (row-bells row)))

(defun %assemble-row (bells)
  (let ((stage (1+ (length bells))) (bells-present 0))
    (dolist (b bells)
      (when (or (>= b stage) (logbitp b bells-present))
        (return-from %assemble-row nil))
      (setf bells-present (logior (ash 1 b) bells-present)))
    (if (logbitp 0 bells-present)
        (when (logbitp (decf stage) bells-present)
          (return-from %assemble-row nil))
        (push 0 bells))
    (and (>= stage +minimum-stage+)
         (%make-row (make-array stage :element-type 'bell :initial-contents bells)))))

(define-condition row-creation-error (simple-error) ())

(define-condition row-creation-parse-error (row-creation-error parse-error) ())

(defun row-creation-error (data parse)
  (error (if parse 'row-creation-parse-error 'row-creation-error)
         :format-control "Can't create a row from ~S."
         :format-arguments (list data)))

(defun read-row (&optional (stream *standard-input*) (eof-error-p t) (eof-value nil)
                   (recursive-p nil))
  "Constructs and returns a @code{row} from the conventional symbols for bells read from
the @var{stream}. The stage of the row read is determined by the bells present, that is by
the largest bell for which a symbol is read. The treble can be elided, in which case it is
assumed to be leading; a @code{parse-error} is signaled if any other bell is omitted.
Bells represented by letters can be either upper or lower case."
  (when eof-error-p
    (peek-char nil stream eof-error-p eof-value recursive-p))
  (iter (for char := (read-char stream nil nil recursive-p))
        (for bell := (and char (bell-from-name char)))
        (while bell)
        (collect bell :into bells)
        (collect char :into chars)
        (finally (when char
                   (unread-char char stream))
                 (return (or (%assemble-row bells)
                             (row-creation-error (coerce chars 'string) t))))))

;; TODO can I delete this in favor of the defreadtable?
;; ;; Make the Lisp reader recognize things beginning with '!' as rows.
;; (set-macro-character #\! #'read-row t)

(defun parse-row (string &key (start 0) (end nil) (junk-allowed nil))
  "Contructs a @code{row} from the conventional symbols for bells in the section of string
@var{string} delimited by @var{start} and @var{end}, possibly preceded or followed by
whitespace. The treble can be elided, in which case it is assumed to be leading; a
@code{parse-error} is signaled if any other bell is omitted. Bells represented by letters
can be either upper or lower case. If @var{string} is not a string a @code{type-error} is
signaled. If the generalized boolean @var{junk-allowed} is false, the default, an error
will be signaled if additional non-whitespace characters follow the representation of a
row. Returns two values: the @code{row} read and a non-negative integer, the index into
the string of the next character following all those that were parsed, including any
trailing whitespace; if parsing consumed the whole of @var{string}, the second value will
be length of @var{string}."
  (let ((result nil) final-index)
    (with-input-from-string (s string :start start :end end :index final-index)
      (when (bell-from-name (peek-char t s nil))
        (setf result (read-row s))
        (when (and (peek-char t s nil) (not junk-allowed))
          (setf result nil))))
    (if (or result junk-allowed)
        (values result final-index)
        (row-creation-error (subseq string start end) t))))

(defun row (&rest bells)
  "Constructs and returns a @code{row} containing the @var{bells}, in the order they
appear in the argument list. If the treble is not present, it defaults to being the first
bell in the row. Duplicate bells or bells other than the treble missing result in an error
being signaled.
@example
 (row 2 1 3 4 7 6 5) @result{} !13245876
@end example"
  (dolist (bell bells)
    (check-type* bell bell))
  (or (%assemble-row bells) (row-creation-error bells nil)))

(define-constant +rounds-bell-array+
    (iter (with result := (make-array +maximum-stage+ :element-type 'bell))
          (for i :from 0 :below +maximum-stage+)
          (setf (aref result i) i)
          (finally (return result)))
  :test #'equalp)

(defun rounds (&optional (stage *default-stage*))
  "Returns a @code{row} representing rounds at the given @var{stage}, which defaults to
@code{*default-stage*} Signals a @code{type-error} if @var{stage} is not a @code{stage},
that is an integer between @code{+minimum-stage+} and @code{+maximum-stage+}, inclusive."
  (check-type* stage stage)
  (%make-row (subseq +rounds-bell-array+ 0 stage)))

(defun bell-at-position (row position)
  "The @code{bell-at-position} function returns the @code{bell} (that is, a small integer)
at the given @var{position} in the @var{row}. The @code{position-of-bell} function returns
position of @var{bell} in @var{row}, or @code{nil} if @var{bell} does not appear in
@var{row}. The indexing into @var{row} is zero-based; so, for example, the leading bell is
at position 0, not 1. Signals an error if @var{row} is not a @code{row}, or if
@var{position} is not a non-negative integer or is too large for the stage of @var{row}
@example
@group
 (bell-at-position !13572468 3) @result{} 6
 (bell-name (bell-at-position !13572468 3)
     @result{} #\\7
 (position-of-bell 6 !13572468) @result{} 3
 (position-of-bell (bell-from-name #\7) !13572468)
     @result{} 3
@end group
@end example"
  (aref (row-bells row) position))

(defun position-of-bell (bell row)
  "===merge: bell-at-position"
  (position bell (row-bells row)))

(defun bells-list (row)
  "The @code{bells-list} function returns a fresh list of @code{bell}s (small, non-negative
integers, zero-based), the bells of @var{row}, in the same order that they appear in
@var{row}. The @code{bells-vector} function returns a vector of @code{bell}s (small,
non-negative integers, zero-based), the bells of @var{row}, in the same order that they
appear in @var{row}. If @var{vector} is not supplied or is @code{nil} a freshly created,
simple general vector is returned.
@example
@group
 (bells-list !13572468) @result{} (0 2 4 6 1 3 5 7)
 (bells-vector !142536) @result{} #(0 3 1 4 2 5)
@end group
@end example

If a non-nil @var{vector} is supplied the @code{bell}s are copied into it and it is
returned. If @var{vector} is longer than the stage of @var{row} only the first elements of
@var{vector}, as many as the stage of @var{row}, are over-written; the rest are unchanged.
If @var{vector} is shorter than the stage of @var{row}, then, if it is adjustable, it is
adjusted to be exactly as long as the stage of @var{row}, and otherwise an error is
signaled without any modifications made to the contents of @var{vector} or its
fill-pointer, if any. If @var{vector} has a fill-pointer and is long enough to hold all
the bells of @var{row}, possibly after adjustment, its fill-pointer is set to the stage of
@var{row}.

A @code{type-error} is signaled if @var{row} is not a @code{row}. An error is signled if
@var{vector} is neither @code{nil} nor a @code{vector} with an element type that is a
supertype of @code{bell}, and of sufficient length or adjustable."
  (coerce (row-bells row) 'list))

(defun bells-vector (row &optional vector)
  "===merge: bells-list"
  (let* ((bells (row-bells row)) (stage (length bells)))
    (unless vector
      (return-from bells-vector (make-array stage :initial-contents bells)))
    (check-type* vector vector)
    (unless (subtypep 'bell (array-element-type vector))
      (error 'row-creation-error
             :format-control "The vector, ~S, is not of a type that can hold bells."
             :format-arguments (list vector)))
    (when (> stage (array-dimension vector 0))
      (if (adjustable-array-p vector)
          (adjust-array vector stage)
          (error 'row-creation-error
                 :format-control "The vector, ~S, is too short to hold the bells of ~S."
                 :format-arguments (list vector row))))
    (when (array-has-fill-pointer-p vector)
      (setf (fill-pointer vector) stage))
    (iter (for b :in-vector bells)
          (for i :from 0)
          (setf (aref vector i) b)))
  vector)

(defun reversed-row (row)
  "Returns a @code{row} of the same stage as @var{row} with its bells in the reverse
order. A @code{type-error} is signaled if @var{row} is not a @code{row}.
@example
 (reversed-row !32148765) @result{} !56784123
@end example"
  (%make-row (reverse (row-bells row))))


;;; Permuting rows

;; Because permute with exactly two arguments of the same stage is often used in inner
;; loops, it's worth going to some trouble to tune it, and thus the definition of it is
;; somewhat convoluted. Most of the measurements and adjustments have been done in SBCL,
;; as that seems the free implementation most like to be used by folks who care about
;; performance. On the other hand, many of the type and inline declarations make no
;; difference to SBCL since it does a pretty good job of type inference, or at least
;; appears to in this case. However, they do still seem worth adding in case they help
;; some other implementations. Note also the weird lambda list: this is done to reduce
;; both consing and time in the common case of exactly two arguments--this also requires
;; some handsprings to get the documentation right, which displays a lambda list of just
;; (row &rest changes). And yes, I know, if we really wanted to sqeeze maximum performance
;; out of it we could change the row abstraction to no longer be immutable, but it doesn't
;; seem worth the extra complexity--if cases turn up where that seems necessary, it's
;; probably better to deal with them one by one under the hood, and leave the public
;; abstraction immutable. Less effort has gone into tuning the more than two argument or
;; unequal stage cases, other than trying to reduce the consing from the obvious, brute
;; force implementation of the former. If it were needed more could be done, but it
;; doesn't seem worth the trouble.

(declaim (inline %permute %fill-bells-vector))

(defun %permute (from-bells by-bells to-bells)
  ;; to-bells must be at least as long as from-bells and by-bells
  (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0) (debug 0)))
  (declare (type %bells-vector from-bells by-bells to-bells))
  (declare (inline min))
  (iter (declare (declare-variables))
        (for b :in-vector by-bells)
        (declare (type bell b))
        (for i :from 0)
        (declare (type fixnum i))
        (setf (aref to-bells i) (if (< b (length from-bells)) (aref from-bells b) b)))
  (when (> (length from-bells) (length by-bells))
    (iter (declare (declare-variables))
          (for i :from (length by-bells) :below (length from-bells))
          (setf (aref to-bells i) (aref from-bells i)))))

(defun %fill-bells-vector (from-bells to-bells)
  ;; to-bells must be at least as long as from-bells
  (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0) (debug 0)))
  (declare (type %bells-vector from-bells to-bells))
  (dotimes (i (length from-bells))
    (declare (type fixnum i))
    (setf (aref to-bells i) (aref from-bells i)))
  (iter (declare (declare-variables))
        (for i :from (length from-bells) :below (length to-bells))
        (declare (type bell i))
        (setf (aref to-bells i) i)))

(defun permute (row &optional (change nil change-supplied) &rest changes)
 "===lambda: (row &rest changes)
Permutes @var{row} by the @var{changes} in turn. That is, @var{row} is first permuted by
the first of the @var{changes}, then the resuling row is permuted by second of the
@var{changes}, and so on. Returns the row resulting from applying all the changes. So long
as one or more @var{changes} are supplied the returned @code{row} is always a freshly
created one: @var{row} and none of the @var{changes} are modified (as you'd expect, since
they are intended to be viewed as immutable). The @var{row} and all the @var{changes}
should be @code{row}s.

At each step of permuting a row by a change, if the row is of higher stage than the
change, only the first @var{stage} bells of the row are permuted, where @var{stage} is the
stage of the change, all the remaining bells of the row being unmoved. If the row is of
lower stage than the change, it is as if the row were extended with bells in their rounds'
positions for all the bells @var{stage} and above. Thus the result of each permuation step
is a @code{row} whose stage is the larger of those of the row and the change.

If no @var{changes} are supplied @code{row} is returned. Signals a @code{type-error} if
@var{row} or any of the @var{changes} are not @code{row}s.
@example
@group
 (permute !34256 !35264) @result{} !145362
 (permute !34125 !4321 !1342) @result{} !24315
 (permute !4321 !654321) @result{} !651234
 (let ((r !13572468))
   (list (eq (permute r) r)
         (equalp (permute r (rounds 8)) r)
         (eq (permute r (rounds 8)) r)))
     @result{} (t t nil)
@end group
@end example"
  (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0) (debug 0)))
  (check-type* row row)
  (unless change-supplied
    (return-from permute row))
  (check-type* change row)
  (locally (declare (type row row change) (inline max make-array length))
    (let ((row-bells (row-bells row)) (change-bells (row-bells change)))
      (declare (type %bells-vector row-bells change-bells))
      (cond (changes
             (let ((result-length (max (length row-bells)
                                       (length change-bells)
                                       (reduce #'max changes
                                               :key #'(lambda (x)
                                                        (check-type* x row)
                                                        (length (row-bells x)))))))
               (declare (type stage result-length))
               (let ((result (make-array result-length :element-type 'bell)))
                 (declare (type %bells-vector result))
                 (%fill-bells-vector row-bells result)
                 (let ((scratch (copy-seq result)))
                   (declare (type %bells-vector result scratch))
                   (%permute row-bells change-bells result)
                   (dolist (c changes)
                     (locally (declare (type row c))
                       (let ((c-bells (row-bells c)))
                         (declare (type %bells-vector c-bells))
                         (rotatef result scratch)
                         (%permute scratch c-bells result)))))
                 (%make-row result))))
            ((eql (length row-bells) (length change-bells))
             (let ((result (make-array (length row-bells) :element-type 'bell)))
               (declare (type %bells-vector result))
               (dotimes (i (length change-bells))
                 (declare (type fixnum i))
                 (setf (aref result i) (aref row-bells (aref change-bells i))))
               (%make-row result)))
            (t (let ((result (make-array (max (length row-bells) (length change-bells))
                                         :element-type 'bell)))
                 (declare (type %bells-vector result))
                 (%fill-bells-vector row-bells result)
                 (%permute row-bells change-bells result)
                 (%make-row result)))))))

(define-modify-macro permutef (&rest changes) permute
                     "===lambda: (row &rest changes)
Permutes @var{row}, which should be a location suitable as the first argument to
@code{setf} containing a @code{row}, by @var{changes}, updating @var{row} to contain the
result, and returns it. Signals a @code{type-error} if the value @var{row} contains is not
a @code{row} or if any of the @var{changes} are not @code{row}s.")

(defgeneric permute-collection (collection change)
  (:documentation "Permutes each of the elements of a sequence or @code{hash-set} and an
individual @code{row}, collecting the results into a similar collection. The
@code{permute-collection} version permutes each the elements of @var{collection} by
@var{change}; @code{permute-by-collection} permutes @var{row} by each of the elements of
@var{collection} by @var{change}. The return value is a list, vector or @code{hash-set} if
@var{collection} is a list, vector or @code{hash-set}, respectively. The
@code{permute-collection} and @code{permute-by-collection} versions always return a fresh
collection; the @code{npermute-collection} and @code{npermute-by-collection} versions
modify @var{collection}, replacing its contents by the permuted rows. If @var{collection}
is a sequence the contents of the result are in the same order: that is, the Nth element
of the result is the Nth element supplied in @var{collection} permuted by or permuting
@var{change} or @var{row}. If @var{collection} is a vector, @code{permute-collection}
and @code{permute-by-collection} always return a simple, general vector.

If the result is a sequence, or if all the elements of @var{collection} were of the same
stage as one another, it is guaranteed that the result will be the same length or
cardinality as @var{collection}. However, if @var{collection} is a @code{hash-set}
containing rows of different stages the result may be of lower cardinality than then the
supplied @code{hash-set}, if @var{collection} contained two or more elements that were not
@code{equalp} because they were of different stages, but after being permuted by, or
permuting, a higher stage row the results are @code{equalp}.

Signals a @code{type-error} if @var{change}, @var{row} or any of the elements of
@var{collection} are not @code{rows}s, or if @var{collection} is not a sequence or
@code{hash-set}."))

(defgeneric permute-by-collection (row collection)
  (:documentation "===merge: permute-collection 1"))

(defgeneric npermute-collection (collection change)
  (:documentation "===merge: permute-collection 2"))

(defgeneric npermute-by-collection (row collection)
  (:documentation "===merge: permute-collection 3"))

(defmethod permute-collection ((collection list) change)
  (check-type* change row)              ; need to check it here in case colleciton is empty
  (mapcar #'(lambda (row) (permute row change)) collection))

(defmethod permute-by-collection (row (collection list))
  (check-type* row row)                 ; need to check it here in case colleciton is empty
  (mapcar #'(lambda (change) (permute row change)) collection))

(defmethod npermute-collection ((collection list) change)
  (check-type* change row)              ; need to check it here in case colleciton is empty
  (mapl #'(lambda (sublist) (permutef (first sublist) change)) collection))

(defmethod npermute-by-collection (row (collection list))
  (check-type* row row)                 ; need to check it here in case colleciton is empty
  (mapl #'(lambda (sublist) (setf (first sublist) (permute row (first sublist))))
        collection))

(defmethod permute-collection ((collection vector) change)
  (check-type* change row)              ; need to check it here in case colleciton is empty
  (map 'vector #'(lambda (row) (permute row change)) collection))

(defmethod npermute-collection ((collection vector) change)
  (check-type* change row)              ; need to check it here in case colleciton is empty
  (dotimes (i (length collection) collection)
    (declare (type fixnum i))
    (permutef (aref collection i) change)))

(defmethod permute-by-collection (row (collection vector))
  (check-type* row row)                 ; need to check it here in case colleciton is empty
  (map 'vector #'(lambda (change) (permute row change)) collection))

(defmethod npermute-by-collection (row (collection vector))
  (check-type* row row)                 ; need to check it here in case colleciton is empty
  (dotimes (i (length collection) collection)
    (declare (type fixnum i))
    (setf (aref collection i) (permute row (aref collection i)))))

(defmethod permute-collection ((collection hash-set) change)
  (check-type* change row)              ; need to check it here in case colleciton is empty
  (let ((result (make-hash-set :size (hash-set-count collection))))
    (do-hash-set (row collection result)
      (hash-set-nadjoinf result (permute row change)))))

(defmethod permute-by-collection (row (collection hash-set))
  (check-type* row row)                 ; need to check it here in case colleciton is empty
  (let ((result (make-hash-set :size (hash-set-count collection))))
    (do-hash-set (change collection result)
      (hash-set-nadjoinf result (permute row change)))))

(defun %npermute-hash-set (set function)
  (assert (<= (hash-set-count set) most-positive-fixnum))
  (let ((elements (make-array (hash-set-count set))))
    (declare (type simple-vector elements))
    (iter (declare (declare-variables))
          (for i from 0)
          (for row :in-hash-set set)
          (declare (type fixnum i))
          (setf (svref elements i) row))
    (hash-set-clear set)
    (map nil #'(lambda (element)
                 (hash-set-nadjoinf set (funcall function element)))
         elements))
  set)

(defmethod npermute-collection ((collection hash-set) change)
  (check-type* change row)              ; need to check it here in case colleciton is empty
  (%npermute-hash-set collection #'(lambda (row) (permute row change))))

(defmethod npermute-by-collection (row (collection hash-set))
  (check-type* row row)                 ; need to check it here in case colleciton is empty
  (%npermute-hash-set collection #'(lambda (change) (permute row change))))

(defun permute-collection-type-error (collection)
  (error 'type-error :expected-type '(or sequence hash-set) :datum collection))

(defmethod permute-collection ((collection t) change)
  (declare (ignore change))
  (permute-collection-type-error collection))

(defmethod permute-by-collection (row (collection t))
  (declare (ignore row))
  (permute-collection-type-error collection))

(defmethod npermute-collection ((collection t) change)
  (declare (ignore change))
  (permute-collection-type-error collection))

(defmethod npermute-by-collection (row (collection t))
  (declare (ignore row))
  (permute-collection-type-error collection))

(defgeneric generate-rows (changes &optional initial-row)
  (:documentation "Generates a sequence of @code{row}s by permuting a starting @code{row}
successively by each element of the sequence @var{changes}. The elements of @var{changes}
should be @code{row}s. If @var{initial-row} is supplied it should be a @code{row}. If it
is not supplied, rounds at the same stage as the first element of @var{changes} is used;
if @var{changes} is empty, rounds at @code{*default-stage*} is used. Two values are
returned. The first is a sequence of the same length as @var{changes}, and the second is a
@code{row}. So long as @var{changes} is not empty, the first element of the first return
value is @var{initial-row}, or the default rounds. The next value is that @code{row}
permuted by the first element of @var{changes}; then that @code{row} permuted by the next
element of @var{changes}, and so on, until all but the last element of @var{changes} has
been used. The second return value is the last element of the first return value permuted
by the last element of @var{changes}. If @var{changes} is empty, then the first return
value is also empty, and @var{initial-row}, or the default rounds, is the second return
value. Thus, for most methods, if @var{changes} are the changes of a lead, the first
return value will be the rows of a lead starting with @var{initial-row}, and the second
return value the lead head of the following lead.

If @var{changes} is a list, the first return value is a list; if @var{changes} is a
vector, the first return value is a vector. The @code{generate-rows} function always
returns a fresh sequence as its first return value, while @code{ngenerate-rows} resuses
@var{changes}, replacing its elements by the permuted rows and returning it. The fresh
vector created and returned by @code{generate-rows} is always a simple, general vector.

Signals an error if @var{initial-row} is neither a @code{row} nor @code{nil}, if
@var{changes} isn't a sequence, or if any elements of @var{changes} are not
@code{row}s.
@example
@group
 (multiple-value-list
   (generate-rows '(!2143 !1324 !2143 !1324) !4321))
     @result{} ((!4321 !3412 !3142 !1324) !1234)
@end group
@end example"))

(defgeneric ngenerate-rows (changes &optional initial-row)
  (:documentation "===merge: generate-rows"))

(defun generate-rows-initial-row (initial-row change)
  (cond (initial-row (check-type* initial-row row)
                     initial-row)
        (t (rounds (or (and (typep change 'row) (stage change)) *default-stage*)))))

(defmethod generate-rows ((changes list) &optional initial-row)
  (iter (with r := (generate-rows-initial-row initial-row (first changes)))
        (for c :in changes)
        (check-type* c row)
        (collect r :into result)
        (permutef r c)
        (finally (return (values result r)))))

(defmethod ngenerate-rows ((changes list) &optional initial-row)
  (iter (with r := (generate-rows-initial-row initial-row (first changes)))
        (for sublist :on changes)
        (for c := (first sublist))
        (check-type* c row)
        (setf (first sublist) r)
        (permutef r c)
        (finally (return (values changes r)))))

(defmethod generate-rows ((changes vector) &optional initial-row)
  ;; I'm not aware of any Lisp implementations where there is a specialized vector type
  ;; that would be able to hold rows, but just in case there is, make sure we're returning
  ;; a simple vector.
  (let ((sv (coerce changes 'simple-vector)))
    (ngenerate-rows (if (eq sv changes) (copy-seq sv) sv) initial-row)))

(defmethod ngenerate-rows ((changes vector) &optional initial-row)
  (iter (with r := (generate-rows-initial-row initial-row
                                              (and (not (zerop (length changes)))
                                                   (aref changes 0))))
        (for i :from 0 :below (length changes))
        (for c := (aref changes i))
        (check-type* c row)
        (setf (aref changes i) r)
        (permutef r c)
        (finally (return (values changes r)))))

(defun generate-rows-error (changes)
  (error 'type-error :expected-type 'row :datum changes))

(defmethod generate-rows ((changes t) &optional initial-row)
  (declare (ignore initial-row))
  (generate-rows-error changes))

(defmethod ngenerate-rows ((changes t) &optional initial-row)
  (declare (ignore initial-row))
  (generate-rows-error changes))

(defun permute-by-inverse (row change)
  "Equivalent to @code{(permute @var{row} (inverse @var{change}))}. Signals a
@code{type-error} if either @var{row} or @var{change} is not a @code{row}.
@example
@group
 (permute-by-inverse !13456287 !45678123) @result{} !28713456
 (permute-by-inverse !54312 !2438756) @result{} !54137862
 (permute-by-inverse !762345 !4312) @result{} !6271345
@end group
@end example"
  (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0) (debug 0)))
  (check-type* row row)
  (check-type* change row)
  (locally (declare (type row row change) (inline max make-array length))
    (let* ((row-bells (row-bells row))
           (row-stage (length row-bells))
           (change-bells (row-bells change)))
      (declare (type %bells-vector row-bells change-bells) (type stage row-stage))
      (if (eql (length change-bells) row-stage)
          (let ((result (make-array row-stage :element-type 'bell)))
            (declare (type %bells-vector result))
            (dotimes (i row-stage)
              (declare (type fixnum i))
              (setf (aref result (aref change-bells i)) (aref row-bells i)))
            (%make-row result))
          (let ((result (make-array (max row-stage (length change-bells))
                                    :element-type 'bell)))
            (declare (type %bells-vector result))
            (%fill-bells-vector row-bells result)
            (dotimes (i (length change-bells))
              (declare (type fixnum i))
              (setf (aref result (aref change-bells i))
                    (if (< i row-stage) (aref row-bells i) i)))
            (%make-row result))))))

(defun permutation-closure (&rest rows)
  "Returns a list of distinct rows that can be generated by permuting, repeatedly if
necessary, any of the @var{rows} by themselves or any others of the @var{rows}. If the
@var{rows} are not all of the same stage, the lower stage ones are converted to the
highest stage present before the closure operation is performed. The order of the returned
rows is undefined. Signals a @code{type-error} if any of the @var{rows} is not a
@code{row}.
@example
@group
 (permutation-closure !13425 !1324 !123465)
   @result{} (!143265 !142365 !124365 !142356 !143256 !124356
       !134265 !132465 !123456 !123465 !132456 !134256)
@end group
@end example"
  (cond ((null rows)
         (return-from permutation-closure nil))
        ((null (rest rows))
         (check-type* (first rows) row)
         (return-from permutation-closure (cyclic-closure (first rows)))))
  (let* ((result (make-hash-set))
         (max-stage (iter (for r :in rows)
                          (check-type* r row)
                          (maximizing (stage r))))
         (gen (iter (for r :in rows)
                    (for cyc := (delete-if #'roundsp
                                           (cyclic-closure (alter-stage r max-stage))))
                    (apply #'hash-set-nadjoin result cyc)
                    (nconcing cyc))))
    (iter (for n := (hash-set-count result))
          (for prev :previous n)
          (until (eql n prev))
          (iter (with elements := (hash-set-elements result))
                (for e :in elements)
                (dolist (g gen)
                  (hash-set-nadjoin result (permute e g))))
          (finally (return (hash-set-elements result))))))

(defun cyclic-closure (row)
  (iter (for r :initially row :then (permute r row))
        (collect r)
        (until (roundsp r))))


;;; Properties of rows and operations on them

(declaim (inline %roundsp))

(defun %roundsp (bells)
  (iter (for b :in-vector bells)
        (for i :from 0)
        (always (eql b i))))

(defun roundsp (row)
  "True if and only if @var{row} is a @code{row} representing rounds at its stage.
@example
@group
 (roundsp !23456) @result{} t
 (roundsp !123546) @result{} nil
 (roundsp 123456) @result{} nil
@end group
@end example"
  (and (rowp row) (%roundsp (row-bells row))))

(defun changep (row)
  "True if and only if @var{row} is a @code{row} representing a permutation with no
bell moving more than one place.
@example
@group
 (changep !214365) @result{} t
 (changep !143265) @result{} nil
 (changep |214365|) @result{} nil
@end group
@end example"
  (and (rowp row)
       (iter (for i :from -1)
             (for k :from 1)
             (for b :in-vector (row-bells row))
             (always (<= i b k)))))

(defun %placesp (bells &rest places)
  ;; Assumes places are all integers, in strictly increasing order, and ignores places
  ;; greater than or equal to the stage. Don't use with jump changes.
  (iter (with stage-1 := (- (length bells) 1))
        (for i :from 0 :to stage-1)
        (cond ((eql i (first places))
               (unless (eql (aref bells i) i)
                 (return nil))
               (pop places))
              ((< i stage-1)
               (unless (and (eql (aref bells i) (+ i 1))
                            (eql (aref bells (+ i 1)) i))
                 (return nil))
               (incf i))
              (t (return nil)))
        (finally (return t))))

(defun placesp (row &rest places)
  "Returns true if and only if @var{row} is a (non-jump) change, with exactly the
specified @var{places} being made, and no others. To match a cross at even stages supply
no @var{places}.

Signals a @code{type-error} if @var{row} is not a @code{row} or any of @var{places} are
not @code{bell}s. Signals an @code{error} if any of @var{places} are not less than the
stage of @var{row}, or are duplicated.
@example
@group
 (placesp !21354768 2 7) @result{} t
 (placesp !21346587 2 7) @result{} nil
 (placesp !21354768 2) @result{} nil
 (placesp !2135476 2) @result{} t
 (placesp !21436587) @result{} t
@end group
@end example"
  (let ((bells (row-bells row)))
    (iter (with stage := (length bells))
          (with seen := 0)
          (with in-order := t)
          (for p :in places)
          (for prev :previous p :initially -1)
          (check-type* p bell)
          (unless (< p stage)
            (error "~D too large for ~S." p row))
          (when (logbitp p seen)
            (error "Duplicate place, ~D, in ~S." p row))
          (unless (< prev p)
            (setf in-order nil))
          (setf seen (logior (ash 1 p) seen))
          (finally (unless in-order
                     (setf places (sort places #'<)))))
    (apply #'%placesp bells places)))

(declaim (inline %in-course-p))

(defun %in-course-p (row-bells)
  (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0) (debug 0))
           (type %bells-vector row-bells))
  (iter (with bells := (copy-seq row-bells))
        (declare (type %bells-vector bells))
        (with end := (length bells))
        (declare (type stage end))
        ;; Note that we don't have to check the i = 0 case, which is guaranteed to be
        ;; OK by the time we get there.
        (with i := 1) (declare (fixnum i))
        (with result := t)
        (while (< i end))
        (cond ((eql (aref bells i) i)
               (incf i))
              (t (rotatef (aref bells i) (aref bells (aref bells i)))
                 (setf result (not result))))
        (finally (return result))))

(defun in-course-p (row)
  "True if and only if @var{row} is a @code{row} representing an even permutation.
@example
@group
 (in-course-p !132546) @result{} t
 (in-course-p !214365) @result{} nil
 (in-course-p \"132546\") @result{} nil
@end group
@end example"
  ;; (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0) (debug 0)))
  (and (rowp row) (%in-course-p (row-bells row))))

(defun inverse (row)
  "Returns the inverse of the @code{row} @var{row}. That is, the @code{row}, @var{r}, such
that when @var{row} is permuted by @var{r}, the result is rounds. A theorem of group
theory implies also that when @var{r} is permuted by @var{row} the result will also be
rounds. Signals a @code{type-error} if @var{row} is not a @code{row}.
@example
@group
 (inverse !13427586) @result{} !14236857
 (inverse !14236857) @result{} !13427586
 (inverse !12436587) @result{} !12436587
 (inverse !12345678) @result{} !12345678
@end group
@end example"
  (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0) (debug 0)))
  (check-type* row row)
  (locally (declare (type row row) (inline length make-array))
    (let* ((bells (row-bells row))
           (stage (length bells))
           (result (make-array stage :element-type 'bell)))
      (declare (type %bells-vector bells result) (type stage stage))
      (dotimes (i stage)
        (declare (type fixnum i))
        (setf (aref result (aref bells i)) i))
      (%make-row result))))

(defun involutionp (row)
  "True if and only if @var{row} is a @code{row} that is its own inverse.
@example
@group
 (involutionp !13248765) @result{} t
 (involutionp !13425678) @result{} nil
 (involutionp nil) @result{} nil
@end group
@end example"
  (and (rowp row)
       (iter (with bells := (row-bells row))
             ;; We skip i=0 because if all the rest are right, it must be, too.
             (for i :from 1 :below (stage row))
             (always (eql (aref bells (aref bells i)) i)))))

(defun order (row)
  "Returns a positive integer, the order of @var{row}: the minimum number of times it must
be permuted by itself to produce rounds. A @code{type-error} is signaled if @var{row} is
not a @code{row}.
@example
@group
 (order !13527486) @result{} 7
 (order !31256784) @result{} 15
 (order !12345678) @result{} 1
@end group
@end example"
  (iter (with bells := (row-bells row))
        (for pos :from 0 :below (length bells))
        (for bell := (aref bells pos))
        (when (<= bell pos)
          (next-iteration))
        (for n := (iter (setf bell (aref bells bell))
                        (when (< bell pos)
                          (return nil))
                        (until (eql bell pos))
                        (counting t)))
        (unless n
          (next-iteration))
        (reducing (+ n 2) :by #'lcm :initial-value 1)))

(defun cycles (row)
  "Returns a list of lists of bells. Each of the sublists is the orbit of all of its
elements in @var{row}. One cycles are included. Thus, if @var{row} is a lead head, all the
sublists of length one are hunt bells, all the rest being working bells; if there are two
or more sublists of length greater than one the corresponding method is differential. The
resulting sublists are each ordered such that the first bell is the lowest numbered bell
in that cycle, and the remaining bells occur in the order in which a bell traverses the
cycle. Within the top level list, the sublists are ordered such that the first bell of
each sublist appear in ascending numerical order.
@example
@group
 (cycles !13572468) @result{} ((0) (1 4 2) (3 5 6) (7))
 (format nil \"~@{(~@{~C~^,~@})~^, ~@}\"
             (mapcar #'(lambda (x) (mapcar #'bell-name x))
             (cycles !13572468)))
     @result{} \"(1), (2,5,3), (4,6,7), (8)\"
@end group
@end example"
  (iter (with bells-used := 0)
        (with bells := (row-bells row))
        (for i :from 0 :below (length bells))
        (unless (logbitp i bells-used)
          ;; Be careful to make the following inner loop get the order correct.
          (collect (iter (with result := nil)
                         (for j :initially (aref bells i) then (aref bells j))
                         (until (logbitp j bells-used))
                         (push j result)
                         (setf (ldb (byte 1  j) bells-used) 1)
                         (finally (return result)))))))

(defun tenors-fixed-p (row &optional (starting-at 6))
  "Returns true if and only if all the bells of @var{row} at positions @var{starting-at}
or higher are in their rounds positions. In the degenerate case of @var{starting-at} being
equal to or greater than the stage of @var{row} it returns true. Note that it is
equivalent to @code{(not (null (alter-stage @var{row} @var{starting-at})))}. If not
supplied @var{starting-at} defaults to @code{6}, that is the position of the bell
conventionally called the seven, though represented in Roan by the small integer @code{6}.
Signals a @code{type-error} if @var{row} is not a @code{row} or @var{starting-at} is not a
non-negative integer.
@example
@group
 (tenors-fixed-p !13254678) @result{} t
 (tenors-fixed-p !13254678 5) @result{} t
 (tenors-fixed-p !13254678 4) @result{} nil
 (tenors-fixed-p !54321) @result{} t
 (tenors-fixed-p !54321 4) @result{} nil
@end group
@end example"
  (check-type* row row)
  (check-type* starting-at (integer 0 *))
  (%tenors-fixed-p (row-bells row) starting-at))

(defun %tenors-fixed-p (bells starting-at)
  (iter (for i :from starting-at :below (length bells))
        (always (eql (aref bells i) i))))

(defun alter-stage (row &optional (new-stage *default-stage*))
  "If there is a @code{row}, @var{r}, of stage @var{new-stage} such that
@code{(equalp (permute (rounds @var{new-stage}) @var{r}) @var{row})} then returns @var{r},
and otherwise @code{nil}. That is, it returns a row of the @var{new-stage} such that the
first bells are as in @var{row}, and any new or omitted bells are in rounds order. If not
supplied @var{new-stage} defaults to the current value of @code{*default-stage*}. Signals
a @code{type-err} if @var{row} is not a @code{row} or @var{new-stage} is not a
@code{stage}.
@example
@group
 (alter-stage !54321 10) @result{} !5432167890
 (alter-stage !5432167890 6) @result{} !543216
 (alter-stage !54321 4) @result{} nil
 (alter-stage !5432167890 4) @result{} nil
@end group
@end example"
  (check-type* row row)
  (check-type* new-stage stage)
  (let ((stage (stage row)))
    (cond ((eql stage new-stage)
           row)
          ((< stage new-stage)
           (permute (rounds new-stage) row))
          (t (let ((bells (row-bells row)))
               (when (%tenors-fixed-p bells new-stage)
                 (iter (with result := (make-array new-stage :element-type 'bell))
                       (for i :from 0 :below new-stage)
                       (for b :in-vector bells)
                       (setf (aref result i) b)
                       (finally (return (%make-row result))))))))))

(defun which-lead-head (bells next-fn)
  (iter (with last := (- (length bells) 1))
        (with result)
        (for p :first last :then (funcall next-fn p))
        (for b := (aref bells p))
        (for prev :previous b)
        (for i :from 0)
        (if-first-time (when (eql b last)
                         (return nil))
                       (progn
                         (until (eql p last))
                         (cond ((not (eql b (funcall next-fn prev))) (return nil))
                               ((eql p last) (finish))
                               ((eql b last) (setf result i)))))
        (finally (return result))))

(defun which-plain-bob-lead-head (row)
  "If @var{row} is a lead head of a plain course of Plain Bob at its stage returns a
positive integer identifying which lead head it is; returns @code{nil} if @var{row} is not
a Plain Bob lead head. If @var{row} is the first lead head of a plain course of Plain Bob
@code{1} is returned, if the second @code{2}, etc. For the purposes of this function
rounds is not a Plain Bob lead head, nor is any row below minimus. Signals a
@code{type-error} if @var{row} is not a @code{row}.
@example
@group
 (which-plain-bob-lead-head !13527486) @result{} 1
 (which-plain-bob-lead-head !42638507T9E) @result{} 10
 (which-plain-bob-lead-head !129785634) @result{} nil
 (which-plain-bob-lead-head !12345) @result{} nil
 (which-plain-bob-lead-head !132) @result{} nil
@end group
@end example"
  (let* ((bells (row-bells row)) (stage (length bells)))
    (and (zerop (aref bells 0))
         (> stage 3)
         (which-lead-head bells #'(lambda (n)
                                    (if (oddp n)
                                        (let ((result (+ n 2)))
                                          (cond ((< result stage) result)
                                                ((eql result stage) (+ n 1))
                                                (t (- n 1))))
                                        (let ((result (- n 2)))
                                          (if (>= result 2) result 1))))))))

(defun which-grandsire-lead-head (row)
  "If @var{row} is a lead head of a plain course of Grandsire at its stage returns a
positive integer identifying which lead head it is; returns @code{nil} if @var{row} is not
a Grandsire lead head. If @var{row} is the first lead head of a plain course of Grandsire
@code{1} is returned, if the second @code{2}, etc. For the purposes of this function
rounds is not a Grandsire lead head, nor is any row below minimus. Signals a
@code{type-error} if @var{row} is not a @code{row}.
@example
@group
 (which-plain-bob-lead-head !1253746) @result{} 1
 (which-plain-bob-lead-head !28967453) @result{} 4
 (which-plain-bob-lead-head !135264) @result{} nil
 (which-plain-bob-lead-head !1243) @result{} 1
 (which-plain-bob-lead-head !12345) @result{} nil
@end group
@end example"
  (let* ((bells (row-bells row)) (stage (length bells)))
    (and (zerop (aref bells 0))
         (eql (aref bells 1) 1)
         (> stage 3)
         (which-lead-head bells #'(lambda (n)
                                    (if (oddp n)
                                        (let ((result (+ n 2)))
                                          (cond ((< result stage) result)
                                                ((eql result stage) (+ n 1))
                                                (t (- n 1))))
                                        (let ((result (- n 2)))
                                          (if (>= result 2) result 3))))))))


;;; Place notation

(define-constant +place-notation-chars+
    (let ((result (copy-seq +named-bells+)))
      (iter (for c :in-string "xX-")
            (setf (svref result (char-code c)) #\x))
      (iter (for c :in-string ".,()[]")
            (setf (svref result (char-code c)) c))
      result)
  :test #'equalp)

(define-constant +cross-changes+
    (let ((result (make-array (+ +maximum-stage+ 1) :initial-element nil))
          (crossed-bells (make-array (* 2 (floor +maximum-stage+ 2)) :element-type 'bell)))
      (iter (for i :from 0 :to (- (length crossed-bells) 2) :by 2)
            (setf (aref crossed-bells i) (+ i 1) (aref crossed-bells (+ i 1)) i))
      (iter (for i :from (length crossed-bells) :downto +minimum-stage+ by 2)
            (setf (aref result i) (%make-row (subseq crossed-bells 0 i))))
      result)
  :test #'equalp)

(define-constant +hunt-changes+
    (let ((result (make-array (+ +maximum-stage+ 1) :initial-element nil))
          (crossed-bells (make-array (* 2 (floor +maximum-stage+ 2))
                                     :element-type 'bell)))
      (setf (aref crossed-bells 0) 0)
      (iter (for i :from 1 :to (- (length crossed-bells) 2) :by 2)
            (setf (aref crossed-bells i) (+ i 1) (aref crossed-bells (+ i 1)) i))
      (iter (for i :from (* 2 (ceiling +minimum-stage+ 2)) :to +maximum-stage+ :by 2)
            (for b := (subseq crossed-bells 0 i))
            (setf (aref b (- i 1)) (- i 1))
            (setf (aref result i) (%make-row b)))
      result)
  :test #'equalp)

;;; We cache constructed changes, which in most cases results in sharing of the resulting
;;; rows.

(defconstant +change-cache-size+ 100)

(define-thread-local *change-cache* nil)

(defun place-notation-error (parsing format-string &rest args)
  (apply (if parsing #'simple-parse-error #'error) format-string args))

(defmacro with-bell-property-resolver (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((ppcre:*property-resolver* #'(lambda (property-name)
                                          (eswitch (property-name :test #'string-equal)
                                            ("bell" #'bell-from-name)))))
       ,@body)))

(with-bell-property-resolver
  (let ((change "(?:x|-|(?:\\p{bell}+|\\(\\p{bell}\\p{bell}\\))+|\\[\\p{bell}+\\])"))
    (define-constant +extended-place-notation-scanner+*
      (ppcre:create-scanner (format nil "^((?:\\.*~A\\.*)+)(?:,((?:\\.*~:*~A\\.*)+))?" change)
                            :case-insensitive-mode t)
      :test #'same-type-p)
    (define-constant +place-notation-fragment-scanner+
      (ppcre:create-scanner (format nil "\\.+|~A" change) :case-insensitive-mode t)
      :test #'same-type-p)))

(defun parse-place-notation (string &key (stage *default-stage*) (start 0) end
                                      junk-allowed)
  "===summary===
@cindex Lisp reader
@cindex reader macro
@cindex sharp bang
@cindex @code{#!} reader macro
@cindex place notation
@cindex palindromes
@cindex jump changes
@cindex involutions
@cindex London Treble Jump Minor
@cindex parentheses
@cindex brackets
@cindex @samp{(} in place notation
@cindex @samp{[} in place notation
@cindex dot
@cindex @samp{.} in place notation
@cindex comma
@cindex @samp{,} in place notation
@cindex @samp{x} in place notation
@cindex @samp{-} in place notation
@cindex @samp{)} following place notation
@cindex quote
Place notation manipulated by Roan is extended to support jump changes and comma as an
unfolding operator for easy notation of palindromic sequences of changes.

Jump changes may be included in the place notation in two ways. Within changes may appear
parenthesized pairs of places, indicating that the bell in the first place jumps to the
second place. Thus the change (13)6 corresponds to the jump change 231546. As usual
implied leading or lying places may be omitted, so that could also be written simply (13).
However, just as with ordinary place notation, all internal places must be noted
explicitly; for example, the change (13)(31) is illegal, and must be written (13)2(31).
Using this notation the first half-lead of London Treble Jump Minor can be written
3x3.(24)x2x(35).4x4.3.

Jump changes may also be written by writing the full row between square brackets. So that
same half-lead of London Treble Jump Minor could instead be notated
3x3[134265]x2x[214536]4x4.3. Or they can be mixed 3x3[134265]x2x(35).4x4.3.

Palindromes may be conveniently notated using a comma operator, which means the changes
preceding the comma are rung backwads, following the last of the changes before the comma,
which is not repeated; followed by the changes following the comma, similarly unfolded.
Thus x3x4,2x3 is equivalent to x3x4x3x2x3x2. A piece of place notation may include at most
one comma. Neither the changes before the comma nor after it may be empty. Any piece of
place notation including a comma is necessarily of even length.

If jump changes appear in place notation that is being unfolded then when rung in reverse
the jump changes are inverted; this makes no difference to ordinary changes, which are
always involutions, but is important for jump changes that are not involutions. If the
central change about which the unfolding operation takes place, that is the last change
in a sequence of changes being unfolded, is not an involution an error is signaled. As an
example, a plain lead of London Treble Jump Minor can be notated as
3x3.(24)x2x(35).4x4.3,2 which is equivalent to
3x3.(24)x2x(35).4x4.3.4x4.(53)x2x(42).3x3.2.

While place notation is normally written using dots (full stops) only between non-cross
changes, @code{parse-place-notation} will accept, and ignore, them between any changes,
adjacent to other dots, and before and after place notation to be parsed. This may
simplify operation with other software that emits place notation with extraneous dots.

Just as Roan can augment the Lisp reader with @samp{!} to read @code{row}s, it can augment
it with the @samp{#!} reader macro to read place notatation. The stage at which the place
notation is to be interpreted can be written as an integer between the @samp{#} and the
@samp{!}. If no explict stage is provided the current value (at read time) of
@code{*default-stage*} is used. The sequence of place notation must be followed by a
character that cannot appear in place notation, such as whitespace, or by end of file.
There is an exception that an unbalanced close parenthesis will also end the reading; this
allows using this to read place notation in lists and vectors without requiring whitespace
following the place notation. The place notation may be extended with the comma unfolding
operator, and with jump changes. The stage at which the place notation is being iterpreted
is not considered in deciding which characters to consume; all that might apply as place
notation at any stage will be consumed. If some are not appropriate an error will only be
signaled after all the continguous, place notation characters have been read.

Note that, unlike @code{row}s, which are Lisp atoms, the result of reading place notation
is a list, so @samp{#!} quotes it. This is appropriate in the usual case where the result
of @samp{#!} is evaluated, but if used in a context where it is not evaluated care must
be exercised.

This @samp{#!} syntax can be turned on and off by using @ref{roan-syntax}. By default it
is off when Roan is loaded. It is also possible to control this syntax by using
@url{https://github.com/melisgl/named-readtables/, Named Readtables}; see
@ref{roan-syntax} for further details.
@example
@group
 ROAN> #6!x2,1
 (!214365 !124365 !214365 !132546)
 ROAN> '(symbol #6!x2,1 x #6!x2x1)
 (SYMBOL '(!214365 !124365 !214365 !132546) X
         '(!214365 !124365 !214365 !132546))
 ROAN> `(symbol ,#6!x2,1 x ,#6!x2x1)
 (SYMBOL (!214365 !124365 !214365 !132546) X
         (!214365 !124365 !214365 !132546))
 ROAN> #6!x2
 (!214365 !124365)
 ROAN> (equalp #10!x1x4,2 #10!x1x4x1x2)
 T
 ROAN> #6!x3.(13)(64)
 (!214365 !213546 !231645)
 ROAN> #6!x3.(13).(64)
 (!214365 !213546 !231546 !132645)
 ROAN> #6!x3[231546](64)
 (!214365 !213546 !231546 !132645)
@end group
@end example
===endsummary===
Parses place notation from @var{string}, returning a list of @code{row}s, representing
changes, of stage @var{stage}. The place notation is parsed as applying to stage
@var{stage}, which, if not supplied, defaults to current value of @code{*default-stage*}.
Only that portion of @var{string} between @var{start} and @var{end} is parsed; @var{start}
should be a non-negative integer, and @var{end} either an integer larger than @var{start}
or @code{nil}, which latter is equivalent to the length of @var{string}. If
@var{junk-allowed}, a generalized Boolean, is @code{nil}, the default, @var{string} must
consist of the place notation parsed and nothing else; otherwise non-place notation
characters may follow the place notation. For purposes of parsing @var{stage} is not
initially considered: if the place notation is only appropriate for higher stages it will
not terminate the parse even if @var{junk-allowed} is true, it will instead signal an
error. Two values are returned. The first is a list of @code{row}s, the changes parsed.
The second is the index of the next character in @var{string} following the place notation
that was parsed.

If the section of @var{string} delimited by @var{start} and @var{end} does not contain
place notation suitable for @var{stage} a @code{parse-error} is signaled. If
@var{string} is not a string, @var{stage} is not a @code{stage} or @var{start} or
@var{end} are not suitable bounding index designators a @code{type-error} is signaled.
@example
@group
 (multiple-value-list (parse-place-notation \"x2.3\" :stage 6))
     @result{} ((!214365 !124365 !213546) 4)
@end group
@end example"
  (check-type* string string)
  (check-type* stage stage)
  (check-type* start (integer 0))
  (check-type* end (or null (integer 0)))
  (when (equal string "")
    (place-notation-error t "Can't parse an empty string as place notation."))
  ;; Curiously scan-to-strings accepts only an integer, and not a bounding index
  ;; designator, as the value of :END.
  (unless end
    (setf end (length string)))
  (unless (and (< start end) (<= end (length string)))
    (place-notation-error t ":START (~D) and :END (~D) do not bound a substring of ~S."
                          start end string))
  (multiple-value-bind (match segments)
      (ppcre:scan-to-strings +extended-place-notation-scanner+* string
                             :start start :end end :sharedp t)
    (unless match
      (place-notation-error t "~S does not appear to contain place notation." string))
    (let ((first (%parse-place-notation (aref segments 0) stage))
          (next (+ start (length match))))
      (unless (or junk-allowed (eql next end))
        (place-notation-error t "~S contains additional characters after place notation."
                              string))
      (values (cond ((null (aref segments 1)) first)
                    ((zerop (length (aref segments 1))) (nunfold first))
                    (t (nconc (nunfold first)
                              (nunfold (%parse-place-notation (aref segments 1) stage)))))
              next))))

(defun %parse-place-notation (segment stage)
  ;; assumes the caller has validated that the string SEGMENT is correctly formed
  ;; place notation
  (iter (for change :in (ppcre:all-matches-as-strings +place-notation-fragment-scanner+
                                                      segment))
        (for c := (schar change 0))
        (unless (eql c #\.)
          (collect (case c
                     (#\[
                      (let ((r (parse-row change :start 1 :end (- (length change) 1))))
                        (unless (eql (stage r) stage)
                          (place-notation-error t "~A is not of stage ~A."
                                                r (stage-name stage)))
                        r))
                     ((#\x #\X #\-)
                      (or (svref +cross-changes+ stage)
                          (place-notation-error t "Cross (~A) cannot be used at odd stage (~A)."
                                                c (stage-name stage))))
                     (t (%parse-change change stage)))))))

(defun %parse-change (change stage)
  ;; This assumes that the caller has validated that the string CHANGE is composed of
  ;; places and of pairs of places within parentheses.
  ;; TODO Consider rewriting this to use Ander Holroyd's bipartite graph algorithm.
  (let ((places (make-array stage :initial-element nil))
        (used (make-array stage :initial-element nil)))
    (flet ((insert (b &optional (p b))
             (when (or (>= b stage) (and (>= p stage) (setf b p)))
               (place-notation-error t "~S is too large for stage ~A."
                                     (bell-name b t) (stage-name stage)))
             (when (or (svref used b) (svref places p))
               (place-notation-error t "Can't interpret change ~A." change))
             (setf (svref used b) t (svref places p) b)))
      (iter (for i :from 0 :below (length change))
            (for c := (schar change i))
            (cond ((eql c #\()
                   (insert (bell-from-name (schar change (+ i 1)))
                           (bell-from-name (schar change (+ i 2))))
                   (incf i 3))
                  (t (insert (bell-from-name (schar change i))))))
      ;; Work out if an implicit lead place is needed.
      (unless (svref places 0)
        (cond ((svref used 0) (insert 1 0))
              ((svref used 1) (insert 0 0))
              (t (iter (for i :from 1)
                       (until (or (svref places i) (svref used i)))
                       (finally (if (oddp i) (insert 0 0) (insert 1 0)))))))
      ;; Work out the interior of the change.
      (iter (for i :from 1 :to (- stage 2))
            (unless (svref places i)
              (if (svref used (- i 1))
                  (insert (+ i 1) i)
                  (insert (- i 1) i))))
      ;; Finally see if an implicit lying place is needed.
      (unless (svref places (- stage 1))
        (if (svref used (- stage 2))
            (insert (- stage 1))
            (insert (- stage 2) (- stage 1)))))
    (%make-row (make-array stage :element-type 'bell :initial-contents places))))

(defun nunfold (changes)
  (let ((reverse (reverse changes)))
    (unless (involutionp (first reverse))
      (place-notation-error t
                            "Can't unfold changes around a non-involution jump change: ~A."
                            (first reverse)))
    (nconc changes (mapcar #'inverse (rest reverse)))))

(defun read-place-notation (&optional
                              (stream *standard-input*)
                              (stage *default-stage*)
                              (eof-error-p t)
                              (eof-value nil)
                              (recursive-p nil))
  "Reads place notation from a stream, resulting in a list of @code{row}s representing
changes. Reads all the consecutive characters that can appear in (extended) place
notation, and then tries to parse them as place notation. It accumulates characters that
could appear as place notation at any stage, even stages above @var{stage}. The sequence
of place notation must be followed by a character that cannot appear in place notation,
such as whitespace, or by end of file. There is an exception, in that an unbalanced close
parenthesis will also end the read; this allows using this to read place notation in lists
and vectors without requiring whitespace following the place notation. The place notation
may be extended with the comma unfolding operator, and with jump changes, as in
@code{parse-place-notation}. The argument @var{stream} is a character stream open for
reading, and defaults to the current value of @code{*standard-input*}; @var{stage} is a
@code{stage}, an integer, and defaults to the current value of @code{*default-stage*}; and
@var{eof-error-p}, @var{eof-value} and @var{recursive-p} are as for the standard
@code{read} function, defaulting to @code{t}, @code{nil} and @code{nil}, respectively.
Returns a non-empty list of @code{row}s, all of stage @var{stage}. Signals an error if no
place notation constituents are available, if the characters read cannot be parsed
as (extended) place noation at @var{stage}, or if one of the usual errorneous conditions
while reading occurs."
  (declare (ignore recursive-p))
  (let ((first-char (read-char stream eof-error-p (and eof-value '#0=#:eof) t)))
    (if (eq first-char '#0#)
        eof-value
        (iter (with paren-depth := 0)
              (for c :initially first-char :then (read-char stream nil nil t))
              (while (and c (svref +place-notation-chars+ (char-code c))))
              (case c
                (#\( (incf paren-depth))
                (#\) (if (> paren-depth 0)
                         (decf paren-depth)
                         (finish))))
              (collect c :into result)
              (finally (when c (unread-char c stream))
                       (return (values (parse-place-notation (coerce result 'string)
                                                             :stage stage))))))))

;; Make the Lisp reader recognize things beginning with '#n!' as place notation at stage n.
;; If not supplied, n defaults to *default-stage* (its value at read time). The result is
;; quoted, and typically only useful in a context in which it will be evaluated.

(defun sharp-bang-reader (stream subchar arg)
  (declare (ignore subchar))
  (values `',(read-place-notation stream (or arg *default-stage*))))

;; TODO can I delete this in favor of the defreadtable?
;; (set-dispatch-macro-character #\# #\! #'sharp-bang-reader)

(defconstant +initial-cross-character+ #\x)

(defparameter *cross-character* +initial-cross-character+
  "The character used by default as ``cross'' when writing place notation. Must be a
character designator for one of @code{#\\x}, @code{#\\X} or @code{#\\-}. Its initial
default value is a lower case @samp{x}, @code{#\\x}.")

(defmacro with-initial-format-characters (&body body)
  `(let ((*print-bells-upper-case* +initial-print-bells-upper-case+)
         (*cross-character* +initial-cross-character+))
     ,@body))

(defun write-place-notation (changes &key
                                       (stream *standard-output*)
                                       (escape *print-escape*)
                                       (comma nil)
                                       (elide :lead-end)
                                       (cross *cross-character*)
                                       (upper-case *print-bells-upper-case*)
                                       (jump-changes :jumps))
  "Writes to @var{stream} characters representing place notation for @var{changes}, a list
of @code{row}s.

The list @var{changes} should be a non-empty list of @code{row}s, all of the same stage.
The @var{stream} should a character stream open for writing. It defaults to the current
value of @code{*standard-output*}. If the generalized boolean @var{escape}, which defaults
to the current value of @code{*print-escape*}, is true the place notation will be written
using the @samp{#!} read macro to allow the Lisp @code{read} function to read it; in this
case the stage will always be explicitly noted between the @samp{#} and the @samp{!}. If
the generalized boolean @var{upper-case}, which defaults to the current value of
@code{*print-bells-upper-case*}, is true positions notated using letters will be written
in upper case, and otherwise in lower case.

The argument @var{cross} controls which character is used to denote a cross change at even
stages. It must be a character designator for @code{ #\\x}, @code{#\\X} or @code{#\\-},
and defaults to the current value of @code{*cross-character*}.

The argument @var{jump-changes} should be one of @code{nil}, @code{:jumps} or @code{:full}.
It determines how jump changes will be notated. If it is @code{nil} and @var{changes}
contains any jump changes an error will be signaled. If it is @code{:jumps} any jump
changes will be notated using pairs of places between parentheses. While
@code{parse-place-notation} and @code{read-place-notation} can interpret ordinary conjunct
motion or even place making notated in parentheses, @code{write-place-notation} will only
use parentheses for bells actually moving more than one place. If @var{jump-changes} is
@code{:full} jump changes will be notated as a row between square brackets. Again, while
ordinary changes notated this way can be parsed or read, @code{write-place-notation} will
only use bracket notation for jump changes.

The argument @var{elide} determines whether, and how, to omit leading and/or lying places.
If the stage of the changes in @var{changes} is odd, or if @var{elide} is @code{nil}, no
such elision takes place. Otherwise @var{elide} should be one of @code{:interior},
@code{:leading}, @code{:lying} or @code{:lead-end}, which last is its default value. For
any of these non-nil values leading or lying places will always be elided if there are
interior places. They differ only for hunts (that is, changes with both a leading and
lying place, and no interior places). If @code{:interior}, no elision takes place if there
are no interior places. If @code{:leading}, the '1' is elided as implicitly available. If
@code{:lying}, the lying place is elided, so that the result is always '1'. The value
@code{:lead-end} specifies the same behavior as @code{:lying} for all the elements of
@var{changes} except the last, for which it behaves as @code{:leading}; this is often
convenient for notating leads of treble dominated methods at even stages.

If the generalized boolean @var{comma} is true an attempt is made to write @var{changes}
using a comma operator separating it into palindromes. In general there can be multiple
ways of splitting an arbitrary piece of place notation into palindromes. If this is the
case the choice is made to favor first a division that has the palindrome after the comma
of length one, and if that is not possible the division that has the shortest palindrome
before the comma. Any sequence of changes of length two can be trivially divided into
palindromes, but notating them with a comma is unhelpful, so @var{comma} applies only to
even length lists of changes of length greater than two. Whether or not a partitioning
into palindromes was possible can be determined by examining the second value returned by
this function, which will be true only if a comma was written.

Returns two values, @var{changes}, and a generalized Boolean indicating whether or not the
result was written with a comma.

Signals an error if @var{changes} is empty, or contains rows of different stages, if
@var{stream} is not a character stream open for writing, or if any of the usual IO errors
occurs."
  (check-type* changes list)
  (check-type* elide (member nil :interior :leading :lying :lead-end))
  (when (and cross (symbolp cross))
    (setf cross (symbol-name cross)))
  (when (and (stringp cross) (eql (length cross) 1))
    (setf cross (char cross 0)))
  (check-type* cross (member #\x #\X #\-))
  (check-type* jump-changes (member nil :jumps :full))
  (let ((*cross-character* cross) (*print-bells-upper-case* upper-case))
    (multiple-value-bind (stage length contains-jump-changes) (changes-info changes)
      (when (and contains-jump-changes (not jump-changes))
        (jump-change-error "~S contains jump changes." changes))
      (when (oddp stage)
        (setf elide nil))
      (unless (and (evenp length) (> length 2))
        (setf comma nil))
      (when escape
        (format stream "#~D!" stage))
      (flet ((write-segment (seg last)
               (%write-place-notation seg stream stage
                                      (if (and (eq elide :lead-end) (not last))
                                          :lying
                                          elide)
                                      jump-changes)))
        (when comma
          (multiple-value-bind (first second)
              (split-palindromic-changes changes length contains-jump-changes)
            (when first
              (write-segment first nil)
              (princ #\, stream)
              (write-segment second t)
              (return-from write-place-notation (values changes t)))))
      (write-segment changes t)
      (values changes nil)))))

(defun changes-info (changes)
  (let ((stage nil) (length 0) (contains-jump-changes nil))
    (dolist (c changes)
      (check-type* c row)
      (incf length)
      (unless (changep c)
        (setf contains-jump-changes t))
      (if stage
          (unless (eql (stage c) stage)
            (error "The elements of ~S are not all of the same stage." changes))
          (setf stage (stage c))))
    (unless stage
      (error "No changes provided."))
    (values stage length contains-jump-changes)))

(define-condition jump-change-error (simple-error) ())

(defun jump-change-error (control &rest arguments)
  (error 'jump-change-error :format-control control :format-arguments arguments))

(defun test-ordinary-palindrome (changes n)
  (if (zerop n)
      (values (list (first changes)) (rest changes))
      (multiple-value-bind (result more)
          (test-ordinary-palindrome (rest changes) (- n 1))
        (if (and result (equalp (first changes) (first more)))
            (values (cons (first changes) result) (rest more))
            (return-from test-ordinary-palindrome nil)))))

(defun test-jump-palindrome (changes n)
  (if (zerop n)
      (and (involutionp (first changes))
           (values (list (first changes)) (rest changes)))
      (multiple-value-bind (result more)
          (test-jump-palindrome (rest changes) (- n 1))
        (if (and result (equalp (first changes) (inverse (first more))))
            (values (cons (first changes) result) (rest more))
            (return-from test-jump-palindrome nil)))))

(defun split-palindromic-changes (changes length contains-jump-changes)
  ;; Assumes caller has ensured changes is a list of changes of even length greater than 2.
  ;; TODO It seems certain there must be a better algorithm possible.
  (let ((test-fn (if contains-jump-changes
                     #'test-jump-palindrome
                     #'test-ordinary-palindrome)))
    (iter (with n := (- (/ length 2) 1))
          (for i :from -1 :below n)
          (for j :first n :then i)
          (multiple-value-bind (first more) (funcall test-fn changes j)
            (when first
              (when-let ((second (funcall test-fn more (- (/ length 2) j 1))))
                (return (values first second))))))))

(defun %write-place-notation (changes stream stage elide jump-changes)
  ;; Assumes the caller has arranged things such that @var{changes} is guaranteed to be a
  ;; non-empty list of changes of the stated stage and that elide may be non-nil only if
  ;; stage is even.
  (with-standard-io-syntax
    (let ((cross-change (aref +cross-changes+ stage))
          (hunt-change (aref +hunt-changes+ stage))
          (dot-required nil))
      (flet ((print-bell (n)
               (princ (bell-name n) stream)))
        (macrolet ((with-dot-context (uses-dot &body body)
                     `(progn
                        (when (and ,uses-dot dot-required)
                          (princ #\. stream))
                        ,@body
                        (setf dot-required ,uses-dot))))
          (iter (for (c . rest) :on changes)
                (when (and (null rest) (eq elide :lead-end))
                  (setf elide :leading))
                (cond ((equalp c cross-change)
                       (with-dot-context nil
                         (princ *cross-character* stream)))
                      ((equalp c hunt-change) ; stage is known to be even
                       (with-dot-context t
                         (unless (eq elide :leading)
                           (print-bell 0))
                         (unless (or (eq elide :lying) (eq elide :lead-end))
                           (print-bell (- stage 1)) stream)))
                      ((or (eql jump-changes :jumps) (changep c))
                       (with-dot-context t
                         (let ((places (make-array stage :initial-element nil))
                               (bells (row-bells c)))
                           (declare (type %bells-vector bells))
                           (iter (for i :from 0 :below stage)
                                 (cond ((eql (aref bells i) i)
                                        (unless (and elide (or (eql i 0)
                                                               (eql i (- stage 1))))
                                          (setf (aref places i) i)))
                                       ((not (<= -1 (- (aref bells i) i) 1))
                                        (setf (aref places (aref bells i)) (list i)))))
                           (iter (for p :in-vector places)
                                 (for i :from 0)
                                 (when p
                                   (if (listp p)
                                       (format stream "(~A~A)"
                                               (bell-name i) (bell-name (first p)))
                                       (princ (bell-name p) stream)))))))
                      ((eq jump-changes :full)
                       (with-dot-context nil
                         (princ #\[ stream)
                         (write-row c :stream stream :escape nil)
                         (princ #\] stream)))
                      (t                ; jump-changes == nil
                       (place-notation-error nil "~A is a jump change."
                                             (row-string c t))))))))))

(defun place-notation-string (changes &rest keys
                              &key comma elide cross upper-case allow-jump-changes)
  "===lambda: (changes &key comma elide cross upper-case allow-jump-changes)
Returns a string of the place notation representing the list @var{changes}. The
arguments are the same as the like named arguments to @code{write-place-notation}. A
leading '#!' is never included in the result.

Signals a @code{type-error} if any elements of @var{changes} are not @code{row}s. Signals
an error if @var{changes} is empty or contains rows of different stages.
@example
@group
 (multiple-value-list
   (place-notation-string #8!x1x4,1 :elide nil))
     @result{} (\"x18x14x18x18\" nil)
 (multiple-value-list
   (place-notation-string #8!x1x4,1 :comma t))
     @result{} (\"x1x4,8\" t)
 (multiple-value-list
   (place-notation-string #8!x1x4,2 :elide :interior))
     @result{} (\"x18x4x18x18\" nil)
@end group
@end example"
  (declare (ignore comma elide cross upper-case allow-jump-changes))
  (with-output-to-string (s)
    (apply #'write-place-notation changes :stream s :escape nil keys)))

(defun canonicalize-place-notation (string-or-changes &rest keys
                                    &key (stage *default-stage* stage-supplied)
                                      (comma t) &allow-other-keys)
  "===lambda: (string-or-changes &key stage comma elide cross upper-case allow-jump-changes)
Returns a string representing the place notation in a canonical form. If
@var{string-or-changes} is a string it should be parseable as place notation at
@var{stage}, which defaults to the current value of @code{*default-stage*}, and otherwise
it should be a list of @code{row}s, all of the same stage. Unless overridden by the other
keyword arguments, which have the same effects as for @code{write-place-notation}, the
canonical form is a compact one using lower case @samp{x} for cross, upper case letters for
high place names, @code{lead-end} style elision of external places, a comma for unfolding
if possible, and notating jump changes as jumps within parentheses.

Signals a @code{type-error} if @var{string-or-changes} is neither a string nor a list, or
if it is a list containing anything other than @code{row}s. Signals a @code{parse-error} if
@var{string-or-changes} is a string and is not parseable at @var{stage}, or if @var{stage}
is not a @code{stage}. Signals an error if @var{cross} is not a suitable character
designator, if @var{allow-jump-changes} is not one of its allowed values, or if
@var{string-or-changes} is a list containing @code{row}s of different stages.
@xref{write-place-notation}.
@example
@group
 (multiple-value-list
   (canonicalize-place-notation \"-16.X.14-6X1\" :stage 6))
     @result{} (\"x1x4,6\" t)
 (multiple-value-list
   (canonicalize-place-notation \"-3-[134265]-1T-\" :stage 12))
     @result{} (\"x3x(24)x1x\" nil)
@end group
@end example"
  (let ((changes (cond ((listp string-or-changes)
                        (when (and stage-supplied
                                   (rowp (first string-or-changes))
                                   (not (eql (stage (first string-or-changes)) stage)))
                          ;; if it's not a row place-notation-string will signal the error
                          (error ":stage ~D was supplied, but ~S is of a different stage"
                                 stage (first string-or-changes)))
                        string-or-changes)
                       (t (parse-place-notation string-or-changes :stage stage)))))
    (with-initial-format-characters
      (apply #'place-notation-string changes :comma comma :allow-other-keys t keys))))
