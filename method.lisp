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

(in-package :roan)


;;; Methods

;;; Don't confuse change ringing methods with CLOS methods; they are completely different
;;; animals. The ringing package shadows the symbols method and method-name.

(defclass method ()
  ((stage :initform nil :accessor method-stage)
   (name :accessor method-name)
   (place-notation :initform nil :accessor method-place-notation)
   (properties :initform nil :accessor method-properties)
   (traits :initform nil))
  (:documentation "===summary===
Roan provides the @code{method} type to describe change ringing methods, not to be
confused with CLOS methods. A @code{method} can only describe a method that can be viewed
as a fixed sequence of changes, including jump changes; while this includes all methods
recognized by the Central Council (as of mid-2017), and many others, it does exclude, for
example, Dixonoids. A @code{method} has a stage, a name, an associated place-notation, and
a property list, though any or all of these may be @code{nil}. In the case of the stage,
name or place notation @code{nil} indicates that the corresponding value is not known. The
stage, if known, should be a @code{stage}, and the name and place notation, if known,
should be strings. The name is not just the portion that the Central Council considers its
name: it also includes any explictly named class, such as 'Surprise', as well as modifiers
such as 'Little' or 'Differential'. For example, the name of Littleport Little Surprise
Royal is \"Littleport Little Surprise\".

The property list may be used to store further information about a method. For example,
@code{lookup-methods-by-name} typically adds the properties @code{:first-tower} and
@code{:first-hand} to methods it creates with details of their first performances. Besides
accessing the property list as a whole with @code{method-properties} individual properties
can be interrogated and set with @code{method-property}.

Because a ringing method is unrelated to a CLOS method the @code{roan} package
shadows @code{common-lisp:method} and @code{common-lisp:method-name}.
===endsummary===
Describes a change ringing method, typically including its name, stage and place notation,
and possibly other properties of the method."))

(defmethod (setf method-stage) :before (value (method method))
  (unless (eql value (method-stage method))
    (check-type* value (or null stage))
    (clear-method-traits method)))

(defmethod method-stage ((method t))
  ;; Signal a less inscrutable error if method-stage is misused.
  (error 'type-error :expected-type 'method :datum method))

(defmethod (setf method-name) :before (value (method method))
  (check-type* value (or null string)))

(defmethod method-name ((method t))
  ;; Signal a less inscrutable error if method-name is misused.
  (error 'type-error :expected-type 'method :datum method))

(defmethod (setf method-place-notation) :before (value (method method))
  (unless (equalp value (method-place-notation method))
    (check-type* value (or null string))
    (clear-method-traits method)))

(defmethod method-place-notation ((method t))
  ;; Signal a less inscrutable error if method-place-notation is misused.
  (error 'type-error :expected-type 'method :datum method))

(defmethod (setf method-properties) :before (value (method method))
  (check-type* value list))

(defmethod method-properties ((method t))
  ;; Signal a less inscrutable error if method-properties is misused.
  (error 'type-error :expected-type 'method :datum method))

(defun method-title (method &optional show-unknown)
  "Returns a string containing as much of the @var{method}'s title as is known. If
@var{show-unknown}, a generalized boolean defaulting to false, is not true then an unknown
name is described as \"Unknown\", and otherwise is simply omitted. If neither the name nor
the stage is known, and @var{show-unknown} is @code{nil}, then @code{nil} is returned
instead of a string. Can be used with @code{setf}, in which case it potentially sets both
the name and stage of @var{method}.
@example
@group
 (method-title (method :stage 8 :name \"Advent Surprise\"))
   @result{} \"Advent Surprise Major\"
 (method-title (method :stage 8))
   @result{} \"Major\"
 (method-title (method :stage 8) t)
   @result{} \"Unknown Major\
 (method-title (method :stage nil :name \"Advent Surprise\"))
   @result{} \"Advent Surprise\"
 (method-title (method :stage nil))
   @result{} nil
@end group
@end example"
  (let ((name (method-name method)) (stage (stage-name (method-stage method))))
    (if (or name show-unknown)
        (format nil "~:[Unknown~;~:*~A~]~@[ ~A~]" name stage)
        stage)))

(defun parse-method-title (string)
  "Returns two values, the method name and the stage extracted from @var{string}, if they
are present. If one or both components is missing @code{nil} is returned as the
corresponding value. Signals a @code{type-error} if @var{string} is neither a non-empty
string nor @code{nil}.
@example
@group
 (multiple-value-list
   (parse-method-title \"Advent Surprise Major\"))
     @result{} (\"Advent Surprise\" 8)
@end group
@end example"
  (unless string
    (return-from parse-method-title (values nil nil)))
  (when (stringp string)
    (setf string (collapse-whitespace string)))
  (check-type* string (and string (not (string 0))))
  (let* ((space-pos (position #\Space string :from-end t))
         (stage (stage-from-name (subseq string (if space-pos (1+ space-pos) 0))))
         (name (cond ((null stage) string)
                     (space-pos (subseq string 0 space-pos)))))
    (values name stage)))

(defsetf method-title (method &optional show-unknown) (string)
  `(multiple-value-bind (name stage) (parse-method-title ,string)
     (setf (method-name ,method) (if (and ,show-unknown (equalp name "Unknown")) nil name)
           (method-stage ,method) stage)
     ,string))

(defun method (&key title (stage nil stage-supplied) name place-notation properties)
  "Creates a new @code{method} instance, with the specified @var{stage}, @var{name}
@var{place-notation} and @var{properties}. If @var{title} is supplied it provides default
values for @var{name} and @var{stage} if none are explicitly provided or are @code{nil}.
If @var{stage} is not provided, and no default is provided by @var{title}, it defaults to
the current value of @code{*default-stage*}. A @code{type-error} is signaled if
@var{stage} is supplied and is neither @code{nil} nor a @code{stage}; if any of
@var{name}, @var{title} or @var{place-notation} are supplied and are neither @code{nil}
nor a string; or if @code{properties} is supplied and is not a list."
  (check-type* title (or null string))
  (when (and title (or (not stage) (not name)))
    (multiple-value-bind (t-name t-stage) (parse-method-title title)
      (unless name
        (setf name t-name))
      (unless stage
        (setf stage t-stage))))
  (let ((result (make-instance 'method)))
    (setf (method-stage result) (or stage (and (not stage-supplied) *default-stage*)))
    (setf (method-name result) name)
    (setf (method-place-notation result) place-notation)
    (setf (method-properties result) properties)
    result))

(defmethod print-object ((method method) stream)
  (cond (*print-readably*
         (with-slots (stage name place-notation properties) method
           (format stream "(~S~:[~2*~; ~S ~S~] ~S ~S~:[~2*~; ~S ~S~]~:[~2*~; ~S ~S~])"
                   'method
                   name :name name
                   :stage stage
                   place-notation :place-notation place-notation
                   properties :properties properties)))
        (*print-escape*
         (print-unreadable-object (method stream :type t :identity t)
           (with-slots (stage name place-notation) method
             (when name (princ name stream))
             (when stage
               (when name (princ #\Space stream))
               (princ stage stream))
             (when place-notation
               (when (or name stage) (princ #\Space stream))
               (princ place-notation stream)))))
        (t (format stream "~A" (method-title method t)))))

(defun method-property (method key &optional default)
  "Returns the property attached to @var{method} with the given @var{key}, if there is
one, and otherwise returns @var{default}, or @code{nil} if no @var{default} is supplied.
Can be used with @code{setf} to change a property. Signals a @code{type-error} if
@var{method} is not a @code{method}. Just a short-hand for, and equivalent to,
@code{(getf (method-properties @var{method}) @var{key} @var{default})}.
@example
@group
 (let ((m (method :title \"Advent Surprise Major\")))
   (setf (method-property m :first-rung) \"1988-07-31\")
   (method-property m :first-rung))
     @result{} \"1988-07-31\"
@end group
@end example"
  (getf (method-properties method) key default))

(defsetf method-property (method key &optional default) (value)
  `(setf (getf (method-properties ,method) ,key ,default) ,value))


;;; Method traits

;; A method-traits instance is used to cache information about a method that is
;; computable from its stage and place notation. It is only weakly referred to by the
;; method, since it can always be reconstructed at need.

(defstruct (method-traits (:copier nil) (:predicate nil))
  (changes nil)
  (contains-jump-changes nil)
  (lead-length nil)
  (lead-head nil)
  (lead-count nil)
  (plain-lead nil)
  (plain-course nil)
  (course-length nil)
  (hunt-bells nil)
  (primary-hunt-bells nil)
  (secondary-hunt-bells nil)
  (working-bells nil)
  (classification nil)
  (lead-head-code nil))

(defun get-method-traits (method)
  (with-slots (traits) method
    (or (and traits (weak-pointer-value traits))
        (weak-pointer-value (setf traits
                                  (make-weak-pointer (make-instance 'method-traits)))))))

(defun clear-method-traits (method)
  (setf (slot-value method 'traits) nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-accessor-names (slot)
    (check-type slot (and symbol (not keyword)))
    (values (symbolicate "METHOD-TRAITS-" slot)
            (symbolicate "%GET-" slot)
            (symbolicate "METHOD-" slot))))

(defmacro define-method-trait (name (update-fn &optional depends-on) doc-string)
  (multiple-value-bind (direct private public) (get-accessor-names name)
    (let ((dep (and depends-on (get-accessor-names depends-on)))
          (update-executor `(progn (,update-fn method traits) (,direct traits))))
      `(progn
         (defun ,private (method &optional (traits (get-method-traits method)))
           (or (,direct traits)
               ,(if dep
                    `(and (not (,dep traits)) ,update-executor)
                    update-executor)))
         (defun ,public (method)
           ,doc-string
           (check-type* method method)
           (publish-trait (,private method)))))))

(defun publish-trait (thing)
  (cond ((arrayp thing) (copy-seq thing))
        ((atom thing) thing)
        ((atom (first thing)) (copy-seq thing))
        (t (mapcar #'copy-seq thing))))

(defun %update-changes (method traits)
  (when-let ((stage (method-stage method))
             (place-notation (method-place-notation method)))
    (let ((changes (parse-place-notation place-notation :stage stage)))
      (setf (method-traits-changes traits) changes)
      (setf (method-traits-contains-jump-changes traits) (notevery #'changep changes)))))

(define-method-trait changes (%update-changes)
  "If @var{method}'s stage and place-notation have been set returns a fresh list of
@code{row}s, representing changes, that constitute a plain lead of @var{method}, and
otherwise returns @code{nil}. Signals a @code{type-error} if @var{method} is not a
@code{method}. Signals a @code{parse-error} if the place notation string cannot be
properly parsed as place notation at @var{method}'s stage.
@example
@group
 (method-changes (method :stage 6
                         :place-notation \"x2,6\"))
     @result{} (!214365 !124365 !214365 !132546)
@end group
@end example")

(defun canonicalize-method-place-notation (method &rest keys &key (comma t) &allow-other-keys)
  "===lambda: (method &key comma elide cross upper-case allow-jump-changes)
Replaces @var{method}'s place-notation by an equivalent string in canonical form, and
returns that canonical notation as a string. Unless overriden by keyword arguments this is
a compact version with leading and lying changes elided according to @code{:lead-end}
format as for @code{write-place-notation}, partitioned with a comma, if possible, with
upper case letters for high number bells and a lower case @samp{x} for cross. The behavior
can be changed by passing keyword arguments as for @code{write-place-notation}. If
@var{method} has no place-notation or no stage, this function does nothing, and returns
@code{nil}; in particular, if there is place-notation but no stage, the place-notation
will be unchanged.

Signals a @code{type-error} if @var{method} is not a @code{method}, and signals an error
if any of the keyword arguments do not have suitable values for passing to
@code{write-place-notation}. Signals a @code{parse-error} if the place notation string
cannot be properly parsed as place notation at @var{method}'s stage.
@xref{canonicalize-place-notation} and @ref{write-place-notation}.
@example
@group
 (let ((m (method :stage 6
                  :place-notation \"-16.X.14-6X16\")))
   (canonicalize-method-place-notation m)
   (method-place-notation m))
     @result{} \"x1x4,6\"
@end group
@end example"
  (when-let ((changes (%get-changes method)))
    ;; Use slot-value instead of the accessor so we don't clear the method's traits.
    (setf (slot-value method 'place-notation)
          (with-initial-format-characters
            (apply #'place-notation-string changes :comma comma keys)))))

(define-method-trait contains-jump-changes (%update-changes changes)
  "If @var{method}'s stage and place-notation have been set and method contains one or
more jump changes returns true, and otherwise returns @code{nil}. Note that even if the
place notation is set and implies jump changes, if the stage is not set
@code{method-contains-jump-changes} will still return @code{nil}. Signals a
@code{type-error} if @var{method} is not a @code{method}. Signals a @code{parse-error} if
the place notation string cannot be properly parsed as place notation at @var{method}'s
stage.
@example
@group
 (method-contains-jump-changes
   (method :place-notation \"x3x4x2x3x4x5,2\"
           :stage 6))
     @result{} nil
 (method-contains-jump-changes
   (method :place-notation \"x3x(24)x2x(35)x4x5,2\"
           :stage 6))
     @result{} t
 (method-contains-jump-changes
   (method :stage 6))
     @result{} nil
 (method-contains-jump-changes
   (method :place-notation \"x3x(24)x2x(35)x4x5,2\"
           :stage nil))
     @result{} nil
@end group
@end example")

(defun %update-lead-length (method traits)
  (when-let ((changes (%get-changes method traits)))
    (setf (method-traits-lead-length traits) (length changes))))

(define-method-trait lead-length (%update-lead-length)
  "If @var{method}'s stage and place-notation have been set returns a positive integer,
the length of one lead of @var{method}, and otherwise @code{nil}. Signals a
@code{type-error} if @var{method} is not a @code{method}. Signals a @code{parse-error} if
the place notation string cannot be properly parsed as place notation at @var{method}'s
stage.
@example
@group
 (method-lead-length
   (method :title \"Cambridge Surprise Minor\"
           :place-notation \"x3x4x2x3x4x5,2\"))
     @result{} 24
@end group
@end example")

(defun %update-plain-lead (method traits)
  (when-let ((changes (%get-changes method traits)))
    (multiple-value-bind (plain-lead lead-head) (generate-rows changes)
      (setf (method-traits-plain-lead traits) plain-lead)
      (setf (method-traits-lead-head traits) lead-head)
      (iter (for c :in (cycles lead-head))
            (if (rest c)
                (collect c :into w)
                (collect (first c) :into h))
            (finally (setf (method-traits-hunt-bells traits) h)
                     (setf (method-traits-working-bells traits) w))))))

(define-method-trait lead-head (%update-plain-lead)
  "If @var{method}'s stage and place-notation have been set returns a @code{row}, the lead
head generated by one plain lead of @var{method}, and otherwise @code{nil}. If
@var{method} has a one lead plain course the result will be rounds. Signals a
@code{type-error} if @var{method} is not a @code{method}. Signals a @code{parse-error} if
the place notation string cannot be properly parsed as place notation at @var{method}'s
stage.
@example
@group
 (method-lead-head (method :stage 8
                           :place-notation \"x1x4,2\"))
     @result{} !16482735
@end group
@end example")

(define-method-trait plain-lead (%update-plain-lead)
  "If @var{method}'s stage and place-notation have been set returns a fresh list of
@code{row}s, starting with rounds, that constitute the first lead of the plain course of
@var{method}, and otherwise returns @code{nil}. The lead head that starts the next lead is
not included. Signals a @code{type-error} if @var{method} is not a @code{method}. Signals
a @code{parse-error} if the place notation string cannot be properly parsed as place
notation at @var{method}'s stage.
@example
@group
 (method-plain-lead (method :stage 6
                            :place-notation \"x2,6\"))
     @result{} (!123456 !214365 !213456 !124365)
@end group
@end example")

(define-method-trait hunt-bells (%update-plain-lead lead-head)
  "If @var{method}'s stage and place-notation have been set @code{method-hunt-bells}
returns a fresh list of @code{bell}s (that is, small integers, with the treble represented
by zero) that are hunt bells of @var{method} (that is, that return to their starting place
at each lead head), and otherwise returns @code{nil}. The bells in the list are ordered in
increasing numeric order. Note that for a method with no hunt bells this function will
also return @code{nil}.

The CCCBR's taxonomy of methods is largely driven by a division of hunt bells into
``primary'' hunt bells and ``secondary'' hunt bells (see the
@url{http://www.methods.org.uk/ccdecs.htm,Central Council's Decisions} for details). The
@code{method-primary-hunt-bells} and @code{method-secondary-hunt-bells} functions return
lists, again ordered in increasing numeric order, of these hunt bells. The lists returned
by these two functions are disjoint, and, in the absence of jump changes, their union
consists exactly of the contents of the list returned by @code{method-hunt-bells}. The
CCCBR's taxonmy, and its definitions of primary and secondary hunt bells, is predicated on
the absence of jump changes, and it is not clear how they would best be extended for
methods with jump changes. The @code{method-primary-hunt-bells} and
@code{method-secondary-hunt-bells} functions therefore return @code{nil} for any methods
containing jump changes; however, @code{method-hunt-bells} can still be usefully used for
such methods. Note that @code{method-primary-hunt-bells} and
@code{method-secondary-hunt-bells} also return @code{nil} for methods without jump changes
that do not contain any of the relevant hunt bells, as well as for methods that have not
had both their stage and place-notation set.

All three of these functions ignal a @code{type-error} if @var{method} is not a
@code{method}, and signal a @code{parse-error} if the place notation string cannot be
properly parsed as place notation at @var{method}'s stage.
@example
@group
 (method-hunt-bells (method :stage 5
                            :place-notation \"3,1.5.1.5.1\"))
     @result{} (0 1)
 (method-primary-hunt-bells (method :stage 5
                            :place-notation \"3,1.5.1.5.1\"))
     @result{} (0 1)
 (method-secondary-hunt-bells (method :stage 5
                            :place-notation \"3,1.5.1.5.1\"))
     @result{} nil
 (method-primary-hunt-bells (method :stage 5
                            :place-notation \"5.1.5.1.125,2\"))
     @result{} (0)
 (method-secondary-hunt-bells (method :stage 5
                            :place-notation \"5.1.5.1.345,2\"))
     @result{} (1)
@end group
@end example")

(define-method-trait working-bells (%update-plain-lead lead-head)
  "If @var{method}'s stage and place-notation have been set returns a list of lists of
@code{bell}s (that is, small integers, with the treble represented by zero) that are
working bells of @var{method} (that is, that do not return to their starting place at each
lead head), and otherwise returns @code{nil}. The sublists each represent a cycle of
working bells. For example, for a major method with Plain Bob lead heads, there will be
one sublist returned, of length seven, containing the bells 1 through 7; while for a
differential method there will be at least two sublists returned. Each of the sublists is
ordered starting with the smallest bell in that sublist, and then in the order the place
bells follow one another in the method. Within the overall, top-level list the sublists
are ordered such that the first element of each sublist occur in increasing numeric order.
Note that for a method with no working bells (which will then have a one lead plain
course) this function also returns @code{nil}. Signals a @code{type-error} if @var{method}
is not a @code{method}. Signals a @code{parse-error} if the place notation string cannot
be properly parsed as place notation at @var{method}'s stage.
@example
@group
 (method-working-bells (method :stage 7
                               :place-notation \"7.1.7.47,27\"))
     @result{} ((1 4 5) (2 6 3))
@end group
@end example")

(defun %update-lead-count (method traits)
  (if-let ((cycles (%get-working-bells method traits)))
    (iter (with lengths := '())
          (for c :in cycles)
          (pushnew (length c) lengths)
          (finally (setf (method-traits-lead-count traits) (apply #'lcm lengths))))
    (if (roundsp (method-lead-head method)) (setf (method-traits-lead-count traits) 1))))

(define-method-trait lead-count (%update-lead-count)
  "If @var{method}'s stage and place-notation have been set returns a positive integer,
the number of leads in a plain course of @var{method}, and otherwise @code{nil}. Signals a
@code{type-error} if @var{method} is not a @code{method}. Signals a @code{parse-error} if
the place notation string cannot be properly parsed as place notation at @var{method}'s
stage.
@example
@group
 (method-lead-count
   (method :title \"Cambridge Surprise Minor\"
           :place-notation \"x3x4x2x3x4x5,2\"))
     @result{} 5
 (method-lead-count
   (method :title \"Cromwell Tower Block Minor\"
           :place-notation \"3x3.4x2x3x4x3,6\"))
     @result{} 1
 (method-lead-count
   (method :title \"Bexx Differential Bob Minor\"
           :place-notation \"x1x1x23,2\"))
     @result{} 6
@end group
@end example")

(defun %update-course-length (method traits)
  (when-let ((length (method-lead-length method))
             (count (method-lead-count method)))
    (setf (method-traits-course-length traits) (* length count))))

(define-method-trait course-length (%update-course-length)
  "If @var{method}'s stage and place-notation have been set returns a positive integer,
the length of a plain course of @var{method}, and otherwise @code{nil}. Signals a
@code{type-error} if @var{method} is not a @code{method}. Signals a @code{parse-error} if
the place notation string cannot be properly parsed as place notation at @var{method}'s
stage.
@example
@group
 (method-course-length
   (method :title \"Cambridge Surprise Minor\"
           :place-notation \"x3x4x2x3x4x5,2\"))
     @result{} 120
 (method-course-length
   (method :title \"Cromwell Tower Block Minor\"
           :place-notation \"3x3.4x2x3x4x3,6\"))
     @result{} 24
 (method-course-length
   (method :title \"Bexx Differential Bob Minor\"
           :place-notation \"x1x1x23,2\"))
     @result{} 72
@end group
@end example")

(defun %update-plain-course (method traits)
  (when-let ((lead (%get-changes method traits)))
    (iter (for head :initially (rounds (method-stage method)) :then next)
          (for (values rows next) := (generate-rows lead head))
          (nconcing rows :into result)
          (counting t :into n)
          (until (roundsp next))
          (finally (setf (method-traits-plain-course traits) result)
                   (setf (method-traits-lead-count traits) n)))))

(define-method-trait plain-course (%update-plain-course)
  "If @var{method}'s stage and place-notation have been set returns a fresh list of the
@code{row}s that constitute a plain course of @var{method}, and otherwise @code{nil}. The
list returned will start with rounds, and end with the @code{row} immediately preceding the
final rounds. Signals a @code{type-error} if @var{method} is not a @code{method}. Signals
a @code{parse-error} if the place notation string cannot be properly parsed as place
notation at @var{method}'s stage.")

(defmacro define-hunt-path-constants (&rest keys)
  `(progn ,@(iter (for k :in keys)
                  (for i :from 0 :by 2)
                  (collect `(defconstant ,(intern (format nil "+~:@(~A~)+" k))  ,i)))
          (define-constant +hunt-path-keywords+
              (vector ,@(iter (for k :in keys) (nconcing (list k nil))))
            :test #'equalp)))

(define-hunt-path-constants :plain :treble-dodging :treble-place :alliance :hybrid)

(defstruct (hunt-path-info
             (:constructor make-hunt-path-info (bell little-p apex))
             (:copier nil)
             (:predicate nil))
  (bell 0 :type bell)
  ;; kind is the value of a numeric constant, such as +plain+, describing the path
  (kind -1 :type integer)
  ;; little-p is a genearlized boolean that is true if the hunt path is little and false
  ;; otherwise
  (little-p nil)
  ;; apex is non-nil iff the path is what the Central Council calls well-formed, and is
  ;; the index into the lead of the second row of the place made at the apex in the first
  ;; half-lead of the palindrome; while unusual, it is possible for the palindrome to have
  ;; multiple pairs of apices, in which case this is the first of them
  (apex nil)
  ;; section-length is the number of rows in which the hunt bell remains in the same
  ;; dodging position in a treble dodging method; section-length is nil if this is not
  ;; a treble dodging method; making a place is considered moving to a new dodging
  ;; position; in the usual case of the treble single dodging section-length is 4
  (section-length nil))

(define-method-trait primary-hunt-bells (%update-classification classification)
  "===merge: method-hunt-bells 1")

(define-method-trait secondary-hunt-bells (%update-classification classification)
  "===merge: method-hunt-bells 2")

(define-method-trait classification (%update-classification)
  "If @var{method} has had its stage and place-notation set returns a list of keywords
describing how it fits into the CCCBR's taxonomy of methods (see
@url{http://www.methods.org.uk/ccdecs.htm,Central Council's Decisions}), and otherwise
@code{nil}. In the absence of jump changes the first element of the result will be one of
@code{:principle}, @code{:bob}, @code{:place}, @code{:slow-course}, @code{:treble-bob},
@code{:delight}, @code{:surprise}, @code{:treble-place}, @code{:alliance} or
@code{:hybrid}. This may be followed by @code{:differential} and/or @code{:little}, if
appropriate. While the CCCBR considers ``differentials'' as a distinct class from
``principles'', what the CCCBR calls a pure ``differential'' is here described as
@code{(:principle :differential)}, in parallel with with ``differential
hunters@footnote{No, ``differential hunter'' is not a description of an infintesimal horse used for
sport, it is an unattractive name for a meta-class of methods. See the CCCBR Decisions for
further details.}'' Note that a principle, whether or not differential, can never be
little.

The CCCBR's taxonomy deals only with methods not containing jump changes, and it is not at
all clear how it should be extended to deal with methods that do contain them. Therefore
@code{method-classificaiton} simply returns @code{(:jump)} for any such methods, and never
appends @code{:differential} or @code{:little} to such a classification. Be careful when
constructing the titles of such methods as the bands that have rung them have sometimes
used more complex names, such as ``treble jump'' to describe them.

The CCCBR's taxonmy unfortunately does contain some ambiguities around unusual methods;
for example little treble dodging methods containing multiple hunt bells with different
cross section locations. The @code{method-classification} function will take its best
guess in such cases, but there is no guarantee that what it returns is how the Council
will end up classifying such methods if they are rung and named.

Signals a @code{type-error} if @var{method} is not a @code{method}, and a
@code{parse-error} if its place-notation cannot be parsed at its stage.

See also @ref{set-method-classified-name} and @ref{method-hunt-bells}.
@example
@group
 (method-classification (method :stage 8
                                :place-notation \"x3x4x2x34,2\")
   @result{} (:surprise :little)
 (method-classification (method :stage 11
                                :place-notation \"3.1.E.3.1.3,1\")
   @result{} (:principle)
 (method-classification (method :stage 12
                                :place-notation \"58x1x67x67x49x,x\")
   @result{} (:hybrid :differential :little)
@end group
@end example")

(defun %get-hunt-bell-info (method traits bell)
  ;; Assumes that
  ;;  - method has its stage and place notation set
  ;;  - that method contains no jump changes
  ;;  - that method has an even lead length
  ;;  - and that bell is a hunt-bell
  ;; Returns a hunt-path-info
  (iter (with occurences := (make-array (method-stage method) :initial-element 0))
        (with last-pos)
        (for pos :initially bell :then (aref (row-bells c) pos))
        (for prev :previous pos)
        (for c :in (%get-changes method traits))
        (for i :from 0)
        (incf (svref occurences pos))
        (when (eql pos prev)
          (collect i :into places))
        (collect pos :into path)
        (setf last-pos pos)
        (finally
         (when (eql last-pos bell)
           (push 0 places))
         (nconcf path path)   ; make it a circular list
         (let ((result (make-hunt-path-info
                        bell
                        (or (zerop (svref occurences 0))
                            (zerop (svref occurences (- (method-stage method) 1))))
                        (iter (with half-lead-length := (/ (method-lead-length method) 2))
                              (for p :in places)
                              (while (< p half-lead-length))
                              (when (every #'equalp
                                           (nthcdr p path)
                                           (subseq-reversed-circular path
                                                                     (+ p half-lead-length)
                                                                     half-lead-length))
                                (return p))))))
                 (setf (hunt-path-info-kind result)
                       (cond ((or (null places)
                                  (oddp (length places))
                                  (null (hunt-path-info-apex result)))
                              +hybrid+)
                             ((iter (with first-non-zero)
                                    (for n :in-vector occurences)
                                    (cond ((> n 0)
                                           (if first-non-zero
                                               (thereis (not (eql n first-non-zero)))
                                               (setf first-non-zero n)))
                                          (first-non-zero (return nil))))
                              ;; not all non-zero occurrences are eql
                              +alliance+)
                             ((> (length places) 2)
                              +treble-place+)
                             ((> (svref occurences 0) 2)
                              +treble-dodging+)
                             (t +plain+)))
                 (setf (hunt-path-info-section-length result)
                       (and (eql (hunt-path-info-kind result) +treble-dodging+)
                            (iter (with init-pos)
                                  (for p :in (nthcdr (hunt-path-info-apex result) path))
                                  (for prev :previous p)
                                  (if-first-time (setf init-pos p))
                                  (for i :from 0)
                                  (until (or (eql p prev) (>= (abs (- p init-pos)) 2)))
                                  (finally (return i)))))
                 (return result)))))

(defun subseq-reversed-circular (clist start length)
  ;; Returns the reverse of a subsequence of a list, which can be circular. Note that the
  ;; usual Common Lisp subseq does not necessarily work on circular lists.
  (iter (with result)
        (for p :in (nthcdr start clist))
        (repeat length)
        (push p result)
        (finally (return result))))

(define-constant +even-stage-lead-head-codes+
    #3A(((#\a #\b #\c) (#\f #\e #\d)) ((#\g #\h #\j) (#\m #\l #\k)))
  :test #'equalp)

(define-constant +odd-stage-lead-head-codes+ #2A((#\p #\q) (#\r #\s)) :test #'equalp)

(defconstant +seconds-index+ 0)
(defconstant +hunt-index+ 1)

(defconstant +early-index+ 0)
(defconstant +late-index+ 1)

(defun lead-head-code (lead-head change)
  ;; assumes lead-head and change are both rows of the same stage
  (let ((c-bells (row-bells change)))
     (when (zerop (aref c-bells 0))
       (when-let ((n (which-plain-bob-lead-head lead-head)))
         (decf n)
         (when-let ((kind (lead-end-kind c-bells)))
           (let ((stage (length c-bells)))
            (if (evenp stage)
                (let* ((break (- (/ stage 2) 2))
                       (half (if (<= n break) +early-index+ +late-index+)))
                  (unless (zerop half)
                    (setf n (- stage n 3)))
                  (let ((excess (- n 2)))
                    (format nil "~C~@[~D~]"
                            (aref +even-stage-lead-head-codes+ kind half (clamp n 0 2))
                            (and (> excess 0) excess))))
                (and (evenp n)
                     (progn
                       (let ((break (- (floor stage 2) 1)))
                         (and (not (eql n break))
                              (let ((half (if (< n break) +early-index+ +late-index+)))
                                (unless (zerop half)
                                  (setf n (- stage n 3)))
                                (format nil "~C~@[~D~]"
                                        (aref +odd-stage-lead-head-codes+ kind half)
                                        (and (> n 0) (/ n 2)))))))))))))))

(defun lead-end-kind (bells)
  (let ((stage (length bells)))
    (if (eql (aref bells 1) 1)
        (and (if (evenp stage)
                 (%placesp bells 0 1)
                 (%placesp bells 0 1 (- stage 1)))
             +seconds-index+)
        (and (if (evenp stage)
                 (%placesp bells 0 (- stage 1))
                 (%placesp bells 0))
             +hunt-index+))))

(defun %update-classification (method traits)
  (cond ((null (%get-changes method traits)))
        ((method-contains-jump-changes method)
         (setf (method-traits-classification traits) '(:jump)))
        (t (iter (with primary := '())
                 (with secondary := '())
                 (for b :in (%get-hunt-bells method traits))
                 (for info := (%get-hunt-bell-info method traits b))
                 (if-first-time (push info primary)
                                (let ((diff (labels ((kind-little (x)
                                                       (+ (hunt-path-info-kind x)
                                                          (if (hunt-path-info-little-p x) 1 0))))
                                              (- (kind-little info)
                                                 (kind-little (first primary))))))
                                  (cond ((zerop diff)
                                         (push info primary))
                                        ((< diff 0)
                                         (nconcf secondary primary)
                                         (setf primary (list info)))
                                        (t (push info secondary)))))
                 (finally
                  (when-let ((info (first primary)))
                    (when (and (null secondary)
                               (null (rest primary))
                               (zerop (hunt-path-info-bell info))
                               (eql (hunt-path-info-apex info) 0))
                      (setf (method-traits-lead-head-code traits)
                            (lead-head-code (method-traits-lead-head traits)
                                            (first (last (method-traits-changes traits)))))))
                  (setf (method-traits-primary-hunt-bells traits)
                        (sort (mapcar #'hunt-path-info-bell primary) #'<))
                  (setf (method-traits-secondary-hunt-bells traits)
                        (sort (mapcar #'hunt-path-info-bell secondary) #'<))
                  (let ((classification (and primary
                                             (hunt-path-info-little-p (first primary))
                                             '(:little))))
                    (when (> (length (%get-working-bells method traits)) 1)
                      (push :differential classification))
                    (setf (method-traits-classification traits)
                          (cons (if (null primary)
                                    :principle
                                    (let ((k (hunt-path-info-kind (first primary))))
                                      (cond ((eql k +plain+)
                                             (%plain-classification method traits primary secondary))
                                            ((eql k +treble-dodging+)
                                             (%treble-dodging-classification method traits primary))
                                            (t (svref +hunt-path-keywords+ k)))))
                                classification))))))))

(defun %plain-classification (method traits primary-hunts secondary-hunts)
  ;; Note that on entry method is known not to contain jump changes, primary-hunts is
  ;; known to be non-null, and to be all plain (though possibily all little as well).
  (block test-slow-course
    (when (and (null (rest primary-hunts)) secondary-hunts)
      (let* ((hunt (first primary-hunts)) (apex (hunt-path-info-apex hunt)))
        (labels ((bell-at-location (index pos)
                   (aref (row-bells (nth index (%get-plain-lead method traits))) pos)))
          (unless (eql (bell-at-location apex 0) (hunt-path-info-bell hunt))
            (incf apex (/ (method-lead-length method) 2))
            (unless (eql (bell-at-location apex 0) (hunt-path-info-bell hunt))
              (return-from test-slow-course)))
          (let ((change-index (- apex 1)))
            (when (< change-index 0)
              (incf change-index (method-lead-length method)))
            (unless (eql (aref (row-bells (nth (mod (- apex 1) (method-lead-length method))
                                               (%get-changes method traits)))
                               1)
                         1)
              ;; second place isn't being made as the primary hunt is leading so not
              ;; slow course
              (return-from test-slow-course))
            (iter (with bell := (bell-at-location apex 1))
                  (for sh :in secondary-hunts)
                  (when (eql (hunt-path-info-bell sh) bell)
                    (return-from %plain-classification :slow-course))))))))
  (labels ((test-bob (b)
             (iter (with prev
                         := (aref (row-bells (first (last (%get-changes method traits))))
                                  b))   ; an involution since no jump changes
                   (with p := b)
                   (with diff := (- p prev))
                   (with new-diff)
                   (if-first-time nil (until (eql p b)))
                   (iter (for c :in (%get-changes method traits))
                         (setf prev p)
                         (setf p (aref (row-bells c) p))
                         (setf new-diff (- p prev))
                         (unless (or (zerop diff) (zerop new-diff) (eql diff new-diff))
                           (return-from %plain-classification :bob))
                         (setf diff new-diff)))))
    (iter (for (wb) :in (%get-working-bells method traits))
          (test-bob wb))
    (iter (for sh :in secondary-hunts)
          (test-bob (hunt-path-info-bell sh)))
    :place))

(defun %treble-dodging-classification (method traits primary-hunts)
  (iter (with all := t)
        (with none := t)
        (with len := (method-lead-length method))
        (with half := (- (/ len 2) 1))
        (with stage := (method-stage method))
        (for h :in primary-hunts)
        (for hl := (mod (+ (hunt-path-info-apex h) half) len))
        (for sec := (hunt-path-info-section-length h))
        (for i := (mod (+ (hunt-path-info-apex h) sec -1) len))
        (for p := (nthcdr i (%get-changes method traits)))
        (iter (repeat (- (/ len sec) 1))
              (unless (eql i hl)
                (let ((v (row-bells (first p))))
                  (if (iter (for j :from 1 :below (- stage 1))
                            (thereis (eql (aref v j) j)))
                      (setf none nil)
                      (setf all nil))))
              (incf i sec)
              (cond ((>= i len)
                     (setf i (mod i len))
                     (setf p (nthcdr i (%get-changes method traits))))
                    (t (setf p (nthcdr sec p)))))
        (finally (return (cond (none :treble-bob)
                               (all :surprise)
                               (t :delight))))))

(define-constant +no-explicit-class-names+
    '("Grandsire" "Double Grandsire" "Reverse Grandsire" "Little Grandsire"
      "Union" "Double Union" "Reverse Union")
  :test #'equal)

(defun set-method-classified-name (method name &optional (error-if-named t))
  "If @var{method} has its stage and place notation set, sets its @code{method-name} to be
@var{name} suitably augmented with its class and other modifiers, mostly from the CCCBR
taxonomy (@pxref{method-classification}). That is, @var{name} will be what the CCCBR calls
its ``name'', and its @code{method-name} will be set so that its @code{method-title} will
usually be as the CCCBR dictates. @var{name} may not be null, but may be an empty string.
If @var{method}'s stage or place notation is not set @code{set-method-classified-name}
does nothing. Returns @var{method}.

Both because of ambiguities in the CCCBR Decsisions, for example around complex
combinations of hunt bell types, and  that Roan supports jump changes, there are
some methods that may not be classified exactly as the CCCBR would dictate. Since the
CCCBR doesn't recognize ringing incorporating jump changes there are no standards for the
name of methods containing them, and there have been a variety of styles of names applied
by those bands that do enjoy them: @code{set-method-classified-name} always just appends
``jump'' to @var{name}.
methods containing jump changes

If the generalized boolean @var{error-if-named} is true, the default, an error will be
signaled if @var{method} already has its name set and that name is not @code{string-equal}
to the one @code{set-method-classified-name} will set it to. If @var{error-if-named} is
false any existing name will be superseded.

Signals a @code{type-error} if @var{method} is not a @code{method} or if @var{name} is not
a string. Signals a @code{parse-error} if @var{method}'s place notation cannot be parsed
at @var{method}'s stage. Signals an error if @var{error-if-named} is true, @var{method}
already has a name and @code{set-method-classified-name} would change that name.
@example
@group
 (let ((meth (method :stage 8 :place-notation \"x34x45x34,2\")))
   (set-method-classified-name meth \"Trafalgar\")
   (method-name meth))
     @result{} \"Trafalgar Differential Little Alliance\"
 (method-title
   (set-method-classified-name
     (method :stage 12 :place-notation \"x1x4,2\")
     \"\"))
        @result{} \"Little Bob Maximus\"
 (method-title
   (set-method-classified-name
     (method :stage 9 :place-notation \"147.(13)(46)(79)\")
     \"Roller Coaster\"))
        @result{} \"Roller Coaster Jump Caters\"
@end group
@end example"
  (check-type* name string)
  (when-let ((classification (method-classification method)))
    (let ((result (string-left-trim
                   " "
                   (format nil "~A~:[~; Differential~]~:[~; Little~]~@[ ~A~]"
                           name
                           (member :differential classification)
                           (member :little classification)
                           (let ((cls (first classification)))
                             (and (not (eq cls :principle))
                                  (not (and (eq cls :bob)
                                            (member name +no-explicit-class-names+
                                                    :test #'equalp)))
                                  (nsubstitute #\Space #\- (string-capitalize cls))))))))
      (if (and error-if-named
               (method-name method)
               (not (string-equal (method-name method) result)))
          (error "~S is already named, and will not be renamed to ~S." method result)
          (setf (method-name method) result))))
  method)

(define-method-trait lead-head-code (%update-classification classification)
  "Returns the lead head code for @var{method} if its stage and place notation are set and
it has Plain Bob lead ends, and otherwise returns @code{nil}. Considers neither twin hunt
methods, such as Grandsire, nor any method below minimus, as having Plain Bob lead ends,
and Rounds is not considered a Plain Bob lead head. When not @code{nil} the result is a
string containing a lower case letter, possibly followed by a digit.

The CCCBR's various collections of methods have, for several decades, use succinct codes
to denote the combination of a Plain Bob lead head and the change that leads to
it (@url{http://methods.org.uk/online/notes.htm}). While officially just a convention for
these collections it has become a de facto standard. Unfortunately these codes are not
precisely defined, and there are ambiguities for some uncommon hunt paths. Consequently
this function only returns a non-@code{nil} result for single hunt, non-hybrid methods,
including little methods, that do not contain any jump chanages. Thus there are cases
where a code is used in a CCCBR collection, but not returned by
@code{method-lead-head-code}. It is also worth noting that for odd stages there are
Plain Bob lead heads for which the CCCBR does not define any code; when called on such a
method @code{method-lead-head-code} also returns @code{nil}.

Signals a @code{type-error} if @var{method} is not a @code{method}, and a @code{parse-error}
if @var{method}'s place notation cannot be interpreted at its stage.
@example
@group
 (method-lead-head-code
   (lookup-method \"Ashtead Surprise\" 8))
     @result{} \"d\"
 (method-lead-head-code
   (lookup-method \"Zanussi Surprise\" 12))
     @result{} \"j2\"
 (method-lead-head-code
     (lookup-method \"Twerton Little Bob\" 9))
       @result{} \"q1\"
 (method-lead-head-code
   (lookup-method \"Double Glasgow Surprise\" 8))
     @result{} nil
@end group
@end example")


;;; Further method properties that are not currently cached as traits

(define-condition no-place-notation-error (error)
  ((method :reader no-place-notation-error-method :initarg :method))
  (:documentation "Signaled in circumstances when the changes constituting a method are
needed but are not available because the method's place notation or stage is empty.
Contains one potentially useful slot accessbile with
@code{no-place-notation-error-method}. Note, however, that many functions that make use
of a method's place notation and stage will return @code{nil} rather than signaling this
error if either is not present.")
  (:report (lambda (condition stream)
             (format stream "The method ~A does not have its place notation or stage set."
                     (no-place-notation-error-method condition)))))

(define-thread-local *rows-distinct-p-hash-table* nil)

(defun rows-distinct-p (rows)
  "TODO Currently not exported. Consider exporting once provers have been tidied up
sufficiently to add it.
Returns true if all the elements of @var{rows} is a @code{row}, and all are distinct. An
empty sequence is considered true. Note that if @var{rows} contains two @code{row}s of
different stages that, if the smaller were extended to the stage of the larger, would be
equal, they are still considered distinct by this function. Signals a @code{type-error} if
@var{rows} is not a sequence, or if it detects that any of its elements is not a
@code{row}; however, if falseness is discovered, no elements after the first duplicate
will be examine, and no error will be signaled if subsequent elements are not
@code{row}s."
  (let ((table (if *rows-distinct-p-hash-table*
                   (clrhash *rows-distinct-p-hash-table*)
                   (setf *rows-distinct-p-hash-table* (make-hash-table :test #'equalp)))))
    (etypecase rows
      (list (dolist (r rows t)
              (when (shiftf (gethash (bells-vector r) table) t)
                (return nil))))
      (vector (iter (for r :in-vector rows)
                    (when (shiftf (gethash (bells-vector r) table) t)
                      (return nil)))))))

(defun method-true-plain-course-p (method &optional (error-if-no-place-notation t))
  "If @var{method} has a non-nil stage and place notation set, returns true if
@var{method}'s plain course is true and @code{nil} otherwise. If @var{method} does not
have a non-nil stage or place notation a @code{no-place-notation-error} is signaled if the
generalized boolean @var{error-if-no-place-notation} is true, and otherwise @code{nil} is
returned; if @var{error-if-no-place-notation} is not supplied it defaults to true. Signals
a @code{type-error} if @var{method} is not a @code{method}. Signals a @code{parse-error}
if the place notation string cannot be properly parsed as place notation at @var{method}'s
stage.
@example
@group
 (method-true-plain-course-p
   (method :title \"Little Bob Minor\"
           :place-notation \"x1x4,2\"))
     @result{} t
 (method-true-plain-course-p
   (method :title \"Unnamed Little Treble Place Minor\"
           :place-notation \"x5x4x2,2\"))
     @result{} nil
@end group
@end example"
  (if-let ((course (method-plain-course method)))
    (rows-distinct-p course)
    (when error-if-no-place-notation
      (error 'no-place-notation-error :method method))))

(defun compare-rows (row1 row2)
  ;; row1 and row2 are assumed to be of the same stage
  (iter (for b1 :in-vector (row-bells row1))
        (for b2 :in-vector (row-bells row2))
        (cond ((< b1 b2) (return -1))
              ((> b1 b2) (return 1)))))

(defun row-list-less-p (list1 list2)
  ;; list1 and list2 are assumed to be equal length lists of rows, all of the same stage
  (iter (for r1 :in list1)
        (for r2 :in list2)
        (case (compare-rows r1 r2)
          (-1 (return t))
          (1 (return nil)))))

(defun canonical-rotation (changes)
  ;; changes is assumed to be a list of rows, all of the same stage
  ;; TODO there's probably a better algorithm possible
  (let ((min (iter (with result := (first changes))
                   (for c :in (rest changes))
                   (when (eql (compare-rows c result) -1)
                     (setf result c))
                   (finally (return result)))))
    (iter (with result)
          (initially (when (equalp (first changes) min)
                       (setf result changes)))
          (for r :on (rest changes))
          (for i :from 1)
          (when (equalp (first r) min)
            (let ((candidate (append r (subseq changes 0 i))))
              (when (or (null result) (row-list-less-p candidate result))
                (setf result candidate))))
          (finally (return result)))))

(define-constant +huffman-table+
    (iter (with result := (make-array 128 :initial-element nil))
          ;; Note that it is impossible in well formed place notation for a cross or dot
          ;; to follow an open paren, or for a dot to follow a comma or a cross.
          (for (codes . chars)
               :in '((("1111100100000") #\()
                     (("1001" "111110010011110" "111110010011111") #\))
                     (("0110" "111110010010010") #\,)
                     (("00111010" "1010000" "1111100101") #\0)
                     (("00110" "101110" "101001") #\1)
                     (("1101" "01011" "101101") #\2)
                     (("000" "11100" "01001") #\3)
                     (("10101" "1100" "11101") #\4)
                     (("1000" "0010" "11110") #\5)
                     (("101111" "0111" "01000") #\6)
                     (("111111" "1111101" "001111") #\7)
                     (("101100" "01010" "11111000") #\8)
                     (("0011100" "101000100" "00111011") #\9)
                     (("111110010011101" "1111100100101101" "111110010010111") #\A #\a)
                     (("1111100100001" "1111100100011" "111110010010000100") #\B #\b)
                     (("111110010010001" "111110010010000000011001" "111110010011100") #\C #\c)
                     (("1111100100010" "11111001001010" "1111100100100000000100") #\D #\d)
                     (("101000111" "1111100100110" "111110011") #\E #\e)
                     (("11111001001000001" "11111001001000000000111" "1111100100100000000111011") #\F #\f)
                     (("1111100100100001011" "11111001001011000" "111110010010000000011011") #\G #\g)
                     (("111110010010000001" "11111001001000000000101" "111110010010000000010100") #\H #\h)
                     (("1111100100100001010" "11111001001011001" "1111100100100000000001") #\J #\j)
                     (("1111100100100000000111010" "1111100100100000000101010" "1111100100100000000101011") #\K #\k)
                     (("11111001001000000000100" "1111100100100000001" "11111001001000000001111") #\L #\l)
                     (("111110010010000000011010" "111110010010000000011100" "11111001001000000000110") #\M #\m)
                     (("11111001001000000000000" "11111001001000000000001" "111110010010000000011000") #\N #\n)
                     (("101000101" "101000110" "111110010010011") #\T #\t)
                     (("11111001001000000001011" "11111001001000011") #\x #\X #\-)))
          (for bits-info := (mapcar #'(lambda (string)
                                        (cons (parse-integer string :radix 2)
                                              (length string)))
                                    codes))
          (iter (for c :in chars)
                (setf (aref result (char-code c)) bits-info))
          (finally (return result)))
  :test #'equalp)

(defun encode-place-notation (notation stage)
  ;; Huffman encodes the stage + place notation, based on the frequencies in the methods
  ;; database as of June 2017. Only place notation characters are allowed, including
  ;; parens and commas (but no brackets). Note that digraphs ending in dot or cross get
  ;; their own encodings.
  (let ((result (make-array (length notation)
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0))
        (buffer 0)
        (index 0))
    (labels ((encode-char (c access-fn)
               (iter (with (code . len) := (funcall access-fn (aref +huffman-table+ (char-code c))))
                     (initially (setf (ldb (byte len index) buffer) code)
                                (incf index len))
                     (while (>= index 8))
                     (vector-push-extend (ldb (byte 8 0) buffer) result)
                     (setf buffer (ash buffer -8))
                     (decf index 8))))
      (iter (with prev := (bell-name (- stage 1)))
            (for c :in-string notation)
            (cond ((not prev)
                   (setf prev c))
                  ((eql c #\x)
                   (encode-char prev #'second)
                   (setf prev nil))
                  ((eql c #\.)
                   (encode-char prev #'third)
                   (setf prev nil))
                  (t (encode-char prev #'first)
                     (setf prev c)))
            (finally (when prev
                       (encode-char prev #'first))))
      ;; We follow the place notation by an unbalanced close paren as an end token to
      ;; ensure unused bits in the last byte don't cause false positives when comparing
      ;; two encodings.
      (encode-char #\) #'first)
      (unless (zerop index)
        (vector-push-extend buffer result))
      (string-right-trim "=" (with-output-to-string (s)
                               (s-base64:encode-base64-bytes result s nil))))))

;; (with-methods-database (conn)
;;   (iter (with table := (make-hash-table))
;;         (initially (iter (for c :in-string (format nil "~Ax,()" +bell-names+))
;;                          (setf (gethash c table) (list 0 0 0))))
;;         (for (stage notation)
;;              :in-sqlite-query "select stage, notation from methods"
;;              :on-database conn)
;;         (when (> (length notation) 0)
;;           (iter (with prev := nil)
;;                 (for c :in-string (format nil "~C~A)"
;;                                           (bell-name (- stage 1))
;;                                           (canonicalize-place-notation notation :stage stage)))
;;                 (cond ((and (eql c #\x) prev)
;;                        (incf (second (gethash prev table)))
;;                        (setf prev nil))
;;                       ((and (eql c #\.) prev)
;;                        (incf (third (gethash prev table)))
;;                        (setf prev nil))
;;                       (prev
;;                        (incf (first (gethash prev table)))
;;                        (setf prev c))
;;                       (t (setf prev c)))
;;                 (finally (when prev
;;                            (incf (first (gethash prev table)))))))
;;         (finally (iter (for (c . nums) :in (sort (hash-table-alist table) #'char< :key #'car))
;;                        (format t "~&~C   ~{~:D  ~}" c nums)))))
;;
;;         (   34  0  0
;;         )   20,567  16  17
;;         ,   19,413  13  0
;;         0   1,057  2,411  439
;;         1   8,378  5,901  5,393
;;         2   25,943  9,695  5,534
;;         3   29,206  13,333  9,301
;;         4   10,852  25,081  13,377
;;         5   20,166  15,194  13,464
;;         6   6,207  19,901  8,890
;;         7   7,487  3,374  4,695
;;         8   5,421  9,335  1,623
;;         9   1,878  566  1,076
;;         A   15  8  14
;;         B   36  47  1
;;         C   12  0  15
;;         D   46  27  0
;;         E   730  62  916
;;         F   3  0  0
;;         G   1  3  0
;;         H   1  0  0
;;         J   1  3  0
;;         K   0  0  0
;;         L   0  1  0
;;         M   0  0  0
;;         N   0  0  0
;;         T   585  637  13
;;         x   0  3  0

(defun method-rotations-p (method-1 method-2)
  "Returns true if and only if the changes constituting a lead of @var{method-1} are the
same as those constituting a lead of @var{method-2}, possibly rotated. If the changes are
the same even without rotation that is considered a trivial rotation, and also returns
true. Note that if @var{method-1} and @var{method-2} are of different stages the result
will always be false.

Signals a @code{no-place-notation-error} if either argument does not have its stage or
place notation set. Signals a @code{type-error} if either argument is not a @code{method}.
Signals a @code{parse-error} if the place notation of either argument cannot be parsed as
place notation at its stage.
@example
@group
 (method-rotations-p
   (method :stage 5 :place-notation \"3,1.5.1.5.1\")
   (method :stage 5 :place-notation \"5.1.5.1,1.3\"))
     @result{} t
 (method-rotations-p
   (method :stage 5 :place-notation \"3,1.5.1.5.1\")
   (method :stage 5 :place-notation \"3,1.5.1.5.1\"))
     @result{} t
 (method-rotations-p
   (method :stage 5 :place-notation \"3,1.5.1.5.1\")
   (method :stage 5 :place-notation \"3,1.3.1.5.1\")
     @result{} nil
 (method-rotations-p
   (method :stage 5 :place-notation \"3,1.5.1.5.1\")
   (method :stage 7 :place-notation \"5.1.5.1,1.3\"))
     @result{} nil)
@end group
@end example"
  (labels ((fail (method)
             (error 'no-place-notation-error :method method)))
    (let ((c1 (method-changes method-1)) (c2 (method-changes method-2)))
      ;; we grab the changes early to ensure an error is signaled for bad input
    (and (eql (or (method-stage method-1) (fail method-1))
              (or (method-stage method-2) (fail method-2)))
         (equalp (canonical-rotation (or c1 (fail method-1)))
                 (canonical-rotation (or c2 (fail method-2))))))))

(defun method-canonical-rotation-key (method)
  "If @var{method} has its stage and place notation set returns a string uniquely
identifying, using @code{equal}, the changes of a lead of this method, invariant under
rotation. That is, if two methods are rotations of one another their
@code{method-canonical-rotation-key}s will always be @code{equal}. The string is, other
than being a string, essentially an opaque type and should generally not be displayed to
an end user or otherwise have its structure depended upon. Case is significant;
@code{equalp} should not be used to compare these keys. While, within one version of Roan,
this key can be counted on to be the same in different sessions and on different machines,
it may change between versions of Roan. If @var{method} does not have both its stage and
place notation set @code{method-canonical-rotation-key} returns @code{nil}.

Signals a @code{type-error} of @var{method} is not a @code{method}. Signals a
@code{parse-error} if @var{method}'s place notation cannot be properly parsed at its
stage.
@example
@group
 (method-canonical-rotation-key
   (lookup-method \"Cambridge Surprise\" 8))
     @result{} \"bAvzluTjWO5P\"
 (method-canonical-rotation-key
   (method :stage 8 :place-notation \"5x6x7,x4x36x25x4x3x2\"))
     @result{} \"bAvzluTjWO5P\"
 (method-canonical-rotation-key
   (method :stage 8 :place-notation \"x1x4,2\"))
     @result{} \"bEvy3Zo\"
 (method-canonical-rotation-key
   (method :stage 10 :place-notation \"x1x4,2\"))
     @result{} \"Oi3Jd2sC\"

 (method-canonical-rotation-key (method) @result{} nil
@end group
@end example"
  (when-let ((changes (method-changes method)))
    (encode-place-notation (canonicalize-place-notation (canonical-rotation changes))
                           (method-stage method))))


;;; Falseness

(defstruct (fch-group (:copier nil))
  "===summary===
Most methods that have been rung and named at stages major and above have been rung at
even stages, with Plain Bob lead ends and lead heads, without jump changes, and with the
usual palindromic symmetry. For major, and at higher stages if the tenors are kept
together, the false course heads of such methods are traditionally partitioned into named
sets all of whose elements must occur together in such methods. These are traditionally
called ``false course head groups'' (FCHs), although they are not what mathemeticians
usually mean by the word ``group''. Further information is available from a variety of
sources, including Appendix B of the
@url{http://www.methods.org.uk/method-collections/xml-zip-files/method+xml+1.0.pdf, CCCBR
Methods Committee's XML format documentation}.

Roan provides a collection of @code{fch-group} objects that represent these FCH groups.
Each is intended to be an singleton object, and under normal circumstances new instances
should not be created. They can thus be compared using @code{eq}, if desired. The
@code{fch-group}s for major are distinct from those for higher stages, though their
contents are closely related.

An @code{fch-group} can be retrieved using the @code{fch-group} function. The first
argument to this function can be either a @code{row} or a string. If a @code{row}
the @code{fch-group} that contains that row is returned. If a string the @code{fch-group}
with that name is returned. In this latter case two further, optional arguments can be
used to state that the group for higher stages is desired, and whether the one with just
in course or just out of course false course heads is desired; for major all the
@code{fch-group}s contain both in and out of course elements.

The @code{fch-group-name}, @code{fch-group-parity} and @code{fch-group-elements} functions
can be used to retrieve the name, parity and elements of a @code{fch-group}. The
@code{method-falseness} function calculates the false course heads of non-differential,
treble dominated methods at even stages major and above, and for those with the usual
palindromic symmetry and Plain Bob lead heads and lead ends, also returns the relevant
@code{fch-group}s. The @code{fch-groups-string} function can be used to format a
collection of @code{fch-group} names in a traditional, compact manner.

It is possible to extend the usual FCH groups to methods with non-Plain Bob lead heads.
However, Roan currently provides no support for this.
===endsummary===
Describes a false course head group, including its name, parity if for even stages above
major, and a list of the course heads it contains. The parity is @code{nil} for major
@code{fch-group}s, and one of the keywords @code{:in-course} or @code{:out-of-course} for
higher stages. The elements of a major @code{fch-group} are major @code{row}s while those
for a higher stage @code{fch-group} are royal @code{row}s."
  (index 0 :type integer :read-only t)
  (name "" :type string :read-only t)
  (parity nil :type symbol :read-only t)
  (elements () :type list :read-only t))

(defmethod print-object ((fch-group fch-group) stream)
  (cond (*print-readably*
         (format stream "(FCH-GROUP ~S~:[~*~; T ~:[NIL~;T~]~])"
                 (fch-group-name fch-group)
                 (fch-group-parity fch-group)
                 (eq (fch-group-parity fch-group) :out-of-course)))
        (*print-escape*
         (print-unreadable-object (fch-group stream :type t :identity t)
           (format stream "~A~@[ ~(~A~)~]"
                   (fch-group-name fch-group)
                   (fch-group-parity fch-group))))
        (t (princ (fch-group-name fch-group) stream))))

(defparameter *fch-groups-by-name* (make-hash-table :test #'equal :size 41))

(defparameter *fch-groups-by-course-head* (make-hash-table :test #'equalp :size 841))

(defparameter *last-fch-group-index* 0)

(defun fch-lessp (r1 r2)
  ;; r1 and r2 should be rows, at an even stage major or above, with the 8 and any higher
  ;; bells home
  (let ((bv1 (row-bells r1)) (bv2 (row-bells r2)))
    (declare (type %bells-vector bv1 bv2))
    (labels ((pos-compare (b)
               (let ((p1 (position b bv1)) (p2 (position b bv2)))
                 (cond ((> p1 p2) (return-from fch-lessp t))
                       ((< p1 p2) (return-from fch-lessp nil))))))
      (pos-compare 6)
      (if (%in-course-p bv1)
          (when (not (%in-course-p bv2))
            (return-from fch-lessp t))
          (when (%in-course-p bv2)
            (return-from fch-lessp nil)))
      (iter (for i :from 5 downto 2)
            (pos-compare i)))))

(defun create-fch-group (name parity &rest elements)
  (let ((group (make-fch-group :name name
                               :parity parity
                               :index (incf *last-fch-group-index*) ; for sorting
                               :elements (sort elements #'fch-lessp))))
    (push group (gethash name *fch-groups-by-name*))
    (dolist (fch (fch-group-elements group))
      (setf (gethash fch *fch-groups-by-course-head*) group))))

(defun fch-group (item &optional (higher-stage nil higher-stage-supplied) out-of-course)
  "Returns an @code{fch-group} described by the provided arguments. The @var{item} can be
either a @code{row} or a string designator.

If @var{item} is a @code{row} the @code{fch-group} that contains that row among its
elements is returned. If it is not at an even stage, major or above, or if it is at an
even stage royal or above but with any of the bells conventially called the seven (and
represented in Roan by the integer @code{6}) or higher out of their rounds positions,
@code{nil} is returned. If @var{item} is a @code{row} at an even stage maximus or above,
with the back bells in their home positions, it is treated as if it were the equivalent
royal @code{row}. When @var{item} is a @code{row} neither @var{higher-stage} nor
@var{out-of-course} may be supplied.

If @var{item} is a string designator the @code{fch-group} that has that name is returned.
If the generalized boolean @var{higher-stage} is true a higher stage @code{fch-group} is
returned and others a major one. In the case of higher stage groups if the generalized
boolean @var{out-of-course} is true the group with the given name containing only out of
course elements is returned, and otherwise the one with only in course elements. Both
@var{higher-stage} and @var{out-of-course} default to @code{nil} if not supplied. If there
is no @code{fch-group} with name @var{item} and the given properties @code{nil} is
returned.

Signals a @code{type-error} if @var{item} is neither a @code{row} nor a string designator.
Signals an error if @var{item} is a @code{row} and @var{higher-stage} or
@var{out-of-course} is supplied.
@example
@group
 (let ((g (fch-group !2436578)))
   (list (fch-group-name g)
         (fch-group-parity g)
         (stage (first (fch-group-elements g)))))
    @result{} (\"B\" nil 8)
 (fch-group \"a1\" t nil) @result{} nil
 (fch-group-elements (fch-group \"a1\" t t)) @result{} (!1234657890)
@end group
@end example"
  (typecase item
    (row (when higher-stage-supplied
           (error "The second and third arguments must not be supplied when the first is a row."))
         (values (gethash (let ((stage (stage item)))
                            (cond ((eql stage 8)
                                   item)
                                  ((and (>= stage 10) (evenp stage))
                                   (alter-stage item 10))
                                  (t (return-from fch-group nil))))
                          *fch-groups-by-course-head*)))
    (string (find (and higher-stage (if out-of-course :out-of-course :in-course))
                  (gethash item *fch-groups-by-name*)
                  :key #'fch-group-parity))
    (symbol (fch-group (symbol-name item) higher-stage out-of-course))
    (character (fch-group (string item) higher-stage out-of-course))
    (otherwise (error 'type-error :datum item :expected-type '(or row string)))))

(define-condition mixed-stage-fch-groups-error (error) ()
  (:report "Major and higher-stage fch-groups cannot be combined.")
  (:documentation "Signaled if two or more @code{fch-group}s are used together in a
context that expects homogeneity but are not all major @code{fch-group}s or all
higher-stage @code{fch-group}s."))

(defun fch-groups-string (collection &rest more-collections)
  "Returns a string succinctly describing a set of @code{fch-group}s, in a conventional
order. The set of @code{fch-group}s is the union of all those contained in the arguments,
each of which should be a sequence or @code{hash-set}, all of whose elements are
@code{fch-group}s. The resulting string contains the names of the distinct
@code{fch-group}s. If there are no groups @code{nil}, rather than an empty string, is
returned.

For higher stages there are two sequences of group names in the string, separated by a
solidus (@samp{/}); those before the solidus are in course and those afterit out of
course. For example, @code{\"B/Da1\"} represents the higher course in course elements of
group B and out of course elements of groups D and a1.

The group names are presented in the conventional order. For major the groups containing
in course, tenors together elements appear first, in alphabetical order; followed by those
all of whose tenors together elements are out of course, in alphabetical order; finally
followed by those all of whose elements are tenors parted. For higher stages the capital
letter groups in each half of the string come first, in alphabetical order, followed by
those with lower case names. Note that a lower case name can never appear before the
solidus.

Signals a @code{type-error} if any of the arguments are not sequences or @code{hash-set}s,
or if any of their elements is not an @code{fch-group}. Signals a
@code{mixed-stage-fch-groups-error} if some of the elements are major and some are higher
stage @code{fch-group}s.
@example
@group
 (fch-groups-string (list (fch-group \"a\") (fch-group \"B\")))
   @result{} \"Ba\"
 (fch-groups-string #((fch-group \"D\" t t)
                      (fch-group \"a1\" t t))
                    (hash-set (fch-group \"B\" t)))
   @result{} \"B/Da1\"
 (fch-groups-string (list (fch-group \"T\" t nil)))
   @result{} \"T/\"
 (fch-groups-string (list (fch-group \"T\" t t)))
   @result{} \"/T\"
@end group
@end example"
  (let ((major nil) (higher nil) (table (make-hash-table)))
    ;; We could signal the mixed stage error immediately on detecting it, but it seems
    ;; better to confirm that all the collections are collections and that all the
    ;; elements thereof are fch-groups, as such type errors are even more likely to be
    ;; bugs worthy of immediate attention.
    (labels ((do-group (g)
               (check-type* g fch-group)
               (if (fch-group-parity g)
                   (setf higher t)
                   (setf major t))
               (setf (gethash g table) g))
             (do-collection (c)
               (typecase c
                 (sequence (map nil #'do-group c))
                 (hash-set (map-hash-set #'do-group c))
                 (otherwise (error 'type-error
                                   :datum c
                                   :expected-type '(or sequence hash-set))))))
      (do-collection collection)
      (dolist (c more-collections)
        (do-collection c)))
    (cond ((and major higher)
           (error 'mixed-stage-fch-groups-error))
          (major
           (format nil "~{~A~}" (sort (hash-table-keys table) #'< :key #'fch-group-index)))
          (higher
           (iter (for (g nil) :in-hashtable table)
                 (if (eq (fch-group-parity g) :in-course)
                     (collect g :into in-course)
                     (collect g :into out-of-course))
                 (finally (return (format nil "~{~A~}/~{~A~}"
                                          (sort in-course #'< :key #'fch-group-index)
                                          (sort out-of-course #'< :key #'fch-group-index)))))))))

(create-fch-group "A" nil !2345678 !3254768)
(create-fch-group "B" nil
                  !2436578 !4263758 !2543678 !5234768 !3245678 !2354768 !2365478
                  !3256748 !4325678 !3452768 !2347658 !3274568 !3527468 !5372648)
(create-fch-group "C" nil
                  !2563478 !5236748 !4236578 !2463758 !3426578 !4362758 !2537468
                  !5273648 !2367458 !3276548 !5327468 !3572648 !4327658 !3472568)
(create-fch-group "D" nil
                  !3254678 !2345768 !4625378 !6452738 !5342678 !3524768 !6345278
                  !3654728 !2435678 !4253768 !2745638 !7254368 !5734268 !7543628)
(create-fch-group "E" nil
                  !3246578 !2364758 !4326578 !3462758 !2536478 !5263748 !2463578
                  !4236758 !5237468 !2573648 !2357468 !3275648 !4367258 !3476528
                  !2567438 !5276348 !2763458 !7236548 !6327458 !3672548 !5327648
                  !3572468 !2647358 !6274538 !3756248 !7365428 !3427568 !4372658)
(create-fch-group "F" nil
                  !4523678 !5432768 !3265478 !2356748 !2634578 !6243758 !2456378
                  !4265738 !3725468 !7352648 !3547268 !5374628 !3247658 !2374568)
(create-fch-group "G" nil
                  !5642378 !6524738 !6354278 !3645728 !2765348 !7256438 !2746538
                  !7264358 !3657428 !6375248 !4753268 !7435628 !6752438 !7625348
                  !6734258 !7643528 !2754638 !7245368 !6257348 !2675438 !3764528
                  !7346258 !5764238 !7546328 !6457238 !4675328 !6347528 !3674258)
(create-fch-group "H" nil
                  !5346278 !3564728 !6342578 !3624758 !5624378 !6542738 !6254378
                  !2645738 !4652378 !6425738 !3654278 !6345728 !2745368 !7254638
                  !5743268 !7534628 !4735268 !7453628 !2735648 !7253468 !3754628
                  !7345268 !4257368 !2475638 !5762438 !7526348 !6437258 !4673528)
(create-fch-group "I" nil
                  !5463278 !4536728 !6532478 !5623748 !3762548 !7326458 !6237548
                  !2673458 !6724358 !7642538 !6753428 !7635248 !4657238 !6475328
                  !5746328 !7564238 !5637428 !6573248 !4726538 !7462358 !4732658
                  !7423568 !2567348 !5276438 !4367528 !3476258 !5427638 !4572368)
(create-fch-group "K" nil
                  !6543278 !5634728 !5362478 !3526748 !4635278 !6453728 !5234678
                  !2543768 !6425378 !4652738 !3452678 !4325768 !6432578 !4623758
                  !5436278 !4563728 !5724368 !7542638 !5743628 !7534268 !4725638
                  !7452368 !5732648 !7523468 !2437658 !4273568 !4527368 !5472638)
(create-fch-group "L" nil
                  !2654378 !6245738 !4256378 !2465738 !3624578 !6342758 !3267458
                  !2376548 !2735468 !7253648 !3745268 !7354628 !5347268 !3574628)
(create-fch-group "M" nil
                  !2356478 !3265748 !2364578 !3246758 !2546378 !5264738 !2643578
                  !6234758 !2756348 !7265438 !3647258 !6374528 !3752468 !7325648
                  !6527438 !5672348 !4763258 !7436528 !3765428 !7356248 !6247358
                  !2674538 !2357648 !3275468 !3457268 !4375628 !3247568 !2374658)
(create-fch-group "N" nil
                  !6243578 !2634758 !5326478 !3562748 !4362578 !3426758 !3546278
                  !5364728 !3456278 !4365728 !6523478 !5632748 !4563278 !5436728
                  !5263478 !2536748 !3562478 !5326748 !6234578 !2643758 !4632578
                  !6423758 !5426378 !4562738 !4237568 !2473658 !3725648 !7352468
                  !4637258 !6473528 !5726348 !7562438 !5723468 !7532648 !4723658
                  !7432568 !2547368 !5274638 !3427658 !4372568 !2537648 !5273468
                  !2457368 !4275638 !4527638 !5472368 !5763428 !7536248 !6427358
                  !4672538 !4237658 !2473568 !3752648 !7325468 !4537268 !5473628)
(create-fch-group "O" nil
                  !5264378 !2546738 !3652478 !6325748 !6524378 !5642738 !4653278
                  !6435728 !6352478 !3625748 !5364278 !3546728 !5643278 !6534728
                  !6542378 !5624738 !2734658 !7243568 !4752368 !7425638 !5746238
                  !7564328 !4657328 !6475238 !6547328 !5674238 !4765328 !7456238
                  !6437528 !4673258 !5762348 !7526438 !3457628 !4375268 !4753628
                  !7435268 !2457638 !4275368 !4735628 !7453268 !6527348 !5672438
                  !4763528 !7436258 !5742368 !7524638 !6752348 !7625438 !6734528
                  !7643258 !2743568 !7234658 !6742538 !7624358 !6735248 !7653428)
(create-fch-group "P" nil
                  !5432678 !4523768 !6435278 !4653728 !5634278 !6543728 !6452378
                  !4625738 !5742638 !7524368 !4725368 !7452638 !5734628 !7543268)
(create-fch-group "R" nil
                  !4562378 !5426738 !5623478 !6532748 !3564278 !5346728 !6253478
                  !2635748 !4537628 !5473268 !6754328 !7645238 !4257638 !2475368
                  !4762538 !7426358 !6537248 !5673428 !5726438 !7562348 !4637528
                  !6473258 !6745328 !7654238 !3724658 !7342568 !4732568 !7423658)
(create-fch-group "S" nil
                  !5236478 !2563748 !3462578 !4326758 !6423578 !4632758 !4536278
                  !5463728 !4567238 !5476328 !5237648 !2573468 !2467358 !4276538
                  !5367428 !3576248 !5427368 !4572638 !6723458 !7632548 !5723648
                  !7532468 !2637458 !6273548 !3726548 !7362458 !4327568 !3472658)
(create-fch-group "T" nil
                  !2453678 !4235768 !2534678 !5243768 !3645278 !6354728 !4352678
                  !3425768 !6245378 !2654738 !2635478 !6253748 !5324678 !3542768
                  !2465378 !4256738 !3256478 !2365748 !3264578 !2346758 !4623578
                  !6432758 !4526378 !5462738 !3724568 !7342658 !6743258 !7634528
                  !6725438 !7652348 !3627458 !6372548 !2736548 !7263458 !3745628
                  !7354268 !2347568 !3274658 !5732468 !7523648 !4267358 !2476538
                  !3567428 !5376248 !5437268 !4573628 !2754368 !7245638 !3257648
                  !2375468 !4765238 !7456328 !6547238 !5674328 !5347628 !3574268)
(create-fch-group "U" nil
                  !3425678 !4352768 !4235678 !2453768 !4365278 !3456728 !6325478
                  !3652748 !5243678 !2534768 !3542678 !5324768 !3526478 !5362748
                  !4263578 !2436758 !2743658 !7234568 !2547638 !5274368 !5736248
                  !7563428 !4627358 !6472538 !3527648 !5372468 !2437568 !4273658)
(create-fch-group "a" nil
                  !2346578 !3264758 !2354678 !3245768 !2645378 !6254738 !3754268
                  !7345628 !3257468 !2375648 !2765438 !7256348 !6347258 !3674528)
(create-fch-group "b" nil
                  !2653478 !6235748 !2564378 !5246738 !6537428 !5673248 !4762358
                  !7426538 !3267548 !2376458 !4756328 !7465238 !5647238 !6574328
                  !4357628 !3475268 !3742568 !7324658 !6735428 !7653248 !6742358
                  !7624538 !2367548 !3276458 !4736528 !7463258 !5627438 !6572348)
(create-fch-group "c" nil
                  !3524678 !5342768 !4253678 !2435768 !4265378 !2456738 !3625478
                  !6352748 !3547628 !5374268 !6745238 !7654328 !2734568 !7243658)
(create-fch-group "d" nil
                  !3465278 !4356728 !4532678 !5423768 !6235478 !2653748 !5423678
                  !4532768 !2637548 !6273458 !3726458 !7362548 !5736428 !7563248
                  !4627538 !6472358 !3742658 !7324568 !4726358 !7462538 !5637248
                  !6573428 !5247638 !2574368 !4267538 !2476358 !3567248 !5376428)
(create-fch-group "e" nil
                  !3642578 !6324758 !5246378 !2564738 !4356278 !3465728 !6324578
                  !3642758 !3765248 !7356428 !6247538 !2674358 !2753468 !7235648
                  !3657248 !6375428 !2746358 !7264538 !5247368 !2574638 !4736258
                  !7463528 !5627348 !6572438 !5267438 !2576348 !3467258 !4376528
                  !2467538 !4276358 !5367248 !3576428 !3746258 !7364528 !2657348
                  !6275438 !3627548 !6372458 !2736458 !7263548 !6427538 !4672358
                  !5763248 !7536428 !3756428 !7365248 !2647538 !6274358 !2753648
                  !7235468 !6237458 !2673548 !3762458 !7326548 !4357268 !3475628)
(create-fch-group "f" nil
                  !5462378 !4526738 !5632478 !6523748 !6534278 !5643728 !6453278
                  !4635728 !4567328 !5476238 !5437628 !4573268 !5764328 !7546238
                  !6457328 !4675238 !4752638 !7425368 !5724638 !7542368 !6732548
                  !7623458 !4723568 !7432658 !6743528 !7634258 !6725348 !7652438)
(create-fch-group "X" nil
                  !2764358 !7246538 !5647328 !6574238 !4756238 !7465328 !5267348
                  !2576438 !3467528 !4376258 !5467238 !4576328 !6357428 !3675248
                  !6732458 !7623548 !2657438 !6275348 !3746528 !7364258 !2763548
                  !7236458 !6327548 !3672458 !6724538 !7642358 !6753248 !7635428)
(create-fch-group "Y" nil
                  !2756438 !7265348 !3647528 !6374258 !6754238 !7645328 !6257438
                  !2675348 !3764258 !7346528 !2764538 !7246358 !6357248 !3675428)
(create-fch-group "Z" nil
                  !5467328 !4576238 !6723548 !7632458)

(create-fch-group "A" :in-course !1234567890)
(create-fch-group "B" :in-course !243657890)
(create-fch-group "C" :in-course !256347890)
(create-fch-group "D" :in-course !325467890 !462537890)
(create-fch-group "E" :in-course !324657890 !432657890)
(create-fch-group "F" :in-course !452367890 !326547890)
(create-fch-group "G" :in-course !564237890 !635427890)
(create-fch-group "H" :in-course !534627890 !634257890)
(create-fch-group "I" :in-course !546327890 !653247890)
(create-fch-group "K" :in-course !654327890 !536247890)
(create-fch-group "L1" :in-course !265437890)
(create-fch-group "L2" :in-course !425637890 !362457890)
(create-fch-group "M" :in-course !235647890 !236457890 !254637890 !264357890)
(create-fch-group "N" :in-course !345627890 !623457890 !463257890 !542637890)
(create-fch-group "O" :in-course !526437890 !365247890 !652437890 !465327890)
(create-fch-group "P1" :in-course !543267890 !643527890)
(create-fch-group "P2" :in-course !563427890 !645237890)
(create-fch-group "R" :in-course !456237890 !562347890 !356427890 !625347890)
(create-fch-group "S" :in-course !523647890 !346257890 !642357890 !453627890)
(create-fch-group "T" :in-course !245367890 !253467890 !364527890 !435267890 !624537890
                                 !263547890 !532467890 !246537890)
(create-fch-group "U1" :in-course !342567890 !423567890 !436527890 !632547890 !524367890
                                  !354267890)
(create-fch-group "U2" :in-course !352647890 !426357890)

(create-fch-group "B" :out-of-course !254367890 !324567890 !236547890 !432567890)
(create-fch-group "C" :out-of-course !423657890 !342657890)
(create-fch-group "D" :out-of-course !534267890 !634527890 !243567890)
(create-fch-group "E" :out-of-course !253647890 !246357890)
(create-fch-group "F" :out-of-course !263457890 !245637890)
(create-fch-group "H" :out-of-course !562437890 !625437890 !465237890 !365427890)
(create-fch-group "K1" :out-of-course !463527890 !523467890 !642537890 !345267890)
(create-fch-group "K2" :out-of-course !643257890 !543627890)
(create-fch-group "N1" :out-of-course !624357890 !532647890 !436257890 !354627890)
(create-fch-group "N2" :out-of-course !652347890 !456327890 !526347890 !356247890)
(create-fch-group "O" :out-of-course !635247890 !536427890 !564327890 !654237890)
(create-fch-group "T" :out-of-course !325647890 !326457890 !462357890 !452637890)
(create-fch-group "a1" :out-of-course !234657890)
(create-fch-group "a2" :out-of-course !235467890 !264537890)
(create-fch-group "b" :out-of-course !265347890 !256437890)
(create-fch-group "c" :out-of-course !352467890 !425367890 !426537890 !362547890)
(create-fch-group "d" :out-of-course !346527890 !453267890 !623547890 !542367890)
(create-fch-group "e" :out-of-course !364257890 !524637890 !435627890 !632457890)
(create-fch-group "f" :out-of-course !546237890 !563247890 !653427890 !645327890)

(define-condition inappropriate-method-error (error)
  ((details :reader inappropriate-method-error-details :initarg :details)
   (method :reader inappropriate-method-error-method :initarg :method))
  (:documentation "Signaled in circumstances when a method with certain properties is
required, but the method supplied does not have those properties. Contains two potentially
useful slots accessible with @code{inappropriate-method-error-details} and
@code{inappropriate-method-error-method}.")
  (:report (lambda (condition stream)
             (format stream (inappropriate-method-error-details condition)
                     (inappropriate-method-error-method condition)))))

(defun falseness-groups (method fchs)
  (and (evenp (method-lead-length method))
       (method-lead-head-code method)
       (funcall (if (method-contains-jump-changes method)
                    #'test-jump-palindrome
                    #'test-ordinary-palindrome)
                (method-changes method)
                (- (/ (method-lead-length method) 2) 1))
       (iter (with result := (make-hash-table))
             (for ch :in fchs)
             (unless (equalp ch !13254768)
               (setf (gethash (fch-group ch) result) t))
             (finally (return (sort (hash-table-keys result)
                                    #'<
                                    :key #'fch-group-index))))))

(defun sort-fch-set (set)
  (and set (sort (hash-set-elements set) #'fch-lessp)))

(define-modify-macro fch-list-f () sort-fch-set)

(defconstant +row-signature-shift+ (integer-length +maximum-stage+))

(defconstant +incidence-initial-hash-set-size+ 16)

(defconstant +summary-initial-hash-set-size+ 120)

(defun method-falseness (method)
  "Computes the most commonly considered kinds of internal falseness of the most common
methods: those at even stages major or higher with a single hunt bell, the treble, and all
the working bells forming one cycle, that is, not differential. Falseness is only
considered with the treble fixed, as whole leads, and, for stages royal and above, with
seventh (that is, the bells roan denotes by @code{6}) and above fixed. Returns three
values: a summary of the courses that are false; for methods that have Plain Bob lead ends
and lead heads and the usual palindromic symmetry, the false course head groups that are
present; and a description of the incidence of falseness.

The first value is a list of course heads, @code{row}s that have the treble and tenors
fixed, such that the plain course is false against the courses starting with any of these
course heads. Rounds is included only if the falseness occurs between rows at two
different positions within the course. Course heads for major have just the tenor (that
is, the bell represented in Roan by the integer @code{7}) fixed, while course heads for
higher stages have all of the seventh and above (that is, bells represented in Roan by the
integers @code{6} and larger) fixed in their rounds positions.

If @var{method} has Plain Bob lead ends and lead heads, and the usual palindromic
symmetry, the second value returned is a list of @code{fch-group} objects, and is
otherwise the second value is @code{nil}. Note also that for methods that are completely
clean in the context used by this function, for example plain royal methods, an empty list
also will be returned. These two cases can be disambiguated by examining the first value
returned.

There is some ambiguity in the interpretation of ``A'' falseness. In Roan a method is
only said to have ``A'' falseness if its plain course is false. That is, the trivial
falseness implied by a course being false against itself and against its reverse by
virtue of containing exactly the same rows is not reported as ``A'' falseness. ``A''
falseness is only reported if there is some further, not-trivial falseness between rows
at two different positions within the course.

The third value returned is a two dimensional, square array, each of the elements of that
array being a possibly empty list of course heads. For element @var{e}, the list at
@var{m},@var{n} of this array, lead @var{m} of the plain course of @var{method} is false
against lead @var{n} of each of the courses starting with an element of @var{e}. The leads
are counted starting with zero. That is, if @var{s} is the stage of @var{method}, then
0≤@var{m}<@var{s}-1 and 0≤@var{n}<@var{s}-1.

A @code{type-error} is signaled if @var{method} is not a @code{method}. Signals a
@code{parse-error} if the place notation string cannot be properly parsed as place
notation at @var{method}'s stage. If @var{method} does not have its stage or
place-notation set a @code{no-place-notation-error}. If @var{method} is not at an even
stage major or above, does not have one hunt bell, the treble, or is differential, an
@code{inappropriate-method-error} is signaled.
@example
@group
 (multiple-value-bind (ignore1 groups ignore2)
     (method-falseness
       (method :stage 8
               :place-notation \"x34x4.5x5.36x34x3.2x6.3,8\"))
   (declare (ignore ignore1 ignore2)
   (fch-groups-string groups))
     @result{} \"BDacZ\"
 (fch-groups-string
   (second
     (multiple-value-list
       (method-falseness
         (lookup-method \"Zorin Surprise\" 10)))))
    @result{} \"T/BDa1c\"
@end group
@end example"
  (unless (method-changes method)
    (error 'no-place-notation-error :method method))
  (unless (let ((stage (method-stage method))) (and (evenp stage) (>= stage 8)))
    (error 'inappropriate-method-error
           :details "The method ~A does not have an even stage of major or above."
           :method method))
  (unless (equal (method-hunt-bells method) '(0))
    (error 'inappropriate-method-error
           :details "The method ~A is does not have the treble as its sole hunt bell."
           :method method))
  (unless (null (cdr (method-working-bells method)))
    (error 'inappropriate-method-error
           :details "The method ~A does not have all its working bells in a single cycle."
           :method method))
  (let* ((stage (method-stage method))
         (limit (- stage 1))
         (table (make-hash-table :size (* (method-lead-length method) limit)))
         (incidence (make-array `(,limit ,limit) :initial-element nil))
         (summary (make-hash-set :size +summary-initial-hash-set-size+)))
    (iter (with start := (if (eql stage 8) 7 6))
          (with lead-len := (method-lead-length method))
          (for row :in (method-plain-course method))
          (for i :from 0)
          (pushnew (cons (floor i lead-len) row)
                   (gethash (iter (with bells := (row-bells row))
                                  (with result := (position 0 bells))
                                  (for i :from start :to limit)
                                  (setf result (logior (ash result +row-signature-shift+)
                                                       (position i bells)))
                                  (finally (return result)))
                            table)
                   :test #'equalp))
    (iter (for (nil list) :in-hashtable table)
          (iter (for ((lead . row) . rest) :on list)
                (when rest
                  (iter (for (other-lead . other-row) :in rest)
                        (for fch := (permute-by-inverse row other-row))
                        (unless (aref incidence lead other-lead)
                          (setf (aref incidence lead other-lead)
                                (make-hash-set :size +incidence-initial-hash-set-size+)))
                        (hash-set-nadjoinf (aref incidence lead other-lead) fch)
                        (hash-set-nadjoinf summary fch)))))
    (dotimes (i limit)
      (dotimes (j limit)
        (fch-list-f (aref incidence i j))))
    (fch-list-f summary)
    (values summary (falseness-groups method summary) incidence)))


;;; Method lookup

(defmacro define-method-lookup (name lambda-list &optional docstring)
  (multiple-value-bind (required optional ignore keyword)
      (parse-ordinary-lambda-list lambda-list :normalize nil)
    (declare (ignore ignore))
    (let ((dispatch-to (format-symbol :roan "%~A" name)))
      (labels ((prune (x)
                 (mapcar #'(lambda (y) (if (atom y) y (first y))) x)))
      `(defun ,name ,lambda-list
         ,docstring
         (if-let ((fn (and (fboundp ',dispatch-to) (symbol-function ',dispatch-to))))
           (funcall fn ,@required ,@(prune optional) ,@(prune keyword))
           (signal-method-lookup-error ',name)))))))

(defun signal-method-lookup-error (name)
  (error "The ~A function requires that the full :roan system be loaded, with SQLite support."
         name))

(defmacro with-methods-database ((var &key database busy-timeout) &body body)
  "For more complex uses of Roan's methods database than the various @code{lookup-methods-}
facilitate SQL queries can be made against the database directly using the
@url{https://common-lisp.net/project/cl-sqlite/,CL-SQLITE API}. The
@code{with-methods-database} macro binds @var{var} to a database handle to Roan's methods
database and executes @var{body}, returning the result; returns @code{nil} if @var{body}
is empty. The database handle is closed on exit.

If @var{database} is supplied it should be a pathname designator to a database with the
Schema of Roan's methods database. If @code{nil}, the default, the standard location for
the database is used.

If @var{busy-timeout} is supplied it should be a non-negative integer, the number of
milliseconds to wait when attempting to operate on a busy database. If @code{nil}, the
default, operations on a busy database fail immediately.

Signals a @code{type-error} if @var{database} is neither @code{nil} nor a pathname
designator, or if @var{busy-timeout} is neither @code{nil} nor a non-negative integer. A
variety of SQLite or file system errors may be signaled if there is difficulty opening the
database file.
@example
@group
 (with-methods-database (conn)
   (iter (for (stage) :in-sqlite-query
                \"select stage from methods
                  where name = 'Cambridge Surprise'\"
              :on-database conn)
         (collect stage)))
     @result{} (6 8 10 12 14 16)
@end group
@end example

The Roan methods database has the following schema:
@example
@group
CREATE TABLE methods (stage int not null,
                      name text not null,
                      notation text not null,
                      canonicalRotation text,
                      firstTower text,
                      firstHand text);
CREATE TABLE version (updated text non null, etag text);
CREATE UNIQUE INDEX key on methods (stage, name);
CREATE INDEX notationIndex on methods (notation);
CREATE INDEX rotationIndex on methods (canonicalRotation);
@end group
@end example
The actual methods data is stored in the @code{methods} table. The @code{version} table
should contain only a single row, and records information used by
@code{update-methods-database} to determine whether or not the database needs updating.
Neither the indices nor the contents of the canonicalRotation column exist in the copy of
the database on the server, and are instead created by @code{update-methods-database}
after it is downloaded to reduce the size of the file transferred over the network.

The @code{stage} and @code{name} columns of the @code{methods} table are the stage and
name of the method described by that row of the table (polysemy alert: a table row is
unrelated to a change ringing row), and together form a primary key for that table; that
is, there can be no two rows that have the same @code{stage} and @code{name}.

The @code{notation} column is the corresponding place notation, in a canonical form with
internal places elided as by @code{:elide :lead-end} in @code{write-place-notation},
capital letters used for place eleven or above, a lower case @samp{x} used for cross, and
palindromic methods with an even lead length unfolded as by @code{:comma t}.

The @code{canonicalRotation} is the place notation corresponding to a unique rotation of
of changes of the method. That is, if two methods have the same changes, up to rotation,
they will have the same @code{canonicalRotation}. Note that this rotation is in no way
a preferred one, and is in most cases one few bands would ever care to ring; it is just a
way of comparing two methods to see if they are the same up to rotation. The place
notation for @code{canonicalRotation} is in the same canonical form as @code{notation}.

The @code{firstTower} and @code{firstHand} columns are succinct, textual representations
of when, and possibly where, the first peal in the methods occurred in tower or hand,
respectively, if that information is available in the database. Sometimes it is not
available, or no such peal has occured, in which the column is @code{null}.
@xref{canonicalize-method-place-notation}
@xref{write-place-notation}"
  `(execute-with-methods-database #'(lambda (,var) ,@body) ,database ,busy-timeout))

(define-method-lookup execute-with-methods-database (thunk database busy-timeout))

(define-method-lookup lookup-methods-by-name
    (name &key (stage *default-stage*) limit update url database busy-timeout)
  "===summary===
Roan provides an @url{https://www.sqlite.org/,SQLite database} of method definitions,
containing the same methods as in the @url{http://www.ringing.org/methods/,ringing.org
web site}. Because use of SQLite requires installation of a binary library,
@code{libsqlite3} available from the @url{https://www.sqlite.org/,SQLite web site},
some Roan users may perfer to avoid it. Roan, without any of the facilities in this
section for looking up methods, can be loaded by using the system @code{roan-base} instead
of the full @code{roan}. Everything else in Roan should work fine without the lookup
functions, and any calls to them will result in an error indicating that they are not
loaded.

In addition to all the methods recognized by the CCCBR at the time Roan's database was
last updated, the database also contains a variety of methods not CCCBR recognized, or, in
some cases, with names or classifications differing from those with the CCCBR's
imprimatur. For example, it contains methods with jump changes; methods that the Council
does not consider distinct from others (for example, New Grandsire); sometimes attaches
commonly-used names to methods in addition to those blessed by the Council (for example,
what the Council calls Bastow Little Bob Doubles is in the database under that name, and
also under St Helen's Doubles and Cloister Doubles); and methods that were disavowed by
the Council because they were rung and named in a peal that did not meet the Council's
requirements at the time is was rung (for example, Brindle Bob Royal, which itself met the
Council's requirements at the time it was rung in spliced, but the peal contained another
method, then illegal but now perfectly acceptable even to the Council). While this
database does extend what the Central Council's collection of methods provides, it is
still not as complete and comprehensive as it could be, though I hope it is at least
helpful.

This database can be interrogated with the @code{lookup-methods-by-name},
@code{lookup-method}, @code{lookup-methods-by-notation} and
@code{lookup-methods-from-changes} functions.

The methods in the database provided by Roan are a collation of methods extracted from
several different sources
@itemize @bullet
@item
The Central Council of Church Bell Ringers Method's Committee's
@url{http://www.methods.org.uk/,,collection of methods}. This collection is copyright
by the Central Council of Church Bell Ringers, and the method definitions extracted
from it have been modified: their place notation and other details have been reformated,
and they have been merged with method definitions from other sources.

@item
Tony Smith's
@url{http://www.methods.org.uk/supplement/provisionally-named/intro.txt,,
Collection of Provisionally Named Methods}. This collection is copyright by Tony Smith,
and the method definitions extracted from it have been modified: their place notation and
other details have been reformated, and they have been merged with method definitions from
other sources.

@item
The Central Council's 1988 @emph{Collection of Plain Methods},

@item
Steven Coleman's @emph{The Method Ringer's Companion}, 1995,

@item
Peter Hinton's
@url{http://www.cambridgeringing.info/Methods/Doubles/DoublesMethods-index.htm,,
Palindromic Plain Doubles Methods} web page,

@item
a variety of helpful messages on the
@url{http://www.bellringers.net/mailman/listinfo/ringing-theory_bellringers.net,,
ringing-theory mailing list},

@item and tradition and word of mouth.
@end itemize
===endsummary===
The @code{lookup-methods-by-name} function returns a list of @code{method}s of a given
@var{stage} and with a name matching @var{name}, or an empty list if there are no such
methods in the database. If the @var{stage} argument is not provided it defaults to the
current value of @code{*default-stage*}. Comparison is done case-insensitively. Note that,
just like for @code{method} objects, the name in this function is not the same as the name
in the CCCBR's taxonomy: it includes what the CCCBR calls the class as well as appropriate
modifiers such as ``Differential'' and ``Little''. That is, for purposes of this function,
the name is the method's entire title with the stage removed. So, for example, the name of
Cambridge Surprise Major is ``Cambridge Surprise'', and that of Baldrick Differential
Little Bob Cinques is ``Baldrick Differential Little Bob''.

The @var{name} argument to @code{lookup-methods-by-name} can include wildcard characters:
@samp{?} matches any one character, and @samp{*} matches any sequence of zero or more
characters. To include a @samp{?} or @samp{*} in a string, not as a wildcard, escape it
with a backslash @samp{\\}. Note that in Lisp literal strings you must backslash escape a
backslash so you will typically end up with two backslashes. To include a backslash in a
@var{name} pattern escape it with a backslash; in a Lisp literal string this will be a
total of four backslashes. Any other use of backslash in a @var{name} pattern other than
escaping @samp{?}, @samp{*} or @samp{\\} signals an error.

For the common case of looking up a single method by name the function
@code{lookup-method} is available. If one @code{method} is found it is returned; if none
are found @code{nil} is returned; and if multiple @code{method}s are found a
@code{too-many-methods-error} is signaled. Apart from the name, which may contain
wildards, and the stagge, no other arguments may be supplied, @code{lookup-method}
behaving similarly to @code{lookup-methods-by-name} when @var{limit}, @var{update},
@var{url}, @var{database} and @var{busy-timeout} are all @code{nil} or unsupplied.

The @code{lookup-methods-by-notation} function returns a list of @code{method}s of a given
@var{stage} and with a plain lead defined by the place notation @var{notation}, a
string, or an empty list if there are no such methods in the database. If
@var{stage} is not supplied it defaults to the current value of @code{*default-stage*}. If
the generalized boolean @var{rotations} is true it also returns any methods whose changes
are a rotation of those given. While the CCCBR recognizes only one method with any given
notation, or rotation thereof, Roan's database may contain multiple such. Wildcards cannot
be used in @var{notation}.

The @code{lookup-methods-from-changes} function returns a list of @code{method}s with a
given list of @var{changes} constituting its plain lead. All the elements of the list
@var{changes} must be @code{row}s, and be of the same stage. The @var{rotations} argument
is as for @code{lookup-methods-by-notation}.

There is no guarantee of what order methods are in the lists returned by any of these
three functions.

For any of these functions the @var{limit} keyword argument can be used to limit the
number of methods returned. If supplied it should be a positive integer. If @code{nil},
the default, there is no limit.

For any of these functions the @var{update} argument can be used to ask that the database
be updated from @url{http://www.ringing.org/methods/,,ringing.org} before querying it.
Possible values for @var{update} are:
@table @asis
@item @code{nil} (the default)
no updating is done: whatever version of the database is
already present is used as is

@item @code{:query} or @code{t}
@url{http://www.ringing.org/methods/,ringing.org} is queried
before executing the lookup, and, if it appears the version of the database on the server
is more recent than that already on the local machine, a fresh copy is downloaded from the
web site and used to replace the version already present

@item a non-negative integer
a time duration, in units of seconds: only if at least this
much time has elapsed since the version on the local machine was created is the server
queried and, if a new version is present there, downloaded; the time the database on the
server was created is not the same as the created or modified timestamp on the local file,
nor the time it was download to the local machine

@item @code{:force}
the most recent version on the server is downloaded, regardless of
whether or not it appears to be any more recent than the version already on the local
machine
@end table
An error is signaled if @var{update} has any other value. If the database does not exist
on the local machine an @code{error} is signaled if @var{update} is @code{nil}, and
otherwise an attempt is made to download and install it.

Unfortunately the libraries required to download and unzip an updated libary do not
currently (as of June 2017) work in CLISP or LispWorks. An attempt to update the the
database with a non-nil value of @var{update} will signal an error in these Lisp
implementations. @xref{update-methods-database}.

The @var{url} argument can be used to specify a different location for downloading a
fresh database, if @var{update} indicates that such an action should be taken. If
@var{update} is @code{nil}, or otherwise indicates that the server need not be queried,
@var{url} is ignored. If @code{nil} the default location is used, and otherwise @var{url}
should be a string.

The @var{database} argument enables use of a different database than Roan's default. It
should be a pathname designator for an existing file which is an SQLite database with the
same schema as Roan's default database. @xref{with-methods-database} for further
information on such databases. If @var{database} is @code{nil}, the default, Roan's
default location for the database is used.

The @var{busy-timout} argument specifies how long to wait, in milliseconds, for a locked
database; if @code{nil}, the default, operations on a locked database fail immediately.

There is no guarantee that any @code{method} object returned by any of these functions is
distinct from that returned by a different call to the same one or a different one that
needs to return such an object describing the same underlying method. For example, if two
different invocations of one or two of these methods are asked to provide a definition for
Cambridge Surprise Majoy the resulting @code{method} objects may or may not be @code{eq},
and subsequent changes to one may or may not be reflected in the ``other'' (since it may
or may not be the same one).

A @code{type-error} is signaled if @var{stage} is not a @code{stage}; @var{name} or
@var{notation} is not a string; @var{changes} is not a non-empty list of @code{row}s;
@var{limit} is neither @code{nil} nor a positive integer; @var{update} is not of any of
the types itemized above; @var{url} is neither @code{nil} nor a string; @var{database} is
not a pathname designator; or @var{busy-timeout} is neither @code{nil} nor a non-negative
integer. A @code{too-many-methods-error} is signaled if two or more methods match the
specification provided to @code{lookup-method}. A @code{parse-error} is signaled if
@var{notation} is a string and is not parseable as place notation at @var{stage}. An
@code{error} is signaled if @var{name} contains a @samp{\\} followed by anything other
than @samp{?}, @samp{*} or @samp{\\}; or if @var{changes} is a list of @code{row}s, but
they are not all of the same stage.

A variety of SQLite, file system or network errors may be signaled if there is difficulty
opening the database file or, if necessary, reaching the server to download a fresh
database.
@example
@group
 (method-place-notation
   (lookup-method \"Advent Surprise\" 8))
     @result{} \"36x56.4.5x5.6x4x5x4x7,8\"
 (method-place-notation
   (first (lookup-methods-by-name \"A?ve?t Sur*e\" :stage 8)))
     @result{} \"36x56.4.5x5.6x4x5x4x7,8\"
 (method-place-notation
   (lookup-method \"Advent Sur?rise\" 8))
     @result{} \"36x56.4.5x5.6x4x5x4x7,8\"
@end group
@group
 (method-property
   (lookup-methods-by-name 8 \"Advent Surprise\" :stage 8))
   :first-tower)
     @result{} \"1988-07-31 at Boston, Massachusetts, USA (Advent)\"
@end group
 (lookup-methods-by-name \"No-such-method-anywhere\" 8) @result{} nil
 (lookup-method \"No-such-method-anywhere\" 9) @result{} nil
 (length (lookup-methods-by-name 8 \"Advent *\")) @result{} 3
@group
 (length (lookup-methods-by-name 8 \"Advent *\" :limit 2))
   @result{} 2
@end group
@group
 (method-title
   (first
     (lookup-methods-by-notation 8 \"36x56.4.5x5.6x4x5x4x7,8\")))
       @result{} \"Advent Surprise Major\"
@end group
 (method-title
  (first (lookup-methods-by-changes '(!214365 !132546))))
    @result{} \"Original Minor\"
@group
 (mapcar #'method-title
  (lookup-methods-by-notation 5 \"5,1.3.1\"))
    @result{} (\"Bastow Little Bob Doubles\"
               \"Cloister Doubles\"
               \"St Helen's Doubles\")
@end group
@group
 (mapcar #'method-title
        (lookup-methods-by-notation \"3,1.5.1.5.1\"
                                    :stage 5
                                    :rotations nil))
   @result{} (\"Grandsire Doubles\")
@end group
@group
 (mapcar #'method-title
        (lookup-methods-by-notation \"3,1.5.1.5.1\"
                                    :stage 5
                                    :rotations t))
   @result{} (\"Grandsire Doubles\" \"New Grandsire Doubles\")
@end group
@end example")

(define-condition too-many-methods-error (error)
  ((count :reader too-many-methods-error-count :initarg :count))
  (:documentation "Signaled in circumstances when only a single method was expected but
multiple were found. Contains one potentially useful slot accessible with
@code{too-many-methods-error-count}, the number of matching methods found.")
  (:report (lambda (condition stream)
             (format stream "Too many~@[, ~D,~] methods match the retrieval specification."
                     (too-many-methods-error-count condition)))))

(define-method-lookup lookup-method (name &optional (stage *default-stage*))
  "===merge: lookup-methods-by-name 1")

(define-method-lookup lookup-methods-by-notation
    (place-notation &key (stage *default-stage*) rotations limit update url database busy-timeout)
  "===merge: lookup-methods-by-name 2")

(define-method-lookup lookup-methods-from-changes
    (changes &key rotations limit update url database busy-timeout)
  "===merge: lookup-methods-by-name 3")

(define-method-lookup update-methods-database (&key since force url database)
  "Possibly downloads a fresh copy of the methods database from
@url{http://www.ringing.org/methods/,the ringing.org server}.

If @var{since} is not @code{nil} it should be a non-negative integer. If fewer than that
number of seconds have passed since the database was created, as recorded in the database,
@code{update_methods-databse} will return @code{nil} immediately, doing nothing further.

Typically, before downloading the database, @code{update-methods-database} queries the
server to see if the copy thereon differs from the local copy, and does not download it if
they are the same. If @var{force} is not @code{nil}, however, it will always attempt to
download a fresh copy of the database, even if it appears to be unchanged.

However, if the database does not exist on the local machine both @var{since} and
@var{force} are ignored, and in this case an attempt is always made to download the
database. That is, if the database does not exist locally the behavior is as if
@var{since} were @code{nil} and @var{force} were @code{t}.

For example,
@example
 (update-methods-database :since (* 24 60 60 30))
@end example
will download a fresh database only if the current one was created at least thirty
days ago, and the version on the server is different than the one currently
stored locally; or if there is currently no such database on the local machine.

If @var{url} is @code{nil}, the default, a standard location from which to download the
database is used. This can be changed by providing a URL as a string as @var{url}.

If @var{database} is @code{nil}, the default, a standard location is used for the
database. This can be overriden by supplying a pathname designator as @var{database}.

Returns the pathname of the database if a fresh database is downloaded and @code{nil}
otherwise.

Signals a @code{type-error} if since is neither @code{nil} nor a non-negative integer;
@var{url} is neither @code{nil} nor a string; or @var{database} is neither @code{nil}
nor a pathname designator.

Unforunately some of the libraries (Drakma, usocket and zip) required to download and
unzip the updated database either don't install or no longer work in CLISP and LispWorks,
as of June 2017. On these implementations a call to @code{update-methods-database} will
result in an error. Several work arounds are practical:
@itemize
@item
The version of the database that is downloaded with Roan will have been reasonably current
at the time that version of Roan was released. For many purposes there may be no need to
update it.

@item
Roan can be used to update it in a different Lisp implementation, such as SBCL or
Clozure CL: the copy of the database so updated will then be available to CLISP
or LispWorks.

@item
The database can be downloaded by hand. Consult the source code for
@code{update-methods-database} for the URL, and where to put the file after unzipping it.
If simply unzipped and not altered it will work for all purposes except searching for
rotations of exisiting methods. To support that as well another column of one of the
tables in the database must be populated after downloading the new database; in addition
some indecies are created to speed up access slightly. This, too, can easily be done by
hand: again, consult the source code.
@end itemize")

(define-method-lookup methods-database-attributes (&optional database)
  "Returns five values describing the Roan methods database:
@itemize
@item
the truename of the database file

@item
the date and time the database was created, as a univeral time; note that this will
typically not be be same as the time the file was created nor the time it was downloaded

@item
a string (the ETag) representing that state of the file on the server, typically used to
determine if an updated version is available

@item
the number of methods (that is, distinct stage and name pairs) that the database
contains

@item
a vector, indexed by stage, of the number methods at each stage that the database
contains
@end itemize
If @var{database} does not name a database currently on the local machine a single value,
@code{nil}, is returned. If @var{database} is @code{nil} or not supplied the default
location for Roan's database is used.

Signals a @code{type-error} if @var{database} is neither @code{nil} nor a pathname
designator. May also signal a variety of file system or SQLite errors if the database
cannot be opened or is not in the correct format.")
