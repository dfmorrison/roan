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


;;; matching

(defconstant +pattern-cache-size+ 40)

(defparameter *pattern-cache* (make-lru-cache +pattern-cache-size+ :test #'equalp))

(defun row-match-p (pattern row &optional following-row)
  "===summary===
Roan provides a simple pattern language for matching rows. This is useful, among other
things, for counting rows considered particularly musical or unmusical.

A pattern string describes the bells in a row, with several kinds of wildcards and other
constructs matching multiple bells. Bells' names match themselves, so, for example,
\"13572468\" matches queens on eight. A question mark matches any bell, and an asterisk
matches runs of zero or more bells. Thus \"*7468\", at major, matches all twenty-four
7468s, and \"?5?6?7?8\" matches all twenty-four major rows that have the 5-6-7-8 in the
positions they are in in tittums. Alternatives can be separated by the pipe character,
@samp{|}. Thus \"13572468|12753468\" matches either queens or Whittingtons. Concatentation
of characters binds more tightly than alternation, but parentheses can be used to group
subexpressions. Thus \"*(4|5|6)(4|5|6)78\" at major matches all 144 combination rollups.
When matched against two major rows \"?*12345678*?\" matches wraps of rounds, but not
either row being rounds.

Two further notations are possible. In each case it does not extend what can be expressed,
it merely makes more compact something that can be expressed with the symbols already
described. The first is a bell class, which consits of one or more bell names within
square brackets, and indicates any one of those bells. Thus an alternative way to match
the 144 combination rollups at major is \"*[456][456]78\".

A more compact notation is also available for describing runs of consecutive bells. Two
bell symbols separated by a hyphen represent the run of bells from one to the other. Thus
\"*5-T\" matches all rows ending 567890ET. If such a run description is followed by a
solidus, @samp{/}, and a one or two digit integer, it matches all runs of the length of
that integer that are subsequences of the given run. Thus \"*2-8/4\" is equivalent to
\"*(2345|3456|4567|5678)\". If instead of a solidus a percent sign, '%', is used it
matches subsequences of both the run and its reverse. Thus \"1-6%4*\" matches all little
bell runs off the front of length four selected from the bells 1 through 6, and is
equivalent to the pattern \"(1234|4321|2345|5432|3456|6543)*\". There is some possible
ambiguity with this notation, in that the second digit of an integer following a solidus
or percent sign could be interpreted as a digit or a bell symbol. In these cases it is
always interpreted as a digit, but the other use can be specified by using parentheses or
a space.

Spaces, but no other whitespace, can be included in patterns. However no spaces may be
included within bell classes or run descriptions. Thus \" 123 [456] 7-T/3 * \" is
equivalent to \"123[456]7-T/3*\", but both \"123[ 4 5 6 ]7-T/3*\" and
\"123[456]7-T / 3*\" are illegal, and will cause errors to be signaled.

In addition to strings, patterns may be represented by parse trees, which are simple list
structures made up of keywords and bells (that is, small, non-negative integers). Strings
are generally more convenient for reading and writing patterns by humans, but parse trees
can be more convenient for programmatically generated patterns. The function
@code{pattern-parse} converts the string representation of a pattern to such a tree
structure. Sequences of elements are represented by lists starting with @code{:sequence};
alternatives by lists starting with @code{:or}; bell classes by lists of the included
bells preceded by @code{:class}; runs by a list of the form @code{(:run @var{start}
@var{end} @var{length} @var{bi})}, where @var{start} is the starting @code{bell},
@var{end} the ending @code{bell}, @var{length} the length of the run, and @var{bi} is a
generalized boolean saying whether or not the runs are bidirectional; @code{bell}s are
represented by themselves; and @samp{?} and @samp{*} by @code{:one} and @code{:any},
respectively. The elements of the @code{:sequence} and @code{:or} lists may also be lists
themselves, representating subexpressions. For example, the string \"(?[234]*|*4-9%4?)*T\"
is equivalent to the tree
@example
@group
 (:sequence (:or (:sequence :one (:class 1 2 3) :any)
                 (:sequence :any (:run 3 8 4 t) :one))
            :any
            11)
@end group
@end example
===endsummary===
Determines whether @var{row}, or a pair of consecutive @code{row}s, @var{row} and
@var{following-row}, match a pattern. If @var{following-row} is supplied it should be of
the same stage as @var{row}. The @var{pattern} may be a string or a tree, and should be
constructed to be appropriate for the stage of @var{row}; an error is signaled if it
contains explicit matches for bells of higher stage than @var{row}. Returns a generalized
boolean indicating whether or not @var{pattern} matches.
@example
@group
 (row-match-p \"*[456][456]78\" !32516478) @result{} t
 (row-match-p \"*[456][456]78\" !12453678) @result{} nil
 (row-match-p \"*[456][456]78\" !9012345678) @result{} t
 (row-match-p \"?*123456*?\" !651234 !562143) @result{} t
 (row-match-p \"?*123456*?\" !651234 !652143) @result{} nil
 (row-match-p \"?*123456*?\" !123456) @result{} nil
 (row-match-p '(:sequence :any 6 7) !65432178) @result{} t
 (row-match-p '(:sequence :any 6 7) !23456781) @result{} nil
@end group
@end example

Signals an error if @var{pattern} cannot be parsed as a pattern, if @var{row} is not a
@code{row}, if @var{following-row} is neither a @code{row} nor @code{nil}, if
@var{pattern} contains bells above the stage of @var{row}, or if @var{following-row} is a
@code{row} of a different stage than @var{row}.

Care should be used when matching against two rows. In the usual use case where searching
for things like wraps every row typically will be passed twice to this method, first as
@var{row} and then as @var{following-row}. A naive pattern might end up matching twice,
and thus double counting. For example, if at major \"*12345678*\" were used to search for
wraps of rounds it would match whenever @var{row} or @var{following-row} were themselves
rounds, possibly leading to double counting. Instead a search for wraps of rounds might be
better done against something like \"?*12345678*?\"."
  (unless pattern
    (row-match-error "No pattern was supplied."))
  (let* ((stage (stage row))
         (key (cons stage pattern))
         (dfa (getcache key *pattern-cache*)))
    (when (and following-row (not (eql (stage following-row) stage)))
      (row-match-error "Rows do not have the same stage: ~A is ~A and ~A is ~A."
                       row (stage-name (stage row))
                       following-row (stage-name (stage following-row))))
    (unless dfa
      (unless (listp pattern)
        (setf pattern (parse-pattern (string pattern) stage)))
      (setf (getcache key *pattern-cache*)
                 (setf dfa (make-dfa (make-nfa pattern stage t)))))
    (not (null (%rows-match-p dfa row following-row)))))

(define-condition row-match-error (simple-error) ()
  (:documentation "Signaled when an anomolous situation is encountered when attempting
to match @code{row}s against a pattern."))

(defun row-match-error (format &rest args)
  (error 'row-match-error :format-control format :format-arguments args))


;;; Parsing

(defvar *pattern-parse-state*)

(defun parse-pattern (pattern &optional (stage *default-stage*))
  "Converts a string representation of a pattern to its parse tree, and returns it. The
@var{stage} is the stage for which @var{pattern} is parsed, and defaults to
@code{*default-stage*}. If @var{pattern} is a non-empty list it is presumed to be a
pattern parse tree and is returned unchanged. Signals a @code{type-error} if @var{pattern}
is neither a string nor a non-empty list, or if @var{stage} is not a @code{stage}. Signals
a @code{parse-error} if @var{pattern} is a string but cannot be parsed as a pattern, or
contains bells above those appropriate for @var{stage}.
@example
@group
 (parse-pattern \"(?[234]*|*4-9%4?)*T\" 12)
     @result{}
   (:sequence (:or (:sequence :one (:class 1 2 3) :any)
                   (:sequence :any (:run 3 8 4 t) :one))
              :any
              11)
@end group
@end example"
  (check-type* pattern (or string cons))
  (check-type* stage stage)
  (when (listp pattern)
    (return-from parse-pattern pattern))
  (let ((*pattern-parse-state* (make-pattern-parse-state pattern stage)))
    (advance-token *pattern-parse-state*)
    (when (zerop (length pattern))
      (pattern-parse-error "empty pattern supplied"))
    (unless (current-token)
      (pattern-parse-error "does not appear to start as a row pattern"))
    (prog1 (parse-disjunction)
      (unless (pattern-parse-state-end-p)
        (pattern-parse-error "unexpected characters at end of pattern")))))

(define-condition pattern-parse-error (parse-error)
  ((message :initarg :message :initform nil :accessor pattern-parse-error-message)
   (pattern :initarg :pattern :initform nil :accessor pattern-parse-error-pattern)
   (index :initarg :index :initform nil :accessor pattern-parse-error-index))
  (:report (lambda (condition stream)
             (let ((message (pattern-parse-error-message condition))
                   (pattern (pattern-parse-error-pattern condition))
                   (index (pattern-parse-error-index condition)))
               (format stream "Unable to parse row pattern~@[, ~A~].~@[~%~A~]~@[~%~vT^~]"
                       message pattern (and pattern (integerp index) index)))))
  (:documentation "An error signaled when attempting to parse a malformed row pattern.
Contains three potenitally useful slots accessible with @code{pattern-parse-error-message},
@code{pattern-parse-error-pattern} and @code{pattern-parse-error-index}."))

(defclass pattern-parse-state ()
  ((string :initarg :string)
   (stage :initarg :stage)
   (index :initform 0)
   (token :initform nil)
   (token-end :initform 0)))

(defun make-pattern-parse-state (string stage)
  (make-instance 'pattern-parse-state :string string :stage stage))

(defun current-token (&optional (state *pattern-parse-state*))
  (slot-value state 'token))

(with-bell-property-resolver
  (define-constant +pattern-token+*
    (ppcre:create-scanner "^ *([?*|()]|\\[\\p{bell}+\\]|\\p{bell}-\\p{bell}(?:[/%]\\d\\d?)?|\\p{bell}) *"
                          :case-insensitive-mode t)
    :test #'same-type-p)
  (define-constant +run-token+* (ppcre:create-scanner "(\\p{bell})-(\\p{bell})(?:([/%])(\\d\\d?))?"
                                                     :case-insensitive-mode t)
    :test #'same-type-p))

(define-constant +pattern-characters+
    (plist-hash-table '(#\* :any #\? :one #\| :or #\( :open #\) :close))
  :test #'equalp)

(defun advance-token (&optional (state *pattern-parse-state*))
  (with-slots (string index token token-end stage) state
    (prog1 token
      (multiple-value-bind (start end reg-start reg-end)
          (ppcre:scan +pattern-token+* string :start index)
        (if start
            (setf token-end (aref reg-end 0)
                  index end
                  token (subseq string (aref reg-start 0) token-end)
                  token (or (gethash (aref token 0) +pattern-characters+ nil)
                            (cond ((eql (aref token 0) #\[) (parse-bell-class token stage))
                                  ((> (length token) 1) (parse-run token stage)))
                            (ensure-valid-bell (bell-from-name (aref token 0)) stage t)))
            (setf token nil))))))

(defun pattern-parse-state-end-p (&optional (state *pattern-parse-state*))
  (with-slots (string index token) state
    (and (null token) (eql index (length string)))))

(defun pattern-parse-error (&optional message &rest args)
  (with-slots (string token-end) *pattern-parse-state*
    (error (make-condition 'pattern-parse-error
                           :message (and message (apply #'format nil message args))
                           :pattern string
                           :index token-end))))

(defun parse-disjunction ()
  (iter (collect (parse-alternative) :into result)
        (while (eq (current-token) :or))
        (advance-token)
        (finally (return (if (rest result)
                             (cons :or result)
                             (first result))))))

(defun parse-alternative ()
  (iter (for e := (parse-element))
        (while e)
        (collect e :into result)
        (finally (return (if (rest result)
                             (cons :sequence result)
                             (first result))))))

(defun parse-element ()
  (case (current-token)
    (:open (advance-token)
           (prog1 (parse-disjunction)
             (unless (eq (current-token) :close)
               (pattern-parse-error "close parenthesis expected"))
             (advance-token)))
    ((:or :close) nil)
    (t (advance-token))))

(defun parse-bell-class (string stage)
  (cons :class (ensure-valid-bell-class (map 'list #'bell-from-name
                                             (subseq string 1 (- (length string) 1)))
                                        stage
                                        t)))

(define-modify-macro logiorf (&rest numbers) logior)

(defun ensure-valid-bell-class (class &optional (stage +maximum-stage+ ) parsing)
  (iter (with present := 0)
        (for b :in class)
        (for n := (ash 1 b))
        (ensure-valid-bell b stage parsing)
        (when (logtest present n)
          (if parsing
              (pattern-parse-error "duplicate bell ~A in class" (bell-name b))
              (error "Duplicate bell ~A in ~S." (bell-name b) (cons :class class))))
        (logiorf present n))
  class)

(defun parse-run (string stage)
  (multiple-value-bind (ignore parts) (ppcre:scan-to-strings +run-token+* string)
    (declare (ignore ignore))
    (destructuring-bind (start end bidirectional length) (coerce parts 'list)
      (setf start (bell-from-name start))
      (setf end (bell-from-name end))
      (setf length (if length (parse-integer length) (+ (abs (- end start)) 1)))
      (setf bidirectional (string= bidirectional "%"))
      (cons :run (ensure-valid-run (list start end length bidirectional) stage t)))))

(defun ensure-valid-run (run &optional (stage +maximum-stage+) parsing)
  (destructuring-bind (start end length bidirectional) run
    (declare (ignore bidirectional))
    (ensure-valid-bell (max start end) stage parsing)
    (unless (<= length (+ (abs (- end start)) 1))
      (if parsing
          (pattern-parse-error "~A-~A is less than ~D bells long"
                               (bell-name start) (bell-name end) length)
          (error "Span from ~A to ~A is less than ~D bells long in ~S."
                               (bell-name start) (bell-name end) length (cons :run run)))))
  run)

(defun ensure-valid-bell (bell stage &optional parsing)
    (unless (< bell stage)
      (funcall (if parsing #'pattern-parse-error #'error)
               "~A is not a bell at ~A"
               (bell-name bell) (stage-name stage)))
    bell)


;;; Convert pattern parse trees back to strings

(defgeneric format-pattern-element (tree stream &optional args context))

(defun format-pattern (tree &optional (upper-case *print-bells-upper-case*))
  "Returns a string that if parsed with @code{parse-pattern}, would return the parse tree
@var{tree}. Note that the generation of a suitable string from @var{tree} is not unique,
and this function simply returns one of potentially many equivalent possibilities. The
case of any bells represented by letters is controlled by @var{upper-case}, which defaults
to the current value of @code{*print-bells-upper-case*}. Signals an error if tree is not
a parse tree for a pattern.
@example
 (format-pattern '(:sequence 0 1 2 :any 7) t) @result{} \"123*8\"
@end example"
  (unless tree
    (error "No pattern supplied."))
  (let ((*print-bells-upper-case* upper-case))
    (with-output-to-string (stream)
      (format-pattern-element tree stream))))

(defmethod format-pattern-element ((list list) stream &optional args context)
  (declare (ignore args))
  (format-pattern-element (first list) stream (rest list) context))

(defmethod format-pattern-element ((key (eql :sequence)) stream &optional args context)
  (declare (ignore key context))
  (iter (with previous-needs-space := nil)
        (for e :in args)
        (setf previous-needs-space
              (format-pattern-element e stream nil
                                      (if previous-needs-space
                                          :space-required
                                          :parens-required)))
        (finally (return previous-needs-space))))

(defmethod format-pattern-element ((key (eql :or)) stream &optional args context)
  (declare (ignore key))
  (when (eq context :parens-required)
    (princ #\( stream))
  (iter (for e :in args)
        (unless (first-time-p)
          (princ #\| stream))
        (format-pattern-element e stream))
  (when (eq context :parens-required)
    (princ #\) stream))
  nil)

(defmethod format-pattern-element ((bell integer) stream &optional args context)
  (declare (ignore args))
  (format stream "~:[~; ~]~A" (eq context :space-required) (bell-name bell)))

(defmethod format-pattern-element ((wildcard (eql :one)) stream &optional args context)
  (declare (ignore wildcard args context))
  (princ #\? stream)
  nil)

(defmethod format-pattern-element ((wildcard (eql :any)) stream &optional args context)
  (declare (ignore wildcard args context))
  (princ #\* stream)
  nil)

(defmethod format-pattern-element ((key (eql :class)) stream &optional args context)
  (declare (ignore key context))
  (ensure-valid-bell-class args)
  (format stream "[~{~A~}]" (mapcar #'bell-name args)))

(defmethod format-pattern-element ((key (eql :run)) stream &optional args context)
  (declare (ignore key))
  (ensure-valid-run args)
  (destructuring-bind (from to length bidirectional) args
    (let ((length-needed (or bidirectional (<= length (abs (- from to))))))
      (format stream "~:[~; ~]~A-~A~:[~2*~;~:[/~;%~]~D~]"
              (eq context :space-required) (bell-name from) (bell-name to)
              length-needed bidirectional length)
      length-needed)))

(defmethod format-pattern-element (key stream &optional args context)
  (declare (ignore stream args context))
  (error "Unknown pattern parse tree element ~S" key))


;;; Non-deterministic finite automata

(defclass nfa-state ()
  ((transitions :initarg :transitions :reader transitions :type 'simple-vector)
   (empty-transition :initform '() :accessor empty-transition :type 'list)
   (final :initform nil :reader get-final :writer set-final)
   (id :initform nil)))

(defvar *nfa-stage*)

(defun make-nfa-state (&optional (stage *nfa-stage*))
  (make-instance 'nfa-state :transitions (make-array stage
                                                     :element-type 'list
                                                     :initial-element '())))

(defun add-transition (source-state destination-state &optional bell)
  (if bell
      (push destination-state (svref (transitions source-state) bell))
      (push destination-state (empty-transition source-state)))
  destination-state)

(defgeneric build-nfa (thing from-state &optional args))

(defun make-nfa (tree *nfa-stage* final-token)
  (let* ((initial-state (make-nfa-state))
         (final-state (build-nfa tree initial-state)))
    (set-final final-token final-state)
    initial-state))

(defmethod build-nfa ((list list) from-state &optional args)
  (declare (ignore args))
  (build-nfa (first list) from-state (rest list)))

(defmethod build-nfa ((key (eql :sequence)) state &optional args)
  (declare (ignore key))
  (dolist (e args)
    (setf state (build-nfa e state)))
  state)

(defmethod build-nfa ((key (eql :or)) from-state &optional args)
  (declare (ignore key))
  (let ((to-state (make-nfa-state)))
    (dolist (e args)
      (add-transition (build-nfa e from-state) to-state))
    to-state))

(defmethod build-nfa ((bell integer) from-state &optional args)
  (declare (ignore args))
  (ensure-valid-bell bell *nfa-stage*)
  (add-transition from-state (make-nfa-state) bell))

(defmethod build-nfa ((wildcard (eql :one)) from-state &optional args)
  (declare (ignore wildcard args))
  (let ((to-state (make-nfa-state)))
    (dotimes (i *nfa-stage*)
      (add-transition from-state to-state i))
    to-state))

(defmethod build-nfa ((wildcard (eql :any)) from-state &optional args)
  (declare (ignore wildcard args))
  (let ((to-state (add-transition from-state (make-nfa-state))))
    (dotimes (i *nfa-stage*)
      (add-transition to-state to-state i))
    to-state))

(defmethod build-nfa ((key (eql :class)) from-state &optional args)
  (declare (ignore key))
  (ensure-valid-bell-class args *nfa-stage*)
  (let ((to-state (make-nfa-state)))
    (dolist (b args)
      (add-transition from-state to-state b))
    to-state))

(defmethod build-nfa ((key (eql :run)) from-state &optional args)
  (declare (ignore key))
  (ensure-valid-run args *nfa-stage*)
  (let ((to-state (make-nfa-state)))
    (destructuring-bind (from to length bidirectional) args
      (labels ((add-run (start end)
                 (if (< start end)
                     (iter (with state := from-state)
                           (for i :from start :below end)
                           (setf state (add-transition state (make-nfa-state) i))
                           (finally (add-transition state to-state end)))
                     (iter (with state := from-state)
                           (for i :from start :above end)
                           (setf state (add-transition state (make-nfa-state) i))
                           (finally (add-transition state to-state end)))))
               (add-runs (start end)
                 (if (< start end)
                     (iter (for i :from start)
                           (while (<= (+ i length -1) end))
                           (add-run i (+ i length -1)))
                     (iter (for i :downfrom start)
                           (while (>= (- i length -1) end))
                           (add-run i (- i length -1))))))
        (add-runs from to)
        (when bidirectional
          (add-runs to from))))
    to-state))

(defmethod build-nfa (key from-state &optional args)
  (declare (ignore from-state args))
  (error "Unknown pattern parse tree element ~S" key))


;;; Deterministic finite automata

(defvar *last-nfa-id*)

(defun nfa-id (nfa-state)
  (with-slots (id) nfa-state
    (or id (setf id (incf *last-nfa-id*)))))

(defclass dfa-state ()
  ((transitions :initarg :transitions :reader transitions :type 'simple-vector)
   (final :initarg :final :reader get-final :type 'list)))

(defvar *dfa-stage*)
(defvar *dfa-states*)

(defun make-dfa-state (&optional (final '()) (stage *dfa-stage*))
  (make-instance 'dfa-state
                 :transitions (make-array stage :initial-element nil)
                 :final final))


(defun make-dfa (initial-nfa-state)
  (assert (null (slot-value initial-nfa-state 'id)))
  ;; NFAs are currently intended to be transient entities, used only to construct one
  ;; DFA, and not reused. If this changes in the future the IDs in the NFA states will
  ;; have to be zapped here.
  (let ((*dfa-stage* (length (transitions initial-nfa-state)))
        (*dfa-states* (make-hash-table))
        (*last-nfa-id* -1))
    (build-dfa-state (list initial-nfa-state))))

(defun build-dfa-state (nfa-states)
  (let ((table (make-hash-table)))
    (dolist (s nfa-states)
      (follow-empty-transitions s table))
    (let* ((states (hash-table-keys table))
           (signature (signature states)))
      (when-let (result (gethash signature *dfa-states* nil))
        (return-from build-dfa-state result))
      (clrhash table)
      (dolist (s states)
        (when-let (final-token (get-final s))
          (setf (gethash final-token table) t)))
      (let ((result (make-dfa-state (hash-table-keys table))))
        (setf (gethash signature *dfa-states*) result)
        (dotimes (i *dfa-stage*)
          (setf (aref (transitions result) i)
                (build-dfa-state (iter (for s :in states)
                                       (appending (aref (transitions s) i))))))
        result))))

(defun follow-empty-transitions (state table)
  (when (not (gethash state table nil))
    (setf (gethash state table) t)
    (dolist (s (empty-transition state))
      (follow-empty-transitions s table))))

(defun signature (states)
  (iter (with result := 0)
        (for s :in states)
        (logiorf result (ash 1 (nfa-id s)))
        (finally (return result))))

(defun %rows-match-p (dfa row1 row2)
  (macrolet ((follow-transitions (row)
               `(dotimes (i (stage ,row))
                  (if dfa
                      (setf dfa (svref (transitions dfa) (aref (row-bells ,row) i)))
                      (return-from %rows-match-p nil)))))
    (follow-transitions row1)
    (when row2
      (follow-transitions row2)))
  (and dfa (get-final dfa)))


;;; match-counters

(defconstant +initial-match-counter-size+ 20)

(defstruct (row-count (:constructor make-row-count (label pattern double-row-p))
                      (:copier nil)
                      (:predicate nil))
  label
  pattern
  double-row-p
  (handstroke 0)
  (backstroke 0))

(defstruct (match-counter (:constructor %make-match-counter (stage))
                          (:copier nil)
                          (:predicate nil))
  "===summary===
Often one would like to count how many times a variety of patterns match many different
rows. To support this use Roan provides @code{match-counter}s. After creating a
@code{match-counter} with @code{make-match-counter} you add a variety of patterns to it,
with @code{add-pattern} or @code{add-patterns}, each with a label, which will typically
be a symbol or string, but can be any Lisp object. You then apply the @code{match-counter}
to @code{row}s with @code{record-matches}, and query how many matches have occurred with
@code{match-counter-counts}.

The order in which patterns are added to a @code{match-counter} is preserved, and is
reflected in the return values of @code{match-counter-labels}, and
@code{match-counter-counts} called without a second argument. Replacing an existing
pattern by adding a different one with a @var{label} that is @code{equalp} to an existing
one does not change the order, but deleting a pattern with @code{remove-pattern} and
then re-adding it does move it to the end of the order. When a pattern has been replaced
by one with an @code{equalp} @var{label} that is not @code{eq} to the original @var{label}
which label is retained is undefined.

A @code{match-counter} also distinguishes matches that occur at handstroke from those
that occur at backstroke. Typically you tell the @code{match-counter} which stroke the
next @code{row} it is asked to match is on, and it then it automatically alternates
handstrokes and backstrokes for subsequent @code{row}s. For patterns that span two
rows, such as wraps, the stroke is considered to be that between the rows; for example a
wrap of rounds that spans a backstroke lead would be considered to be ``at'' backstroke.
@example
@group
 (let ((m (make-match-counter 8)))
   (add-patterns m '((cru \"*[456][456]78\")
                     (wrap \"?*12345678*?\" t)
                     (lb4 \"1-7%4*|*1-7%4\")))
   (loop for (row following)
         on (generate-rows #8!36.6.5.3x5.56.5,2)
         do (record-matches m row following))
   (values (match-counter-counts m)))
     @result{} ((cru . 3) (wrap . 1) (lb4 . 5))
@end group
@end example
===endsummary===
Used to collect statistics on how many rows match a variety of patterns."
  (stage *default-stage* :read-only t)
  (%handstroke-p t)
  (patterns-vector (make-array +initial-match-counter-size+ :fill-pointer 0 :adjustable t))
  (patterns-table (make-hash-table :test #'equalp :size +initial-match-counter-size+)
                 :read-only t)
  (dfas nil))

(defmethod print-object ((counter match-counter) stream)
  (print-unreadable-object (counter stream :type t :identity t)
    (format stream "~D ~:[back~;hand~] ~D"
            (match-counter-stage counter)
            (match-counter-%handstroke-p counter)
            (length (match-counter-patterns-vector counter)))))

(defun make-match-counter (&optional (stage *default-stage*))
  "Returns a fresh @code{match-counter}, initially containing no patterns, that is
configured to attempt to match patterns against @code{row}s of @var{stage} bells.
If not supplied, @var{stage} defaults to the current value of @code{*default-stage*}.
Attempts to add patterns only appropriate for a different stage or match rows of a
different stage with @code{record-matches} will signal an error."
  (check-type* stage stage)
  (%make-match-counter stage))

(defun add-pattern (counter label pattern &optional double-row-p)
  "Adds one or more patterns to those matched by the @code{match-counter} @var{count}.
A single pattern, @var{pattern}, is added, with label @var{label}, by @code{add-pattern}.
If the generalized boolean @var{double-row-p} is true two rows (which typically should be
consecutive) will be matched against @var{pattern}, and others one row; if not supplied
@var{double-row-p} is @code{nil}. Multiple patterns may be added together with
@code{add-patterns}: @var{lists} should be a list of lists, where the sublists are of the
form @code{(@var{label} @var{pattern} &optional @var{double-row-p})}, and the patterns are
added in the order given. In either case the @var{pattern} may be either a string or list
structure that is a parsed pattern, such as returned by @code{parse-pattern}. If
@var{label} is @code{equalp} to the label of a pattern already added to @var{counter} that
pattern will be replaced, and its corresponding counts reset to zero. Either function
reeturns @var{counter}. Either signals a @code{type-error} if @var{counter} is not a
@code{match-counter}. Signals an error if any of the @var{pattern}s are not an
appropriate pattern for the stage of @var{counter}."
  (setf (match-counter-dfas counter) nil)
  (let ((p (parse-pattern pattern (match-counter-stage counter))))
    (multiple-value-bind (current present-p)
        (gethash label (match-counter-patterns-table counter))
      (if present-p
          (setf (row-count-label current) label
                (row-count-pattern current) p
                (row-count-double-row-p current) double-row-p
                (row-count-handstroke current) 0
                (row-count-backstroke current) 0)
          (vector-push-extend (setf (gethash label (match-counter-patterns-table counter))
                                    (make-row-count label p double-row-p))
                              (match-counter-patterns-vector counter)))))
  counter)

(defun add-patterns (counter lists)
  "===merge: add-pattern"
  ;; Preflight stuff as much as practical so we don't end up with a partially updated
  ;; match-counter after an error.
  (check-type* counter match-counter)
  (check-type* lists list)
  (iter (for sublist :in lists)
        (check-type* sublist list)
        (for (label pattern double-row-p . rest) := sublist)
        (when (or rest (not (or (stringp pattern) (listp pattern))))
          (error "Not a suitable description of a pattern to add: ~S" sublist))
        (collect (list label
                       (parse-pattern pattern (match-counter-stage counter))
                       double-row-p)
          :into args)
        (finally (dolist (a args)
                   (apply #'add-pattern counter a))))
  counter)

(defun remove-pattern (counter label)
  "Removes any pattern in @code{method-counter} @var{count} with its label @code{equalp}
to @var{label}. Returns @code{t} if such a pattern was found and removed, and @code{nil}
otherwise. Signals a @code{type-error} if @var{count} is not a @code{method-counter}."
  (multiple-value-bind (count present-p)
      (gethash label (match-counter-patterns-table counter))
    (when present-p
      (deletef (match-counter-patterns-vector counter) count)
      (remhash label (match-counter-patterns-table counter)))))

(defun remove-all-patterns (counter)
  "Removes all the patterns in the @code{method-counter} @var{counter}, and returns a
positive integer, the number of patterns so removed, if any, or @code{nil} if @var{counter}
had no patterns. Signals a @code{type-error} if @var{counter} is not a
@code{match-counter}."
  (let ((n (length (match-counter-patterns-vector counter))))
    (when (> n 0)
      (fill (match-counter-patterns-vector counter) nil) ; allow old stuff to be GCed
      (setf (fill-pointer (match-counter-patterns-vector counter)) 0)
      (clrhash (match-counter-patterns-table counter))
      n)))

(defun match-counter-pattern (counter label &optional (as-string t)
                                              (upper-case *print-bells-upper-case*))
  "Returns two values: the first is the pattern whose label in @var{count} is
@code{equalp} to @var{label}, if any, and otherwise @code{nil}; the second is a
generalized boolean if and only if the first value is non-nil and the pattern is to be
matched against two rows rather than just one. If the generalized boolean @var{as-string}
is true the pattern is returned as a string, as by @code{format-pattern}, with the case of
any bells represented by letters controled by the generalized boolean @var{upper-case};
and otherwise as a parse tree, as by @code{parse-pattern}. A string return value may not
be @code{string-equal} to that added to @var{counter}, but will match the same
@code{row}s. If @var{as-string} is not supplied it defaults to true; if @var{upper-case}
is not supplied it defaults to the current value of @code{*print-bells-upper-case*}.
Signals a @code{type-error} if @var{counter} is not a @code{match-counter}."
  (when-let* ((count (gethash label (match-counter-patterns-table counter)))
              (pattern (row-count-pattern count)))
    (values (if as-string
                (format-pattern pattern upper-case)
                pattern)
            (row-count-double-row-p count))))

(defun match-counter-labels (counter)
  "Returns two lists, the labels of those patterns in @var{count} that are matched against
a single row, and those that are matched against two rows. Both lists are in the order in
which the corresponding patterns were first added to @var{counter}. Signals a
@code{type-error} if @var{counter} is not a @code{match-counter}."
  (iter (for count :in-vector (match-counter-patterns-vector counter))
        (if (row-count-double-row-p count)
            (collect (row-count-label count) :into double)
            (collect (row-count-label count) :into single))
        (finally (return (values single double)))))

(defun %counts (count)
  (values (+ (row-count-handstroke count) (row-count-backstroke count))
          (row-count-handstroke count)
          (row-count-backstroke count)))

(defun match-counter-counts (counter &optional (label nil label-supplied-p))
  "Returns three values, the number of times the pattern with label @code{equalp} to
@var{label} in @var{counter} has matched @code{row}s presented to it with
@code{record-matches} since @var{counter} was reset or the relevent pattern was added to
it. The first return value is the total number of matches, the second the number of
matches at handstroke, and the third the number of matches at backstroke. If no
@var{label} is supplied it instead returns three a-lists mapping the labels of the
patterns in @var{counter} to the number of matches, again total, handstroke and
backstroke. The elements of these a-lists are in the order in which the corresponding
patterns were first added to @var{counter}. Returns @code{nil} if there is no pattern
labeled @var{label}. Signals a @code{type-error} if @var{counter} is not a
@code{match-counter}."
  (if label-supplied-p
      (when-let ((count (gethash label (match-counter-patterns-table counter))))
        (%counts count))
      (iter (for count :in-vector (match-counter-patterns-vector counter))
            (for label := (row-count-label count))
            (for (values total hand back) := (%counts count))
            (collect (cons label total) :into totals)
            (collect (cons label hand) :into hands)
            (collect (cons label back) :into backs)
            (finally (return (values totals hands backs))))))

(defun reset-match-counter (counter)
  "Resets all the counts associated with all the patterns in @var{counter} to zero.
Signals a @code{type-error} if @var{counter} is not a @code{match-counter}."
  (iter (for count :in-vector (match-counter-patterns-vector counter))
        (setf (row-count-handstroke count) 0)
        (setf (row-count-backstroke count) 0)))

(defun match-counter-handstroke-p (counter)
  "Returns a generalized boolean indicating that the next row presented to @var{counter}
will be a handstroke. Can be used with @code{setf} to tell @var{counter} whether or not it
should consider the next row a handstroke or a backstroke. If not explicitly set again,
either with @code{(setf match-counter-handstroke-p)}, or with the @var{handstroke-p}
argument to @code{record-matches}, whether or not subsequent rows will be considered
handstroke or backstroke will alternate. Signals a @code{type-error} if @var{counter} is
not a @code{match-counter}."
  (match-counter-%handstroke-p counter))

(defsetf match-counter-handstroke-p (counter) (stroke)
  `(setf (match-counter-%handstroke-p ,counter) (not (null ,stroke))))

(defun record-matches (counter row &optional following-row
                                     (handstroke-p nil handstroke-p-supplied))
  "Causes all the single-row patterns of @var{counter} to be matched against @var{row},
and, if a @var{following-row} is supplied and not @code{nil}, also all the double-row
patterns to be matched against both rows. If the generalized boolean @var{handstroke-p} is
supplied it indicates whether @var{row} is to be considered a handstroke or not, and,
unless explicitly set again, either with the @var{handstroke-p} argument to
@code{record-matches} by with @code{(setf match-counter-handstroke-p)}, whether or not
subsequent rows will be considered handroke or backstroke will alternate. That is,
supplying a @var{handtroke-p} argument to @code{record-matches} is equivalent to calling
@code{(setf match-counter-handstoke-p)} immediately before it. Signals a @code{type-error}
if @var{counter} is not a @code{match-counter}, @var{row} is not a @code{row}, or
@var{following-row} is neither a @code{row} nor @code{nil}."
  (when handstroke-p-supplied
    (setf (match-counter-handstroke-p counter) handstroke-p))
  (unless (match-counter-dfas counter)
    (make-match-counter-dfas counter))
  (when-let ((single (car (match-counter-dfas counter))))
    (dolist (count (%rows-match-p single row nil))
      (if (match-counter-%handstroke-p counter)
          (incf (row-count-handstroke count))
          (incf (row-count-backstroke count)))))
  (when-let ((double (and following-row (cdr (match-counter-dfas counter)))))
    (dolist (count (%rows-match-p double row following-row))
      (if (match-counter-%handstroke-p counter)
          (incf (row-count-backstroke count))
          (incf (row-count-handstroke count)))))
  (shiftf (match-counter-%handstroke-p counter)
          (not (match-counter-%handstroke-p counter))))

(defun make-match-counter-dfas (counter)
  (iter (with single := nil)
        (with double := nil)
        (for (nil count) :in-hashtable (match-counter-patterns-table counter))
        (for nfa := (make-nfa (row-count-pattern count) (match-counter-stage counter) count))
        (if (row-count-double-row-p count)
            (if double
                (add-transition double nfa)
                (setf double nfa))
            (if single
                (add-transition single nfa)
                (setf single nfa)))
        (finally (setf (match-counter-dfas counter)
                       (cons (and single (make-dfa single))
                             (and double (make-dfa double)))))))


;;; Patterns for named rows or named categories of rows

(defgeneric named-row-pattern (name &optional stage covered)
  (:documentation "Returns a pattern, as a parse tree, that matches a named row at
@var{stage}. The @var{name} is one of those listed below. If @var{stage} is not supplied
it defaults to the current value of @code{*default-stage*}. If @var{covered}, a
generalized boolean, is non-nil the @code{row}('s) that will be matched will assume an
implicit tenor. If @var{covered} is not supplied it defaults to @code{nil} for even stages
and @code{t} for odd stages. If there is no such named row known that corresponds to the
values of @var{stage} and @var{covered} @code{nil} is returned. Signals an error if
@var{name} is not a keyword or is not a known named row name as enumerated below, or if
@var{stage} is not a @code{stage}.

The supported values for @var{name}, and the stages at which they are defined, are:
@table @code
@item :backrounds
any stage
@item :queens
uncovered singles and above, or covered two and above.
@item :kings
uncovered minimus and above, or covered singles and above; note that kings at uncovered
minor or covered doubles is the same row as Whittingtons at those stages
@item :whittingtons
uncovered minor and above, or covered doubles and above; note that Whittingtons at
uncovered minor or covered doubles is the same row as kings at those stages
@item :double-whittingtons
covered cinques or uncovered maximus, only
@item :roller-coaster
covered caters or uncovered royal, only
@item :near-miss
any stage
@end table

@example
@group
 (format-pattern (named-row-pattern :whittingtons 10 nil))
     @result{} \"1234975680\"
 (format-pattern (named-row-pattern :whittingtons 9 t)
     @result{} \"123497568\"
 (format-pattern (named-row-pattern :whittingtons 9 nil))
     @result{} \"123864579\"
 (named-row-pattern :whittingtons 4)
     @result{} nil
@end group
@end example"))

(defmethod named-row-pattern ((name t) &optional stage covered)
  (declare (ignore stage covered))
  (typecase name
    (keyword (error "No named row is known called ~S." name))
    (t (error 'type-error :datum name :expected-type 'keyword))))

(defmacro def-named-row (name (&optional stage-var covered-var minimum-bells) &body body)
  (check-type* name keyword)
  (check-type* stage-var (and symbol (not keyword) (not (member t))))
  (check-type* covered-var (and symbol (not keyword) (not (member t))))
  (check-type* minimum-bells (or null stage))
  `(defmethod named-row-pattern ((#1=#:name (eql ,name))
                                 &optional
                                   (,(or stage-var '#2=#:stage) *default-stage*)
                                   (,(or covered-var '#3=#:covered)
                                     (oddp ,(or stage-var '#2#))))
     (declare (ignore #1#))
     (named-row-definition ,(or stage-var '#2#) ,(or covered-var '#3#) ,minimum-bells
                           #'(lambda (,(or stage-var '#2#) ,(or covered-var '#3#))
                               ,@(unless (and stage-var covered-var)
                                         `((declare (ignore ,@(unless stage-var '(#2#))
                                                            ,@(unless covered-var '(#3#))))))
                               (block ,name
                                 ,@body)))))

(defun named-row-definition (stage covered minimum-bells thunk)
  (check-type* stage stage)
  (and (or (null minimum-bells) (>= (if covered (1+ stage) stage) minimum-bells))
       (funcall thunk stage covered)))

(defun total-bells (stage covered)
  (if covered (1+ stage) stage))

(def-named-row :back-rounds (stage)
  `(:sequence ,@(iter (for i :from (- stage 1) :downto 0) (collect i))))

(def-named-row :queens (stage covered 3)
  (let ((start (if (xor covered (oddp stage)) 1 0)))
    (cons :sequence
          (nconc (iter (for i :from start :below stage :by 2)
                       (collect i))
                 (iter (for i :from (if (zerop start) 1 0) :below stage :by 2)
                       (collect i))))))

(def-named-row :kings (stage covered 4)
  (cons :sequence
        (nconc (iter (for i :from (- stage (if covered 1 2)) :downto 0 :by 2)
                     (collect i))
               (iter (for i :from (if (xor covered (evenp stage)) 1 0) :below stage
                          :by 2)
                     (collect i)))))

(def-named-row :whittingtons (stage covered 6)
  (iter (with limit := (if covered 5 6))
        (with n := (- stage limit))
        (for i :in '(4 2 0 1 3 5))
        (repeat limit)
        (collect (+ i n) :into result)
        (finally (return (cons :sequence
                               (iter (for j :from (- n 1) :downto 0)
                                     (push j result)
                                     (finally (return result))))))))

(def-named-row :double-whittingtons (stage covered)
  (case stage
    (11 (and covered (list :sequence 4 2 0 1 3 5 10 8 6 7 9)))
    (12 (and (not covered) (list :sequence 4 2 0 1 3 5 10 8 6 7 9 11)))))

(def-named-row :roller-coaster (stage covered)
  (case stage
    (9  (and covered (list :sequence 2 1 0 5 4 3 8 7 6)))
    (10 (and (not covered) (list :sequence 2 1 0 5 4 3 8 7 6 9)))))

(def-named-row :near-miss (stage)
  (labels ((one-near-miss (p)
             `(:sequence ,@(and (> p 0)
                                `((:run 0 ,(- p 1) ,p nil)))
                         ,(+ p 1)
                         ,p
                         ,@(and (< p (- stage 2))
                                `((:run ,(+ p 2) ,(- stage 1) ,(- stage p 2) nil))))))
    (cons :or (iter (for i :from 0 :below (- stage 1))
                    (collect (one-near-miss i))))))
