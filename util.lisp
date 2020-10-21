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


;;; check-type*

(defmacro check-type* (object type)
  ;; Similar to check-type, but object need not be a place, and if object is not of type
  ;; the error it signals is a non-continuable error; returns object if no error.
  (once-only (object)
    `(if (typep ,object ',type)
         ,object
         (error 'type-error :expected-type ',type :datum ,object))))


;;; Byte utilities

(defun equal-byte-specifiers (bs1 bs2)
  (and (eql (byte-size bs1) (byte-size bs2))
       (eql (byte-position bs1) (byte-position bs2))))

(defun byte-left (bs)
  (+ (byte-position bs) (byte-size bs)))


;;; Threading support

(defparameter *threading-p* (member :bordeaux-threads *features*))

(defun thread-call (name &rest args)
  (apply (symbol-function (find-symbol (string name) :bordeaux-threads)) args))

(defmacro deflock (name)
  (check-type* name symbol)
  `(defparameter ,name (when *threading-p*
                         (thread-call :make-lock ,(string-trim "*+" name)))))

(defmacro with-lock ((lock) &body body)
  `(%with-lock ,lock #'(lambda () ,@body)))

(defun %with-lock (lock thunk)
  (if *threading-p*
      (unwind-protect
           (progn
             (thread-call :acquire-lock lock t)
             (funcall thunk))
        (thread-call :release-lock lock))
      (funcall thunk)))

(defmacro define-thread-local (name &optional initial-value)
  ;; Note that initial-value will typically be evaluated multiple times, once in each
  ;; thread.
  (check-type name (and symbol (not keyword) (not (member nil t))))
  `(progn
     (defparameter ,name ,initial-value)
     (when *threading-p*
       (pushnew '(,name . ,initial-value)
                (symbol-value (find-symbol "*DEFAULT-SPECIAL-BINDINGS*" :bordeaux-threads))
                :key #'car))))


;;; Least recently used cache

(defclass cache-entry ()
  ((key :accessor entry-key)
   (value :accessor entry-value)
   (previous :type (or cache-entry null) :initform nil :accessor previous-entry)
   (next :type (or cache-entry null) :initform nil :accessor next-entry))
  (:documentation "The elements of a doubly-linked list that are threaded through the hash
table entries for this @code{lru-cache}. Elements in the list are maintained in a newest
to oldest order."))

(defmethod print-object ((entry cache-entry) stream)
  (print-unreadable-object (entry stream :type t :identity t)
    (prin1 (slot-value entry 'key) stream)))

(defclass lru-cache ()
  ((size :type (integer 1 *) :initarg :size :reader cache-size)
   (table :type hash :initarg :table :reader cache-entry-table)
   (newest :type (or cache-entry null) :initform nil :accessor newest-entry)
   (oldest :type (or cache-entry null) :initform nil :accessor oldest-entry))
  (:documentation "A least recent used cache implemented as a threaded hash table."))

(defun make-lru-cache (size &key (test #'eql))
  "Creates an @code{lru-cache} of the given @var{size}, which should be a positive integer.
The @var{:test} argument should be suitable for passing as the @var{:test} of
@code{make-hash-table}, and is the equality comparison used to compare keys. If during use
of the @code{lru-cache} more then @var{size} elements are added, the oldest are deleted to
ensure the cache contains at most @var{size} elements. Adding or retrieving an element
marks it as the newest, and ages any others in the cache in turn, keeping their current
age order."
  (check-type* size (integer 1 *))
  (make-instance 'lru-cache :size size :table (make-hash-table :size size :test test)))

(defun cache-count (cache)
  "Returns the number of elements currently in the @code{lru-cache} @var{cache}."
  (hash-table-count (cache-entry-table cache)))

(defmethod print-object ((cache lru-cache) stream)
  (print-unreadable-object (cache stream :type t :identity t)
    (format stream "~D/~D" (cache-count cache) (cache-size cache))))

(defun add-entry (entry cache)
  ;; Adds ENTRY as the newest in CACHE.
  (with-slots (newest oldest) cache
    (when newest
      (setf (previous-entry newest) entry))
    (shiftf (next-entry entry) newest entry)
    (unless oldest
      (setf oldest entry)))
  entry)

(defun remove-entry (entry cache)
  (with-slots (previous next) entry
    (if previous
      (setf (next-entry previous) next)
      (setf (newest-entry cache) next))
    (if next
      (setf (previous-entry next) previous)
      (setf (oldest-entry cache) previous)))
  entry)

(defun move-entry (entry cache)
  ;; Moves the ENTRY to be the newest in CACHE.
  (add-entry (remove-entry entry cache) cache))

(defmacro get-entry (key cache)
  `(gethash ,key (cache-entry-table ,cache) nil))

(defun getcache (key cache &optional (default nil))
  "Retrieves a value from @var{cache}. @var{key} may be any object. @var{cache} should be
an @code{lru-cache}. Returns two values. The first is the element in the cache with the
given @var{key}, or @var{default} if it is not present in @var{cache}. The second is
@code{t} if an element was found, and @code{nil} otherwise. Retrieving an element from the
cache marks it as the newest, aging the others in turn. @code{getcache} can be used with
@code{setf}."
  (if-let (entry (get-entry key cache))
    (values (entry-value (move-entry entry cache)) t)
    (values default nil)))

(defun get-newest-key (cache &optional default)
  "Retrieves the key of the most recently accesed entry in @var{cache}, which should be an
@code{lru-cache}. @var{default} can be any object. Returns two values. If there are any
entries in @var{cache}, the key associated with the most recently accessed entry, and
@code{t}, are returned. If @var{cache} is empty, @var{default} and @code{nil} are
returned."
  (if-let (entry (newest-entry cache))
    (values (entry-key entry) t)
    (values default nil)))

(defun get-newest-value (cache &optional default)
  "Retrieves the value of the most recently accesed entry in @var{cache}, which should be
an @code{lru-cache}. @var{default} can be any object. Returns two values. If there are any
entries in @var{cache}, the value associated with the most recently accessed entry, and
@code{t}, are returned. If @var{cache{ is empty, @var{default} and @code{nil} are
returned."
  (if-let (entry (newest-entry cache))
    (values (entry-value entry) t)
    (values default nil)))

(defun putcache (key cache value)
  (let ((entry (get-entry key cache)))
    (cond (entry
           (unless (eq entry (newest-entry cache))
             (move-entry entry cache)))
          (t
           (cond ((>= (cache-count cache) (cache-size cache))
                  ;; It should not be possible to be greater than, but use the paranoid
                  ;; test, just in case.
                  (setf entry (move-entry (oldest-entry cache) cache)) ; recycle it
                  (remhash (entry-key entry) (cache-entry-table cache)))
                 (t
                  (setf entry (add-entry (make-instance 'cache-entry) cache))))
           (setf (entry-key entry) key
                 (get-entry key cache) entry)))
    (setf (entry-value entry) value)))

(defsetf getcache (key cache &optional default) (value)
  (declare (ignorable default))
  `(progn (putcache ,key ,cache ,value)))

(defun remcache (key cache)
  "Removes the entry with key @var{key} from the @code{lru-cache} @var{cache}.
Returns @code{t} if there was such an element, and @code{nil} otherwise."
  (let ((entry (get-entry key cache)))
    (cond (entry (remove-entry entry cache)
                 (remhash key (cache-entry-table cache)))
          (t nil))))

(defun clrcache (cache)
  "Removes all the entries from the lru-cache CACHE. Returns CACHE, which is now empty."
  (clrhash (cache-entry-table cache))
  (setf (newest-entry cache) nil
        (oldest-entry cache) nil)
  cache)


;;; Equalp hash sets

;; Hash-sets are structures rather than instances of a subclass of standard-class so
;; we can compare them with equalp.
#-(or Lispworks6 Lispworks7) (declaim (inline hash-set-table))
(defstruct (hash-set
             (:constructor %make-hash-set (table))
             (:copier nil)
             (:print-object prin1-hash-set))
  "===summary===
@cindex sets
@cindex @code{equalp}
For change ringing applications it is often useful to manipulate sets of rows. That is,
unordered collections of rows without duplicates. To support this and similar uses Roan
supplies @code{hash-set}s, which use @code{equalp} as the comparison for whether or not
two candidate elements are ``the same''. In addition, @code{equalp} can be used to compare
two @code{hash-set}s themselves for equality: they are @code{equalp} if they contain the
same number of elements, and each of the elements of one is @code{equalp} to an element of
the other.
@example
@group
 (equalp (hash-set !12345678 !13572468 !12753468 !13572468)
         (hash-set-union (hash-set !12753468 !12345678)
                         (hash-set !13572468 !12753468 !13572468)))
     @result{} t
@end group
@end example
===endsummary===
A set data structure, with element equality determined by @code{equalp}. That is, no two
elements of such a set will ever be @code{equalp}, only one of those added remaining
present in the set. Set membership testing, adding new elements to the set, and deletion
of elements from the set is, on average, constant time. Two @code{hash-set}s can be
compared with @code{equalp}: they are considered @code{equalp} if and only if they contain
the same number of elements, and each of the elements of one is @code{equalp} to an
element of the other."
  (table (make-hash-table :test #'equalp) :type hash-table :read-only t))

(declaim (inline checked-hash-set-table))
(defun checked-hash-set-table (set)
  (check-type* set hash-set)
  (hash-set-table set))

(defun make-hash-set (&rest keys &key size rehash-size rehash-threshold initial-elements)
  "===lambda: (&key size rehash-size rehash-threshold initial-elements)
Returns a new @code{hash-set}. If @var{initial-elements} is supplied and non-nil,
it must be a list of elements that the return value will contain; otherwise an empty set
is returned. If any of @var{size}, @var{rehash-size} or @var{rehash-threshold} are
supplied they have meanings analagous to the eponymous arguments to
@code{make-hash-table}."
  (declare (ignore size rehash-size rehash-threshold))
  (apply #'hash-set-nadjoin
         (%make-hash-set (apply #'make-hash-table :test #'equalp :allow-other-keys t keys))
         initial-elements))

(defun hash-set (&rest initial-elements)
  "Returns a new @code{hash-set} containing the elements of @var{initial-elements}. If no
@var{initial-elements} are supplied, the returned @code{hash-set} is empty.
@example
@group
 (hash-set 1 :foo 2 :foo 1) @result{} #<HASH-SET 3>
 (hash-set-elements (hash-set 1 :foo 2 :foo 1))
     @result{} (1 2 :foo)
 (hash-set-elements (hash-set)) @result{} nil
@end group
@end example"
  (make-hash-set :initial-elements initial-elements))

(defun hash-set-copy (set &rest keys &key size rehash-size rehash-threshold)
  "===lambda: (set &key size rehash-size rehash-threshold)
Returns a new @code{hash-set} containing the same elements as the @code{hash-set}
@var{set}. If any of @var{size}, @var{rehash-size} or @var{rehash-threshold} are supplied
they have the same meanings as the eponymous arguments to @code{copy-hash-table}. A
@code{type-error} is signaled if @var{set} is not a @code{hash-set}."
  (declare (ignore size rehash-size rehash-threshold))
  (%make-hash-set (apply #'copy-hash-table (checked-hash-set-table set) keys)))

(declaim (inline hash-set-count))
(defun hash-set-count (set)
  "Returns a non-negative integer, the number of elements the @code{hash-set} @var{set}
contains. Signals a @code{type-error} if @var{set} is not a @code{hash-set}.
@example
@group
 (hash-set-count (hash-set !1234 !1342 !1234)) @result{} 2
 (hash-set-count (hash-set)) @result{} 0
@end group
@end example"
  (hash-table-count (checked-hash-set-table set)))

(defun prin1-hash-set (set stream)
  (if *print-readably*
      (prin1 (cons 'hash-set (hash-set-elements set)) stream)
      (print-unreadable-object (set stream :type t :identity t)
        (princ (hash-set-count set) stream))))

(declaim (inline hash-set-empty-p))
(defun hash-set-empty-p (set)
  "True if and only if the @code{hash-set} @var{set} contains no elements. Signals a
@code{type-error} if @var{set} is not a @code{hash-set}."
  (zerop (hash-set-count set)))

(defun hash-set-elements (set)
  "Returns a list of all the elements of the @code{hash-set} @var{set}. The order of the
elements in the list is undefined, and may vary between two invocations of
@code{hash-set-elements}. Signals a @code{type-error} if @var{set} is not a
@code{hash-set}.
@example
 (hash-set-elements (hash-set 1 2 1 3 1)) @result{} (3 2 1)
@end example"
  (hash-table-keys (checked-hash-set-table set)))

(declaim (inline hash-set-member))
(defun hash-set-member (item set)
  "True if and only if @var{item} is an element of the @code{hash-set} @var{set}.
Signals a @code{type-error} if @var{set} is not a @code{hash-set}.
@example
@group
 (hash-set-member !1342 (hash-set !1243 !1342)) @result{} t
 (hash-set-member !1342 (hash-set !12435 !12425)) @result{} nil
@end group
@end example"
  (values (gethash item (checked-hash-set-table set) nil)))

(defun hash-set-subset-p (subset superset)
  "The @code{hash-set-subset-p} predicate is true if and only if all elements of
@var{subset} occur in @var{superset}. The @code{hash-set-proper-subset-p} predicate is
true if and only that is the case and further that @code{subset} does not contain all the
elements of @var{superset}. @code{type-error} is signaled if either argument is not a
@code{hash-set}.
@example
@group
 (hash-set-subset-p (hash-set 1) (hash-set 2 1) @result{} t
 (hash-set-proper-subset-p (hash-set 1) (hash-set 2 1) @result{} t
 (hash-set-subset-p (hash-set 1 2) (hash-set 2 1) @result{} t
 (hash-set-proper-subset-p (hash-set 1 2) (hash-set 2 1) @result{} nil
 (hash-set-subset-p (hash-set 1 3) (hash-set 2 1) @result{} nil
 (hash-set-proper-subset-p (hash-set 1 3) (hash-set 2 1) @result{} nil
@end group
@end example"
  (let ((subtable (checked-hash-set-table subset))
        (supertable (checked-hash-set-table superset)))
    (and (<= (hash-table-count subtable) (hash-table-count supertable))
         (iter (for (e nil) :in-hashtable subtable)
               (always (gethash e supertable nil))))))

(defun hash-set-proper-subset-p (subset superset)
  "===merge: hash-set-subset-p"
  (and (< (hash-set-count subset) (hash-set-count superset))
       (hash-set-subset-p subset superset)))

(defun hash-set-clear (set)
  "Removes all elements from @var{set}, and then returns the now empty @code{hash-set}.
Signals a @code{type-error} if @var{set} is not a @code{hash-set}."
  (clrhash (checked-hash-set-table set))
  set)

(defun hash-set-nadjoin-list-elements (set list)
  "===merge: hash-set-adjoin 3"
  (let ((table (checked-hash-set-table set)))
    (dolist (e list)
      (setf (gethash e table) t)))
  set)

(defun hash-set-adjoin-list-elements (set list)
  "===merge: hash-set-adjoin 1"
  (hash-set-nadjoin-list-elements (hash-set-copy set) list))

(defun hash-set-nadjoin (set &rest elements)
  "===merge: hash-set-adjoin 2"
  (hash-set-nadjoin-list-elements set elements))

(defun hash-set-adjoin (set &rest elements)
  "Returns a @code{hash-set} that contains all the elements of @var{set} to which have
been added the @var{elements}, or the elements of the @var{list}. As usual duplicate
elements are not added, though exactly which of any potential duplicates are retained is
undefined. The @code{hash-set-adjoin} and @code{hash-set-adjoin-list-elements} functions
do not modify @var{set} but might return it if no changes are needed; that is, the caller
cannot depend upon it necessarily being a fresh copy. The @code{hash-set-nadjoin} and
@code{hash-set-nadjoin-list-elements} functions modify @var{set} (if one or more of the
elements is not already contained therein) and return it. Note that
@code{hash-set-[n]adjoin-list-elements} differs from @code{(apply #'hash-set-[n]adjoin
...)} in that the latter can adjoin at most @code{call-arguments-limit} elements. Signals
a @code{type-error} if @var{set} is not a @code{hash-set}.
@example
@group
 (hash-set-elements (hash-set-adjoin (hash-set 1 2 3) 4 3 2))
     @result{} (3 4 1 2)
@end group
@end example"
  (hash-set-nadjoin-list-elements (hash-set-copy set) elements))

(defun hash-set-delete (set &rest elements)
  "Deletes from the @code{hash-set} @var{set} all elements @code{equalp} to elements of
@var{elements}, and returns the modified set. Signals a @code{type-error} if @var{set} is
not a @code{hash-set}."
  (iter (with table := (checked-hash-set-table set))
        (with deleted := 0)
        (for e :in elements)
        (when (remhash e table)
          (incf deleted))
        (finally (return (values set (if (zerop deleted) nil deleted))))))

(defun hash-set-remove (set &rest elements)
  "Returns a new @code{hash-set} that contains all the elements of @var{set} that are not
@code{equalp} to any of the @var{elements}. Signals a @code{type-error} if @var{set} is
not a @code{hash-set}."
  (apply #'hash-set-delete (hash-set-copy set) elements))

(defun hash-set-ndifference (set &rest more-sets)
  "===merge: hash-set-difference"
  (let ((table (checked-hash-set-table set)))
    (dolist (s more-sets)
      (maphash-keys #'(lambda (k) (remhash k table)) (checked-hash-set-table s))))
  set)

(defun hash-set-difference (set &rest more-sets)
  "Returns a @code{hash-set} containing all the elements of @var{set} that are not
contained in any of @var{more-sets}. The @code{hash-set-difference} version returns a
fresh @code{hash-set}, and does not modify @var{set} or any of the @var{more-sets}. The
@code{hash-set-ndifference} version modifies and returns @var{set}, but does not modify
any of @var{more-sets}. Signals a @code{type-error} if @var{set} or any of @var{more-sets}
are not @code{hash-set}s.
@example
@group
 (hash-set-elements
   (hash-set-difference
     (hash-set !12345 !23451 !34512 !45123)
     (hash-set !23451 !54321 !12345)))
     @result{} (!34512 !45123)
@end group
@end example"
  (apply #'hash-set-ndifference (hash-set-copy set) more-sets))

(defun %extreme-hash-set (set more-sets test)
  ;; SET is a hash-set and MORE-SETS is a list of hash-sets. Returns two values, the first
  ;; the first element of (APPEND SET MORE-SETS) that contains the most/fewest elements,
  ;; according to the function TEST, and the second that same appended list with that
  ;; etreme element spliced out of it. Duplicate (eq) sets are weeded out before looking
  ;; for the extremum.
  (iter (with extremum := set)
        (with extreme-count := (hash-set-count extremum))
        (with rest := ())
        (for s :in more-sets)
        (for c := (hash-set-count s))
        (unless (or (eq s extremum) (member s rest :test #'eq))
          (push (cond ((funcall test c extreme-count)
                       (setf extreme-count c)
                       (shiftf extremum s))
                      (t s))
                rest))
        (finally (return (values extremum rest)))))

(defun %hash-set-nunion (set more-sets)
  (iter (with table := (hash-set-table set))
        (for s :in more-sets)
        (maphash-keys (lambda (k) (setf (gethash k table) t)) (hash-set-table s)))
  set)

(defun hash-set-nunion (set &rest more-sets)
  "===merge: hash-set-union"
  (cond (more-sets
         (dolist (s more-sets)
           (check-type* s hash-set))
         (%hash-set-nunion set more-sets))
        (t (check-type* set hash-set))))

(defun hash-set-union (set &rest more-sets)
  "Returns a @code{hash-set} containing all the elements that appear in @var{set} or in
any of the @var{more-sets}. The @code{hash-set-union} function does not modify @var{set}
or any of the @var{more-sets}, but may return any one of them unmodified if appropriate;
the caller should not assume a fresh @code{hash-set} is returned. The
@code{hash-set-nunion} function always returns @var{set}, modifying it if necessary; it
does not modify any of the @var{more-sets}. Signals a @code{type-error} if @var{set} or
any of the @var{more-sets} are not @code{hash-set}s.
@example
@group
 (coerce
   (hash-set-elements
     (hash-set-union
       (apply #'hash-set (coerce \"abcdef\" 'list))
       (apply #'hash-set (coerce \"ACEG\" 'list))))
   'string)
     @result{} \"FaeGbcd\"
 (hash-set-empty-p (hash-set-union)) @result{} t
@end group
@end example"
  (cond ((null more-sets)
         (check-type* set hash-set))
        ((rest more-sets)
         (multiple-value-bind (largest rest) (%extreme-hash-set set more-sets #'>)
           (%hash-set-nunion (hash-set-copy largest) rest)))
        ((> (hash-set-count (first more-sets)) (hash-set-count set))
         (%hash-set-nunion (hash-set-copy (first more-sets)) (list set)))
        (t (%hash-set-nunion (hash-set-copy set) more-sets))))

(defun %hash-set-nintersection (set more-sets)
  (let ((table (hash-set-table set))
        (more-tables (mapl #'(lambda (sublist)
                               (setf (first sublist) (hash-set-table (first sublist))))
                           more-sets)))
    (maphash-keys (lambda (k)
                    (unless (every (lambda (tab) (gethash k tab)) more-tables)
                      (remhash k table)))
                  table))
  set)

(defun hash-set-nintersection (set &rest more-sets)
  "===merge: hash-set-intersection"
  (cond (more-sets
         (dolist (s more-sets)
           (check-type* s hash-set))
         (%hash-set-nintersection set more-sets))
        (t (check-type* set hash-set))))

(defun hash-set-intersection (set &rest more-sets)
  "Returns a @code{hash-set} such at all of its elements are also elements of @var{set}
and of all the @var{more-sets}. The @code{hash-set-intersection} function does not modify
@var{set} or any of the @var{more-sets}, but may return any one of them unmodified if
appropriate; the caller should not assume a fresh @code{hash-set} is returned. The
@code{hash-set-nintersection} function always returns @var{set}, modifying it if
necessary; it does not modify any of the @var{more-sets}. Signals a @code{type-error} if
@var{set} or any of the @var{more-sets} are not @code{hash-set}s.
@example
@group
 (coerce
   (hash-set-elements
     (hash-set-intersection
       (apply #'hash-set (coerce \"abcdef\" 'list))
       (apply #'hash-set (coerce \"ACEG\" 'list))))
   'string)
     @result{} \"EaC\"
@end group
@end example"
  (cond ((null more-sets)
         (check-type* set hash-set))
        ((rest more-sets)
         (multiple-value-bind (smallest rest) (%extreme-hash-set set more-sets #'<)
           (%hash-set-nintersection (hash-set-copy smallest) rest)))
        ((< (hash-set-count (first more-sets)) (hash-set-count set))
         (%hash-set-nintersection (hash-set-copy (first more-sets)) (list set)))
        (t (%hash-set-nintersection (hash-set-copy set) more-sets))))

(defun map-hash-set (function set)
  "Calls @var{function} on each element of the @code{hash-set} @var{set}, and returns
@code{nil}. The order in which the elements of @var{set} have @var{function} applied to
them is undefined. With one exception, the behavior is undefined if @var{function}
attempts to modify the contents of @var{set}: @var{function} may call
@code{hash-set-delete} to delete the current element, but no other. A @code{type-error} is
signaled if @var{set} is not a @code{hash-set}.
@example
@group
 (let ((r nil))
   (map-hash-set #'(lambda (e)
                     (push (list e (in-course-p e)) r))
                 (hash-set !135246 !123456 !531246))
   r)
     @result{} ((!135246 nil) (!531246 nil) (!123456 t))
@end group
@end example"
  (maphash-keys function (checked-hash-set-table set)))

(defmacro do-hash-set ((var set &optional result-form) &body body)
  "===lambda: ((var set &optional result-form) &body body)
Evaluates the @var{body}, an implicit @code{progn}, repeatedly with the symbol
@code{var} bound to the elements of the @code{hash-set} @var{set}. Returns the result of
evaluating @var{result-form}, which defaults to @code{nil}, after the last iteration. A
value may be returned by using @code{return} or @code{return-from nil}, in which case
@var{result-form} is not evaluated. The order in which the elements of @var{set} are bound
to @code{var} for evaluating @var{body} is undefined. With one exception the behavior is
undefined if @var{body} attempts to modify the contents of @var{set}: @var{function} may
call @code{hash-set-delete} to delete the current element, but no other. A
@code{type-error} is signaled if @var{set} is not a @code{hash-set}.
@example
@group
 (let ((r nil))
   (do-hash-set (e (hash-set !135246 !123456 !531246) r)
     (push (list e (in-course-p e) r))))
     @result{} ((!531246 nil) (!123456 t) (!135246 nil))
@end group
@end example"
  (check-type* var (and symbol (not keyword) (not (member nil t))))
  `(block nil
     (map-hash-set #'(lambda (,var) ,@body) ,set)
     ,result-form))

(defmacro-clause (for var in-hash-set set)
  "Binds @var{var} to elements of the @code{hash-set} @var{set}. The order in which the
elements are iterated over is undefined, and may vary between invocations of this clause
on the same @code{hash-set}. If @var{set} is not a @code{hash-set} a @code{type-error} is
signaled."
  `(for (,var nil) in-hashtable (checked-hash-set-table ,set)))

(defun hash-set-pop (set &optional (error-p t) empty-value)
  "Deletes an element from @var{set} and returns it. The particular element chosen to be
removed and returned is undefined. If @var{set} is empty returns @var{empty-value} if the
generalized Boolean @var{error-p} is false and otherwise signals an error. By default
@var{error-p} is true and @var{empty-value} is @code{nil}. Signals a @code{type-error} if
@var{set} is not a @code{hash-set}."
  (cond ((not (hash-set-empty-p set))
         (let ((result (do-hash-set (e set) (return e))))
           (hash-set-delete set result)
           result))
        (error-p (error "Attempt to pop an element from the empty hash-set ~S" set))
        (t empty-value)))


;;; with-warnings-muffled

(defmacro with-warnings-muffled ((&key (condition 'warning) (if t)) &body body)
  "Evaluates BODY in a dynamic environment such that nothing is printed for warnings
signaled. Only warnings of type CONDITION, which is not evaluated, are muffled; and only
if IF, a genearlized Boolean, which is evaluated once, before BODY is evaluated, and
outside the dynamic envirnoment muffling warnings, is non-nil. Returns the values of the
last form in BODY."
  `(let ((#0=#:if ,if))
     (handler-bind ((,condition #'(lambda (c) (when #0# (muffle-warning c)))))
       ,@body)))


;;; collapse-whitespace

(defparameter *contains-collapsable-whitespace-scanner*
  (ppcre:create-scanner "  |\\t|^ | $"))

(defparameter *collapsable-whitespace-scanner*
  (ppcre:create-scanner "(:?  | *\\t *)+"))

(defun collapse-whitespace (s)
  "Collapses multiple space characters and one or more tabs to single spaces, and removes
leading or trailing spaces and tabs. If @var{s} contains any such whitespace that needs to
be collapsed returns a copy suitably corrected, and otherwise returns S unchanged. If
@var{s} is not a string or @code{nil}, signals an @code{type-error}."
  (etypecase s
    (string (if (ppcre:scan *contains-collapsable-whitespace-scanner* s)
                (values (ppcre:regex-replace-all *collapsable-whitespace-scanner*
                                                 (string-trim '(#\Space #\Tab)  s)
                                                 " "))
                s))
    (null nil)))


;;; same-type-p

(defun same-type-p (x y)
  (and (typep x (type-of y)) (typep y (type-of x))))


;;; with-utf-8-file

(defparameter +utf-8-external-format+ (uiop:encoding-external-format :utf-8))

(defmacro with-roan-file ((stream filespec &rest options
                                  &key (external-format '+utf-8-external-format+)
                                  &allow-other-keys)
                          &body body)
  `(with-open-file (,stream ,filespec :external-format ,external-format ,@options)
     ,@body))
