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

(in-readtable :roan+interpol)


;;; Methods

;;; Don't confuse change ringing methods and classes with CLOS methods and classes, they
;;; are completely different animals. The ringing package shadows the symbols method,
;;; method-name, class and class-name.

;; The classification slot of a method encodes its stage and classification.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +stage-field+ (byte (ceiling (log +maximum-stage+ 2)) 0)
    :test #'equal-byte-specifiers)
  (define-constant +classes+
      ;; important that nil comes first, so it's zero
      #(nil :surprise :delight :treble-bob :bob :place :alliance :treble-place :hybrid :hunt)
    :test #'equalp)
  (define-constant +class-names+
      (iter (for k :in-vector +classes+)
            (when k
              (collect (cons k (string-capitalize (substitute #\Space #\- (string k)))))))
    :test #'equalp)
  (define-constant +class-field+
      (byte (ceiling (log (length +classes+) 2)) (byte-size +stage-field+))
    :test #'equal-byte-specifiers)
  (define-constant +little-field+ (byte 1 (byte-left +class-field+))
    :test #'equal-byte-specifiers)
  (define-constant +differential-field+ (byte 1 (byte-left +little-field+))
    :test #'equal-byte-specifiers)
  (define-constant +jump-field+ (byte 1 (byte-left +differential-field+))
    :test #'equal-byte-specifiers)
  (deftype encoded-classification ()
    `(integer 0 ,(dpb -1 (byte (1+ (byte-position roan::+jump-field+)) 0) 0))))

(defclass method ()
  ((name :type (or string null) :initform nil :accessor method-name :initarg :name)
   (classification :type encoded-classification :initform 0
                   :accessor %method-classification :initarg :classification)
   (place-notation :type (or string null) :initform nil
                   :accessor method-place-notation :initarg :place-notation))
  (:documentation "===summary===
Roan provides the @code{method} type to describe change ringing methods, not to be
confused with CLOS methods. A @code{method} can only describe what the Central Council of
Church Bell Ringers @url{https://cccbr.github.io/method_ringing_framework/, Framework for
Method Ringing} (FMR) calls a static method, a method that can be viewed as a fixed
sequence of changes, including jump changes; while this includes nearly all methods rung
and named to date, it does exclude, for example, Dixonoids. A @code{method} has a name, a
stage, classifacation details, and an associated place-notation, though any or all of
these may be @code{nil}. In the case of the stage or place notation @code{nil} indicates
that the corresponding value is not known; the same is also true if the name is
@code{nil}, except for the case of Little Bob, which in the taxonomy of the FMR has no
name. The stage, if known, should be a @code{stage}, and the name and place notation, if
known, should be strings.

The classification follows the taxonomy in the FMR and consists of a @code{class} and
three boolean attributes for jump methods, differential methods and little methods. The
@code{class} may be @code{nil}, for principles and pure differentials; one of the keywords
@code{:bob}, @code{:place}, @code{:surprise}, @code{:delight}, @code{:treble-bob},
@code{:alliance}, @code{:treble-place} or@code{:hybrid}, naming the corresponding class;
or @code{:hunt} indicating a method with one or more hunt bells that does not fall into
any of the named classes, which can only apply to jump methods. The classification
consists merely of details stored in the @code{method} object, and does not necessary
correspond to the actual classification of the method described by the
@code{place-notation}, if supplied. The classification can be set to match the place
notation by calling @code{classify-method}.

Similarly the name does not necessarily correspond to the name by which the place notation
is known, unless the @code{method} has been looked up from a suitable library.
@xref{Methods library}.

Because ringing methods and their classes are unrelated to CLOS methods and classes, the
@code{roan} package shadows the symbols @code{common-lisp:method},
@code{common-lisp:method-name} and @code{common-lisp:class-name}.
===endsummary===
Describes a change ringing method, typically including its name, stage, classification and
place notation."))

(defun copy-method (method)
  "Returns a new @code{method} whose name and place notation are @code{equal} to those
of @var{method}, and with the same classification as @var{method}. Signals a
@code{type-error} if @var{method} is not a @code{method}."
  (make-instance 'method
                 :name (and (method-name method)
                            (copy-sequence 'string (method-name method)))
                 :classification (%method-classification method)
                 :place-notation (and (method-place-notation method)
                                      (copy-sequence 'string (method-place-notation method)))))

(defmethod %method-classification ((method t))
  ;; Signal a less inscrutable error when our callers are misused than would otherwise
  ;; be the case.
  (error 'type-error :expected-type 'method :datum method))

(defmethod method-name ((method t))
  ;; Signal a less inscrutable error when we are misused than would otherwise be the case.
  (error 'type-error :expected-type 'method :datum method))

(defmethod (setf method-name) :before (value (method method))
  (check-type* value (or null string)))

(defun method-stage (method)
  (let ((result (mask-field +stage-field+ (%method-classification method))))
    (if (zerop result) nil result)))

(defun set-method-stage (method stage)
  (unless (eql stage (mask-field +stage-field+ (%method-classification method)))
    (check-type* stage (or stage null))
    (clear-method-traits method)
    (setf (mask-field +stage-field+ (%method-classification method)) (or stage 0))))

(defsetf method-stage set-method-stage)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-method-boolean ((method-var value-var) (name field) &body extra-actions)
    (let ((setter (symbolicate "SET-" name)))
      `(let ((bit (dpb -1 ,field 0)))
         (defun ,name (,method-var)
           (not (zerop (mask-field ,field (%method-classification ,method-var)))))
         (defun ,setter (,method-var ,value-var)
           (prog1
               (not (zerop (setf (mask-field ,field (%method-classification ,method-var))
                                 (if ,value-var bit 0))))
             ,@extra-actions))
         (defsetf ,name ,setter))))
  (defsetf method-class set-method-class)
  (define-method-boolean (method value) (method-little-p +little-field+)
    (when value
      (if (method-jump-p method)
          (unless (eq (method-class method) :hunt)
            (setf (method-class method) :hunt))
          (when (null (method-class method))
            (setf (method-class method) :hybrid)))))
  (define-method-boolean (method value) (method-differential-p +differential-field+))
  (define-method-boolean (method value) (method-jump-p +jump-field+)
    (if value
        (when (not (member (method-class method) '(nil :hunt)))
          (setf (method-class method) :hunt))
        (when (not (member (method-class method) '(nil :hybrid)))
          (setf (method-class method) :hybrid)))))

(defun method-class (method)
  (svref +classes+ (ldb +class-field+ (%method-classification method))))

(defun set-method-class (method class)
  (let ((c (position class +classes+)))
    (unless c
      (error 'type-error :expected-type 'class :datum class))
    (setf (ldb +class-field+ (%method-classification method)) c))
  (cond ((eq class :hunt)
         (unless (method-jump-p method)
           (setf (method-jump-p method) t)))
        ((null class)
         (when (method-little-p method)
           (setf (method-little-p method) nil)))
        (t (when (method-jump-p method)
             (setf (method-jump-p method) nil))))
  class)

(defun class-name (class)
  (cdr (assoc class +class-names+)))

(defun class-from-name (string)
  (and string (typep string 'string) (not (equalp string "nil"))
       (or (find string +classes+ :test #'string-equal)
           (car (rassoc string +class-names+ :test #'string-equal)))))

(defun method-details (method)
  (values (method-name method) (method-jump-p method) (method-differential-p method)
          (method-little-p method) (method-class method) (method-stage method)))

(defmethod (setf method-place-notation) :before (value (method method))
  (unless (equalp value (method-place-notation method))
    (check-type* value (or null string))
    (clear-method-traits method)))

(defmethod method-place-notation ((method t))
  ;; Signal a less inscrutable error if method-place-notation is misused.
  (error 'type-error :expected-type 'method :datum method))

(define-condition inconsistent-method-specification-error (simple-error)
  ((method :reader no-place-notation-error-method :initarg :method))
  (:documentation "Signaled in circumstances when the various classification details
provided cannot occur together, such as a little principle."))

(defun method (&key name jump differential little class (stage nil stage-supplied)
                 place-notation)
  "Creates a new @code{method} instance, with the specified @var{name}, @var{stage},
@var{classification} and @var{place-notation}.
If @var{stage} is not provided, it defaults to
the current value of @code{*default-stage*}; to create a @code{method} with no stage
@code{:stage nil} must be explicitly supplied.

A @code{type-error} is signaled if @var{stage} is supplied and is neither @code{nil} nor a
@code{stage}; if either of @var{name} or @var{place-notation} are supplied and are neither
@code{nil} nor a string; or if @code{class} is supplied and is neither @code{nil} nor one
of the keywords @code{:bob}, @code{:place}, @code{:surprise}, @code{:delight},
@code{:treble-bob}, @code{:alliance}, @code{:treble-place} or @code{:hybrid}. A
@code{inconsistent-method-specification-error} is signaled if the various classification
details cannot occur together, such as a little principle."
  (check-type* name (or string null))
  (check-type* place-notation (or string null))
  (cond (stage (check-type* stage stage))
        (stage-supplied (setf stage 0))
        (t (setf stage *default-stage*)))
  (cond (little (unless class
                  (error 'inconsistent-method-specification-error
                         :format-control "A principle cannot be a little method")))
        (jump (unless (member class '(nil :hunt))
                (error 'inconsistent-method-specification-error
                       :format-control "A jump method may not have class ~S"
                       :format-arguments (list class))))
        (t (when (eq class :hunt)
             (error 'inconsistent-method-specification-error
                    :format-control "The :hunt pseudo-class is reserved for jump methods"))))
  (let ((result (make-instance 'method
                               :name name
                               :classification stage
                               :place-notation place-notation)))
    (when class (setf (method-class result) class))
    (when jump (setf (method-jump-p result) t))
    (when differential (setf (method-differential-p result) t))
    (when little (setf (method-little-p result) t))
    result))

(define-constant +suppress-class+
    '("Grandsire" "Double Grandsire" "Reverse Grandsire" "Little Grandsire"
      "Union" "Double Union" "Reverse Union" "Little Union")
  :test #'equalp)

(defun method-title (method &optional show-unknown)
  "Returns a string containing as much of the @var{method}'s title as is known. If
@var{show-unknown}, a generalized boolean defaulting to false, is not true then an unknown
name is described as \"Unknown\", and otherwise is simply omitted. Signals a
@code{type-error} if @var{method} is not a @code{method}.

The one argument case can be used with @code{setf}, in which case it potentially sets any
or all of the name, classification and stage of @var{method}. There is an ambiguity when
parsing method titles in that there being no explicit class named can indicate with that
the method has no class (principles and pure differentials) or that the class is Hybrid.
When parsing titles for @code{setf} an absence of a class name is taken to mean that there
is no class. Also, if there is no stage name specified when using @code{setf} with
@code{method-title} the stage is set to @code{nil}; @code{*default-stage*} is not
consulted.
@example
@group
 (method-title (method \"Advent\" :class :surprise :stage 8))
   @result{} \"Advent Surprise Major\"
 (method-title (method :name \"Grandsire\" :class :bob :stage 9))
   @result{} \"Grandsire Caters\"
 (method-title (method :stage 8))
   @result{} \"Major\"
 (method-title (method :class :delight :stage 8) t)
   @result{} \"Unknown Delight Major\
 (method-title (method :name \"Advent\" :class :surprise :stage nil))
   @result{} \"Advent Surprise\"
 (method-title (method :name \"Slinky\" :stage 12 :class :place
                       :little t :differential t))
   @result{} \"Slinky Differential Little Place Maximimus\"
 (method-title (method :name \"Stedman\" :stage 11))
   @result{} \"Stedman Cinques\"
 (method-title (method :name \"Meson\" :class :hybrid
                       :little t :stage 12))
   @result{} \"Meson Maximus\"
@end group
@end example"
  (multiple-value-bind (name jump differential little class stage)
      (method-details method)
    (when (and (null name) show-unknown
               ;; Little bob is special, and has no name.
               (not (and little (eq class :bob) (not (or jump differential)))))
      (setf name "Unnamed"))
    (when (or (member class '(:hybrid :hunt))
              (and (member name +suppress-class+ :test #'string-equal)
                   (member class '(:bob :place))
                   (not jump)
                   (not differential)
                   (or (not little) (member name '("Little Grandsire" "Little Union")
                                            :test #'string-equal))
                   ;; nasty special case
                   (not (and (eql stage 5) (eq class :bob) (string-equal name "Union")))))
      (setf class nil)
      (setf little nil))
    (format nil "~{~@[~A~^ ~]~}"
            (iter (for phrase :in (list name
                                        (and jump "Jump")
                                        (and differential "Differential")
                                        (and little "Little")
                                        (and class (not (eq class :hunt)) (class-name class))
                                        (and stage (stage-name stage))))
                  (when phrase
                    (collect phrase))))))

(defmethod print-object ((method method) stream)
  (cond (*print-readably*
         (unless *read-eval*
           (error 'print-not-readable :object method))
         (multiple-value-bind  (name jump differential little class stage)
             (method-details method)
           (let ((place-notation (method-place-notation method)))
             (format stream "#.(~S ~S ~S~{~^ ~S ~S~})" 'method :stage stage
                     (iter (for v :in (list name jump differential little class place-notation))
                           (for k :in '(:name :jump :differential :little :class :place-notation))
                           (for b :in '(nil t t t nil nil nil))
                           (when v
                             (nconcing (list k (if b t v)))))))))
        (*print-escape*
         (print-unreadable-object (method stream :type t :identity t)
           (format stream "~@[~A~]~@[~:*~@[ ~*~]~A~]"
                   (method-title method) (method-place-notation method))))
        (t (format stream "~A" (method-title method t)))))

(defparameter +method-title-scanner+
  (ppcre:create-scanner
   `(:sequence
     :start-anchor
     (:greedy-repetition 0 1
        (:sequence
         (:greedy-repetition 0 nil #\Space)
         (:register (:sequence (:non-greedy-repetition 0 nil :everything) :word-boundary))
         (:greedy-repetition 0 nil #\Space)
         (:greedy-repetition 0 1 (:register (:sequence :word-boundary "Jump" :word-boundary)))
         (:greedy-repetition 0 nil #\Space)
         (:greedy-repetition 0 1 (:register (:sequence :word-boundary "Differential" :word-boundary)))
         (:greedy-repetition 0 nil #\Space)
         (:greedy-repetition 0 1 (:register (:sequence :word-boundary "Little" :word-boundary)))
         (:greedy-repetition 0 nil #\Space)
         (:greedy-repetition 0 1 (:register
                                  (:sequence :word-boundary
                                             (:group (:alternation ,@(iter (for (nil . c) :in +class-names+)
                                                                           (when c
                                                                             (collect (string c))))))
                                             :word-boundary)))
         (:greedy-repetition 0 nil #\Space)
         (:greedy-repetition 0 1 (:register
                                  (:sequence :word-boundary
                                             (:group (:alternation ,@(iter (for s :in-vector +stage-names+)
                                                                           (when s
                                                                             (collect s))))))))
         (:greedy-repetition 0 nil #\Space)))
     :end-anchor)
   :case-insensitive-mode t))

(defun parse-method-title (title)
  (check-type* title string)
  (ppcre:register-groups-bind (name jump differential little class stage)
      (+method-title-scanner+ title)
    ;; In obscure cases I don't understand, but that seem to involve "unusual" Unicode
    ;; characters in a name, a trailing space is coming out as part of the name; zap it.
    (setf name (string-right-trim " " name))
    (setf stage (stage-from-name stage))
    (setf class (class-from-name class))
    (when (and little (null class))
      (setf name (concatenate 'string name " " "Little"))
      (setf little nil))
    (when (equal name "")
      (setf name nil))
    (when (and (null class) (not jump) (not differential))
      ;; special cases
      (cond ((and (eql stage 4)
                  (not little)
                  (member name '("Grandsire" "Reverse Grandsire") :test #'string-equal))
             (setf class :place))
            ((member name +suppress-class+ :test #'string-equal)
             (setf class :bob)
             (when (member name '("Little Grandsire" "Little Union") :test #'string-equal)
               (setf little t)))))
    (values name jump differential little class stage)))

(defun set-method-title (method title)
  (multiple-value-bind (name jump differential little class stage)
      (parse-method-title title)
    (setf (method-name method) name)
    (setf (method-jump-p method) jump)
    (setf (method-differential-p method) differential)
    (setf (method-little-p method) little)
    (setf (method-class method) class)
    (setf (method-stage method) stage))
  title)

(defsetf method-title set-method-title)

(defun method-from-title (title &optional place-notation)
  "Creates a new @code{method} instance, with its name, classification and stage
as specified by @var{title}, and with the given @var{place-notation}.
If the title does not include a stage name, the stage of the result is the current value
of @code{*default-stage*}.

Note that it is not possible to distinguish hybrid methods from non-jump principles, nor
jump methods with hunt bells from those without, from their titles. By convention, if no
hunt bell class is specified in @var{title} a principle, that is a method without hunt
bells, is assumed. If in some specific use this is not correct it can be corrected by
setting @code{method-class}, and possibly @code{method-little-p}, of the resulting method
as desired.

A @code{type-error} is signaled if @var{title} is not a string, or if @var{place-notation}
is neither a string nor @code{nil}.
@example
@group
 (let ((m (method-from-title \"Advent Surprise Major\")))
   (list (method-title m) (method-class m) (method-stage m)))
   @result{} (\"Advent\" :surprise 8)
@end group
@end example"
  (check-type* title string)
  (check-type* place-notation (or string null))
  (let ((result (method :place-notation place-notation)))
    (setf (method-title result) title)
    (unless (method-stage result)
      (setf (method-stage result) *default-stage*))
    result))


;;; Method names

;;; Makes the value for the following constant
;; #+sbcl
;; (defun make-method-name-character-data ()
;;   (let ((character-data (make-hash-table)))
;;     (labels ((alphanp (char &optional exclude)
;;                (let ((category (sb-unicode:general-category char)))
;;                  (and (not (eq category exclude)) (member category '(:ll :lu :nd)))))
;;              (add-alphanum-block (start end &rest omit)
;;                (iter (for i :from start :to end)
;;                      (for c := (code-char i))
;;                      (when (and (alphanp c) (not (member c omit)))
;;                        (setf (gethash c character-data) t)))))
;;       (add-alphanum-block 0 #x7f)         ; Basic Latin
;;       (add-alphanum-block #x80 #xff)      ; Latin-1 Supplement
;;       (add-alphanum-block #x100 #x17f)    ; Latin Extended-A
;;       (add-alphanum-block #x180 #x24f #\Latin_Small_Letter_N_Preceded_By_Apostrophe) ; Latin Extended-B
;;       (add-alphanum-block #x1e00 #x1eff)  ; Latin Extended Additional
;;       (iter (for c :in-string " _!\"&'(),-./=%?£$€™⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉")
;;             (setf (gethash c character-data) t))
;;       (iter (for (c nil) :in-hashtable character-data)
;;             (setf (gethash c character-data)
;;                   (let ((result (sb-unicode:normalize-string (string c) :nfkd)))
;;                     (setf result (delete-if-not (rcurry #'gethash character-data) result))
;;                     (setf result (sb-unicode:casefold result))
;;                     (setf result (ppcre:regex-replace "ø" result "o"))
;;                     (setf result (ppcre:regex-replace "æ" result "ae"))
;;                     (setf result (ppcre:regex-replace "œ" result "oe"))
;;                     (setf result (nsubstitute-if-not #\Space (rcurry #'alphanp :lu) result))
;;                     (cons (not (null (alphanp c))) (coerce result 'list)))))
;;       (iter (for (c (b . r)) :in-hashtable character-data)
;;             (collect (list* (char-code c) b (mapcar #'char-code r)))))))

(define-constant +method-name-character-data+
    (iter (with data := '((48 T 48) (49 T 49) (50 T 50) (51 T 51) (52 T 52) (53 T 53) (54 T 54)
                          (55 T 55) (56 T 56) (57 T 57) (65 T 97) (66 T 98) (67 T 99) (68 T 100)
                          (69 T 101) (70 T 102) (71 T 103) (72 T 104) (73 T 105) (74 T 106) (75 T 107)
                          (76 T 108) (77 T 109) (78 T 110) (79 T 111) (80 T 112) (81 T 113) (82 T 114)
                          (83 T 115) (84 T 116) (85 T 117) (86 T 118) (87 T 119) (88 T 120) (89 T 121)
                          (90 T 122) (97 T 97) (98 T 98) (99 T 99) (100 T 100) (101 T 101) (102 T 102)
                          (103 T 103) (104 T 104) (105 T 105) (106 T 106) (107 T 107) (108 T 108)
                          (109 T 109) (110 T 110) (111 T 111) (112 T 112) (113 T 113) (114 T 114)
                          (115 T 115) (116 T 116) (117 T 117) (118 T 118) (119 T 119) (120 T 120)
                          (121 T 121) (122 T 122) (181 T) (192 T 97) (193 T 97) (194 T 97) (195 T 97)
                          (196 T 97) (197 T 97) (198 T 97 101) (199 T 99) (200 T 101) (201 T 101)
                          (202 T 101) (203 T 101) (204 T 105) (205 T 105) (206 T 105) (207 T 105)
                          (208 T 240) (209 T 110) (210 T 111) (211 T 111) (212 T 111) (213 T 111)
                          (214 T 111) (216 T 111) (217 T 117) (218 T 117) (219 T 117) (220 T 117)
                          (221 T 121) (222 T 254) (223 T 115 115) (224 T 97) (225 T 97) (226 T 97)
                          (227 T 97) (228 T 97) (229 T 97) (230 T 97 101) (231 T 99) (232 T 101)
                          (233 T 101) (234 T 101) (235 T 101) (236 T 105) (237 T 105) (238 T 105)
                          (239 T 105) (240 T 240) (241 T 110) (242 T 111) (243 T 111) (244 T 111)
                          (245 T 111) (246 T 111) (248 T 111) (249 T 117) (250 T 117) (251 T 117)
                          (252 T 117) (253 T 121) (254 T 254) (255 T 121) (256 T 97) (257 T 97)
                          (258 T 97) (259 T 97) (260 T 97) (261 T 97) (262 T 99) (263 T 99) (264 T 99)
                          (265 T 99) (266 T 99) (267 T 99) (268 T 99) (269 T 99) (270 T 100) (271 T 100)
                          (272 T 273) (273 T 273) (274 T 101) (275 T 101) (276 T 101) (277 T 101)
                          (278 T 101) (279 T 101) (280 T 101) (281 T 101) (282 T 101) (283 T 101)
                          (284 T 103) (285 T 103) (286 T 103) (287 T 103) (288 T 103) (289 T 103)
                          (290 T 103) (291 T 103) (292 T 104) (293 T 104) (294 T 295) (295 T 295)
                          (296 T 105) (297 T 105) (298 T 105) (299 T 105) (300 T 105) (301 T 105)
                          (302 T 105) (303 T 105) (304 T 105) (305 T 305) (306 T 105 106)
                          (307 T 105 106) (308 T 106) (309 T 106) (310 T 107) (311 T 107) (312 T 312)
                          (313 T 108) (314 T 108) (315 T 108) (316 T 108) (317 T 108) (318 T 108)
                          (319 T 108) (320 T 108) (321 T 322) (322 T 322) (323 T 110) (324 T 110)
                          (325 T 110) (326 T 110) (327 T 110) (328 T 110) (329 T 110) (330 T 331)
                          (331 T 331) (332 T 111) (333 T 111) (334 T 111) (335 T 111) (336 T 111)
                          (337 T 111) (338 T 111 101) (339 T 111 101) (340 T 114) (341 T 114)
                          (342 T 114) (343 T 114) (344 T 114) (345 T 114) (346 T 115) (347 T 115)
                          (348 T 115) (349 T 115) (350 T 115) (351 T 115) (352 T 115) (353 T 115)
                          (354 T 116) (355 T 116) (356 T 116) (357 T 116) (358 T 359) (359 T 359)
                          (360 T 117) (361 T 117) (362 T 117) (363 T 117) (364 T 117) (365 T 117)
                          (366 T 117) (367 T 117) (368 T 117) (369 T 117) (370 T 117) (371 T 117)
                          (372 T 119) (373 T 119) (374 T 121) (375 T 121) (376 T 121) (377 T 122)
                          (378 T 122) (379 T 122) (380 T 122) (381 T 122) (382 T 122) (383 T 115)
                          (384 T 384) (385 T 595) (386 T 387) (387 T 387) (388 T 389) (389 T 389)
                          (390 T 596) (391 T 392) (392 T 392) (393 T 598) (394 T 599) (395 T 396)
                          (396 T 396) (397 T 397) (398 T 477) (399 T 601) (400 T 603) (401 T 402)
                          (402 T 402) (403 T 608) (404 T 611) (405 T 405) (406 T 617) (407 T 616)
                          (408 T 409) (409 T 409) (410 T 410) (411 T 411) (412 T 623) (413 T 626)
                          (414 T 414) (415 T 629) (416 T 111) (417 T 111) (418 T 419) (419 T 419)
                          (420 T 421) (421 T 421) (422 T 640) (423 T 424) (424 T 424) (425 T 643)
                          (426 T 426) (427 T 427) (428 T 429) (429 T 429) (430 T 648) (431 T 117)
                          (432 T 117) (433 T 650) (434 T 651) (435 T 436) (436 T 436) (437 T 438)
                          (438 T 438) (439 T 658) (440 T 441) (441 T 441) (442 T 442) (444 T 445)
                          (445 T 445) (446 T 446) (447 T 447) (452 T 100 122) (454 T 100 122)
                          (455 T 108 106) (457 T 108 106) (458 T 110 106) (460 T 110 106) (461 T 97)
                          (462 T 97) (463 T 105) (464 T 105) (465 T 111) (466 T 111) (467 T 117)
                          (468 T 117) (469 T 117) (470 T 117) (471 T 117) (472 T 117) (473 T 117)
                          (474 T 117) (475 T 117) (476 T 117) (477 T 477) (478 T 97) (479 T 97)
                          (480 T 97) (481 T 97) (482 T 97 101) (483 T 97 101) (484 T 485) (485 T 485)
                          (486 T 103) (487 T 103) (488 T 107) (489 T 107) (490 T 111) (491 T 111)
                          (492 T 111) (493 T 111) (494 T 658) (495 T) (496 T 106) (497 T 100 122)
                          (499 T 100 122) (500 T 103) (501 T 103) (502 T 405) (503 T 447) (504 T 110)
                          (505 T 110) (506 T 97) (507 T 97) (508 T 97 101) (509 T 97 101) (510 T 111)
                          (511 T 111) (512 T 97) (513 T 97) (514 T 97) (515 T 97) (516 T 101)
                          (517 T 101) (518 T 101) (519 T 101) (520 T 105) (521 T 105) (522 T 105)
                          (523 T 105) (524 T 111) (525 T 111) (526 T 111) (527 T 111) (528 T 114)
                          (529 T 114) (530 T 114) (531 T 114) (532 T 117) (533 T 117) (534 T 117)
                          (535 T 117) (536 T 115) (537 T 115) (538 T 116) (539 T 116) (540 T 541)
                          (541 T 541) (542 T 104) (543 T 104) (544 T 414) (545 T 545) (546 T 547)
                          (547 T 547) (548 T 549) (549 T 549) (550 T 97) (551 T 97) (552 T 101)
                          (553 T 101) (554 T 111) (555 T 111) (556 T 111) (557 T 111) (558 T 111)
                          (559 T 111) (560 T 111) (561 T 111) (562 T 121) (563 T 121) (564 T 564)
                          (565 T 565) (566 T 566) (567 T 567) (568 T 568) (569 T 569) (570 T 11365)
                          (571 T 572) (572 T 572) (573 T 410) (574 T 11366) (575 T 575) (576 T 576)
                          (577 T 578) (578 T 578) (579 T 384) (580 T 649) (581 T 652) (582 T 583)
                          (583 T 583) (584 T 585) (585 T 585) (586 T 587) (587 T 587) (588 T 589)
                          (589 T 589) (590 T 591) (591 T 591) (7680 T 97) (7681 T 97) (7682 T 98)
                          (7683 T 98) (7684 T 98) (7685 T 98) (7686 T 98) (7687 T 98) (7688 T 99)
                          (7689 T 99) (7690 T 100) (7691 T 100) (7692 T 100) (7693 T 100) (7694 T 100)
                          (7695 T 100) (7696 T 100) (7697 T 100) (7698 T 100) (7699 T 100) (7700 T 101)
                          (7701 T 101) (7702 T 101) (7703 T 101) (7704 T 101) (7705 T 101) (7706 T 101)
                          (7707 T 101) (7708 T 101) (7709 T 101) (7710 T 102) (7711 T 102) (7712 T 103)
                          (7713 T 103) (7714 T 104) (7715 T 104) (7716 T 104) (7717 T 104) (7718 T 104)
                          (7719 T 104) (7720 T 104) (7721 T 104) (7722 T 104) (7723 T 104) (7724 T 105)
                          (7725 T 105) (7726 T 105) (7727 T 105) (7728 T 107) (7729 T 107) (7730 T 107)
                          (7731 T 107) (7732 T 107) (7733 T 107) (7734 T 108) (7735 T 108) (7736 T 108)
                          (7737 T 108) (7738 T 108) (7739 T 108) (7740 T 108) (7741 T 108) (7742 T 109)
                          (7743 T 109) (7744 T 109) (7745 T 109) (7746 T 109) (7747 T 109) (7748 T 110)
                          (7749 T 110) (7750 T 110) (7751 T 110) (7752 T 110) (7753 T 110) (7754 T 110)
                          (7755 T 110) (7756 T 111) (7757 T 111) (7758 T 111) (7759 T 111) (7760 T 111)
                          (7761 T 111) (7762 T 111) (7763 T 111) (7764 T 112) (7765 T 112) (7766 T 112)
                          (7767 T 112) (7768 T 114) (7769 T 114) (7770 T 114) (7771 T 114) (7772 T 114)
                          (7773 T 114) (7774 T 114) (7775 T 114) (7776 T 115) (7777 T 115) (7778 T 115)
                          (7779 T 115) (7780 T 115) (7781 T 115) (7782 T 115) (7783 T 115) (7784 T 115)
                          (7785 T 115) (7786 T 116) (7787 T 116) (7788 T 116) (7789 T 116) (7790 T 116)
                          (7791 T 116) (7792 T 116) (7793 T 116) (7794 T 117) (7795 T 117) (7796 T 117)
                          (7797 T 117) (7798 T 117) (7799 T 117) (7800 T 117) (7801 T 117) (7802 T 117)
                          (7803 T 117) (7804 T 118) (7805 T 118) (7806 T 118) (7807 T 118) (7808 T 119)
                          (7809 T 119) (7810 T 119) (7811 T 119) (7812 T 119) (7813 T 119) (7814 T 119)
                          (7815 T 119) (7816 T 119) (7817 T 119) (7818 T 120) (7819 T 120) (7820 T 120)
                          (7821 T 120) (7822 T 121) (7823 T 121) (7824 T 122) (7825 T 122) (7826 T 122)
                          (7827 T 122) (7828 T 122) (7829 T 122) (7830 T 104) (7831 T 116) (7832 T 119)
                          (7833 T 121) (7834 T 97) (7835 T 115) (7836 T 7836) (7837 T 7837)
                          (7838 T 115 115) (7839 T 7839) (7840 T 97) (7841 T 97) (7842 T 97) (7843 T 97)
                          (7844 T 97) (7845 T 97) (7846 T 97) (7847 T 97) (7848 T 97) (7849 T 97)
                          (7850 T 97) (7851 T 97) (7852 T 97) (7853 T 97) (7854 T 97) (7855 T 97)
                          (7856 T 97) (7857 T 97) (7858 T 97) (7859 T 97) (7860 T 97) (7861 T 97)
                          (7862 T 97) (7863 T 97) (7864 T 101) (7865 T 101) (7866 T 101) (7867 T 101)
                          (7868 T 101) (7869 T 101) (7870 T 101) (7871 T 101) (7872 T 101) (7873 T 101)
                          (7874 T 101) (7875 T 101) (7876 T 101) (7877 T 101) (7878 T 101) (7879 T 101)
                          (7880 T 105) (7881 T 105) (7882 T 105) (7883 T 105) (7884 T 111) (7885 T 111)
                          (7886 T 111) (7887 T 111) (7888 T 111) (7889 T 111) (7890 T 111) (7891 T 111)
                          (7892 T 111) (7893 T 111) (7894 T 111) (7895 T 111) (7896 T 111) (7897 T 111)
                          (7898 T 111) (7899 T 111) (7900 T 111) (7901 T 111) (7902 T 111) (7903 T 111)
                          (7904 T 111) (7905 T 111) (7906 T 111) (7907 T 111) (7908 T 117) (7909 T 117)
                          (7910 T 117) (7911 T 117) (7912 T 117) (7913 T 117) (7914 T 117) (7915 T 117)
                          (7916 T 117) (7917 T 117) (7918 T 117) (7919 T 117) (7920 T 117) (7921 T 117)
                          (7922 T 121) (7923 T 121) (7924 T 121) (7925 T 121) (7926 T 121) (7927 T 121)
                          (7928 T 121) (7929 T 121) (7930 T 7931) (7931 T 7931) (7932 T 7933)
                          (7933 T 7933) (7934 T 7935) (7935 T 7935) (32 NIL 32) (95 NIL 32) (33 NIL 32)
                          (34 NIL 32) (38 NIL 32) (39 NIL 32) (40 NIL 32) (41 NIL 32) (44 NIL 32)
                          (45 NIL 32) (46 NIL 32) (47 NIL 32) (61 NIL 32) (37 NIL 32) (63 NIL 32)
                          (163 NIL 32) (36 NIL 32) (8364 NIL 32) (8482 NIL 116 109) (8304 NIL 48)
                          (185 NIL 49) (178 NIL 50) (179 NIL 51) (8308 NIL 52) (8309 NIL 53)
                          (8310 NIL 54) (8311 NIL 55) (8312 NIL 56) (8313 NIL 57) (8320 NIL 48)
                          (8321 NIL 49) (8322 NIL 50) (8323 NIL 51) (8324 NIL 52) (8325 NIL 53)
                          (8326 NIL 54) (8327 NIL 55) (8328 NIL 56) (8329 NIL 57)))
          (with result := (make-hash-table :size (length data)))
          (for (c p . s) :in data)
          (setf (gethash (code-char c) result) (cons p (mapcar #'code-char s)))
          (finally (return result)))
  :test #'equalp)

(defun comparable-method-name (string)
  "If @var{string} is a suitable name for a method, returns a version appropriate for
comparison with other comparable names, and otherwise returns @code{nil}.

The Central Council of Church Bell Ringers
@url{https://cccbr.github.io/method_ringing_framework/, Framework for Method
Ringing} (FMR), appendix B describes a syntax for method names and their comparisons. This
function both determines whether or not they fit within the syntax described by the FMR,
and, if so, provides a canonical representation for them suitable for comparing whether or
not two apparently different names will be considered the same when describing a method.
This comparable representation is not intended for presentation to end users, but
rather just for comparing names for equivalence.

Signals a @code{type-error} if @var{string} is not a string.
@example
@group
 (comparable-method-name \"New Cambridge\")
   @result{} \"new cambridge\"
 (comparable-method-name \"London No.3\")
   @result{} \"london no 3\"
 (comparable-method-name \"m@U{00E4}k@U{010D}e@U{0148} E=mc@U{00B2}\")
   @result{} \"makcen e mc2\"
 (comparable-method-name \"Two is Too  Many Spaces\")
   @result{} nil
 (comparable-method-name \"@U{0395}@U{03BB}@U{03BB}@U{03B7}@U{03BD}@U{03B9}@U{03BA}@U{03AC} is Greek to me\")
   @result{} nil
@end group
@end example"
  (check-type* string string)
  (and (name-spaces-p string)
       (multiple-value-bind (result alphanum) (comparableize-string string)
         (and alphanum result (string-trim " " result)))))

(defun name-spaces-p (s)
  (not (ppcre:scan "^ |  | $" s)))

(defun comparableize-string (s)
  (and (<= 1 (length s) 120)
       (iter (with result := (make-array (* 2 (length s))
                                         :element-type 'character
                                         :fill-pointer 0))
             (for c :in-string s)
             (for d := (gethash c +method-name-character-data+))
             (for (alphanum . replacements) := d)
             (always d)
             (counting alphanum :into n)
             (mapc (rcurry #'vector-push result) replacements)
             (finally (return (values (ppcre:regex-replace-all "  +" result " ")
                                      (> n 0)))))))


;;; Method traits

;; A method-traits instance is used to cache information about a method that is
;; computable from its stage and place notation. A least recently used (LRU) cache of
;; method-traits is maintained to improve the performance of some method functions.

(defstruct (method-traits (:copier nil) (:predicate nil))
  (changes nil)
  (contains-jump-changes-p nil)
  (lead-length nil)
  (lead-head nil)
  (lead-count nil)
  (plain-lead nil)
  (plain-course nil)
  (course-length nil)
  (hunt-bells nil)
  (principal-hunt-bells nil)
  (secondary-hunt-bells nil)
  (working-bells nil)
  (classification nil)
  (lead-head-code nil))

(defconstant +method-traits-cache-size+ 100)

(defparameter *method-traits-cache* (make-lru-cache +method-traits-cache-size+ :test #'eq))

(defun get-method-traits (method &optional (create-if-necessary t))
  (let ((exisiting (getcache method *method-traits-cache*)))
    (cond (exisiting)
          (create-if-necessary
           (putcache method *method-traits-cache* (make-method-traits))))))

(defun clear-method-traits (method)
  (when-let ((traits (get-method-traits method nil)))
    (setf (method-traits-changes traits) nil)
    (setf (method-traits-contains-jump-changes-p traits) nil)
    (setf (method-traits-lead-length traits) nil)
    (setf (method-traits-lead-head traits) nil)
    (setf (method-traits-lead-count traits) nil)
    (setf (method-traits-plain-lead traits) nil)
    (setf (method-traits-plain-course traits) nil)
    (setf (method-traits-course-length traits) nil)
    (setf (method-traits-hunt-bells traits) nil)
    (setf (method-traits-principal-hunt-bells traits) nil)
    (setf (method-traits-secondary-hunt-bells traits) nil)
    (setf (method-traits-working-bells traits) nil)
    (setf (method-traits-classification traits) nil)
    (setf (method-traits-lead-head-code traits) nil))
  method)

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
      (setf (method-traits-contains-jump-changes-p traits) (notevery #'changep changes)))))

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

(define-method-trait contains-jump-changes-p (%update-changes changes)
  "If @var{method}'s stage and place-notation have been set and method contains one or
more jump changes returns true, and otherwise returns @code{nil}. Note that even if the
place notation is set and implies jump changes, if the stage is not set
@code{method-contains-jump-changes-p} will still return @code{nil}.

Note that this function reflects the place notation of @var{method} while
@code{method-jump-p} reflects the classification stored in the method, and they may not
agree.

Signals a @code{type-error} if @var{method} is not a @code{method}. Signals a
@code{parse-error} if the place notation string cannot be properly parsed as place
notation at @var{method}'s stage.
@example
@group
 (method-contains-jump-changes-p
   (method :place-notation \"x3x4x2x3x4x5,2\"
           :stage 6))
     @result{} nil
 (method-contains-jump-changes-p
   (method :place-notation \"x3x(24)x2x(35)x4x5,2\"
           :stage 6))
     @result{} t
 (method-contains-jump-changes-p
   (method :stage 6))
     @result{} nil
 (method-contains-jump-changes-p
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
   (method-from-title \"Cambridge Surprise Minor\" \"x3x4x2x3x4x5,2\"))
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
 (method-lead-head (method-from-title \"Little Bob Major\" \"x1x4,2\"))
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

Signals a @code{type-error} if @var{method} is not a @code{method}, and signal a
@code{parse-error} if the place notation string cannot be properly parsed as place
notation at @var{method}'s stage.
@example
@group
 (method-hunt-bells (method-from-title \"Grandsire Doubles\"
                                       \"3,1.5.1.5.1\"))
     @result{} (0 1)
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

(define-constant +lead-head-codes+
    (iter (for s :in '("ABCDEF" "GHJKLM" "PQ" "RS"))
          (collect (iter (with vlen = (- (* 2 (ceiling +maximum-stage+ 2)) 4))
                         ;; vlen is always even
                         (with v := (make-array vlen))
                         (with mid := (floor (length s) 2))
                         (for i :from 0 :below (floor vlen 2))
                         (for j := (if (eql mid 1) i (- i 2)))
                         (labels ((set-val (vi ci &optional d)
                                    (setf (svref v vi)
                                          (make-keyword (format nil "~C~@[~D~]"
                                                                (char s ci) d)))))
                           (cond ((< i mid)
                                  (set-val i i)
                                  (set-val (- vlen 1 i) (- (* 2 mid) 1 i)))
                                 (t
                                  (set-val i (- mid 1) j)
                                  (set-val (- vlen 1 i) mid j))))
                         (finally (return v)))))
  :test #'equalp)

(defun %update-lead-head-code (method traits)
  (setf (method-traits-lead-head-code traits) nil)
  (when (>= (method-lead-length method) 2)
    (let* ((stage (method-stage method))
           (max (- stage 2))
           (lh (method-lead-head method))
           n)
      (labels ((set-code (kind)
                 (let ((codes (find kind +lead-head-codes+ :key #'(lambda (x) (svref x 0)))))
                   (setf (method-traits-lead-head-code traits)
                         (aref codes (if (<= n (funcall (if (evenp stage) #'floor #'ceiling)
                                                        max 2))
                                         (- n 1)
                                         (- (length codes) (- max n) 1)))))))
        (cond ((setf n (which-plain-bob-lead-head lh))
               (let ((bells (row-bells (first (last (method-changes method))))))
                 (if (evenp stage)
                     (cond ((%placesp bells 0 1)
                            (set-code :a))
                           ((%placesp bells 0 (- stage 1))
                            (set-code :g)))
                     (cond ((%placesp bells 0 1 (- stage 1))
                            (set-code :p))
                           ((%placesp bells 0)
                            (set-code :r))))))
              ((and (setf n (which-grandsire-lead-head lh))
                    (>= (method-lead-length method) 3)
                    (%placesp (row-bells (first (last (method-changes method))))
                              ;; Note that %placesp will ignore the second argument at
                              ;; odd stages.
                              0 (- stage 1)))
               (decf max)
               (let ((bells (row-bells (first (method-changes method)))))
                 (if (oddp stage)
                     (cond ((%placesp bells 2)
                            (set-code :a))
                           ((%placesp bells (- stage 1))
                            (set-code :g)))
                     (cond ((%placesp bells 2 (- stage 1))
                            (set-code :p))
                           ((%placesp bells)
                            (set-code :r)))))))))))

(define-method-trait lead-head-code (%update-lead-head-code)
  "Returns the lead head code for @var{method}, as a keyword, if its stage and place

notation are set and it has Plain Bob or Grandsire lead ends, and otherwise returns
@code{nil}. No methods below minimus are considered to have such lead ends, nor is rounds
considered such a lead end. When not @code{nil} the result is a keyword whose name
consists of a single letter, possibly followed by a digit.

The CCCBR's various collections of methods have, for several decades, used succinct codes,
typically single letters or, more recently, single letters followed by digits, to denote
various lead ends for the methods they contain. While the choices made have in the past
varied by collection, in recent decades a consistent set of codes has been used, which is
now codified in the Central Council of Church Bell Ringers
@url{https://cccbr.github.io/method_ringing_framework/, Framework for Method
Ringing} (FMR), appendix C. While these codes actually describe both a row and a change
adjacent to that row, and thus two different rows, the FMR calls them \"lead head codes\",
so that phrasing is also used here.

There is currently (as of July 2019) an issue with the definitions of these codes in the
FMR, where those for Grandsire-like methods do not correctly correspond to common
practice. For example, most ringers would consider Itchingfield Slow Bob Doubles and
Longford Bob Doubles to have the same lead ends. However, the current FMR definition says
that the former has 'c' Grandsire lead ends, and the latter does not. This is currently
under discussion for correction in the next revision of the FMR. The
@code{method-lead-head-code} function is implemented assuming that this will be corrected
in the next revision of the FMR to match common practice. For example, it considers
neither Itchingfield Slow Bob nor Longford Bob as having Grandsire lead ends.

It is also worth noting that, for some of the less common cases, the lead end codes
defined in the FMR differ from those used in earlier CCCBR collections.

Signals a @code{type-error} if @var{method} is not a @code{method}, and a
@code{parse-error} if @var{method}'s place notation cannot be interpreted at its stage.
@example
@group
 (method-lead-head-code
   (lookup-method-by-title \"Advent Surprise Major\"))
     @result{} :h
 (method-lead-head-code
   (lookup-method-by-title \"Zanussi Surprise Maximus\"))
     @result{} :j2
 (method-lead-head-code
   (lookup-method-by-title \"Sgurr Surprise Royal\"))
     @result{} :d
 (method-lead-head-code
   (lookup-method-by-title \"Twerton Little Bob Caters\"))
     @result{} :q2
 (method-lead-head-code
   (lookup-method-by-title \"Grandsire Royal\"))
     @result{} :p
 (method-lead-head-code
   (lookup-method-by-title \"Double Glasgow Surprise Major\"))
     @result{} nil
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
   (method-from-title \"Cambridge Surprise Minor\"
                      \"x3x4x2x3x4x5,2\"))
     @result{} 5
 (method-lead-count
   (method-from-title \"Cromwell Tower Block Surprise Minor\"
                      \"3x3.4x2x3x4x3,6\"))
     @result{} 1
 (method-lead-count
   (method-from-title \"Bexx Differential Bob Minor\"
                      \"x1x1x23,2\"))
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
          (for n :from 1)
          (nconcing rows :into result)
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


;;; Method classification

(define-constant +path-types+
    #(:plain :treble-dodging :treble-place :alliance :hybrid)
  :test #'equalp)

(defun classify-method (method)
  "Assigns the classification fields of @var{method} to match the classification assigned
by the Central Council of Church Bell Ringers
@url{https://cccbr.github.io/method_ringing_framework/, Framework for Method
Ringing} (FMR) for the place notation contained in that @code{method}, and returns the
method. Signals a @code{type-error} if @var{method} is not a @code{method}. Signals a
@code{no-place-notation-error} if either the stage or place notation of @var{method} are
not set. Signals a @code{parse-error} if the value of the place notation field cannot be
interpreted as place notation at the stage of @var{method}.
@example
@group
 (method-title (classify-method
                 (method :stage 8 :place-notation \"x3x6x5x45,2\"))
               t)
     @result{} \"Unnamed Differential Little Surprise Major\"
@end group
@end example"
  (unless (method-changes method)
    (error 'no-place-notation-error :method method))
  (setf (method-differential-p method)
        (destructuring-bind (&optional first &rest rest) (method-working-bells method)
          (and rest (let ((n (length first)))
                      (notevery (lambda (x) (eql (length x) n)) rest)))))
  (setf (method-jump-p method) (method-contains-jump-changes-p method))
  (let ((hunt-bells (method-hunt-bells method)))
    (cond ((null hunt-bells)
           (setf (method-class method) nil)
           (setf (method-little-p method) nil))
          ((method-jump-p method)
           (setf (method-class method) :hunt)
           (setf (method-little-p method)
                 (every (lambda (b) (jump-allowed-path-little-p method b)) hunt-bells)))
          (t (iter (with little-p)
                   (with best-path-type)
                   (with best-hunt-bells)
                   (for h :in hunt-bells)
                   (for (values path-type path-little-p cross-sections)
                        := (path-attributes-non-jump method h))
                   (unioning cross-sections :into all-cross-sections)
                   (cond ((or (first-iteration-p)
                              ;; The or...0 is to make annoying SBCL shut up about
                              ;; position maybe being nil, even though it never will be.
                              (< (or (position path-type +path-types+) 0)
                                 (or (position best-path-type +path-types+) 0)))
                          (setf best-path-type path-type)
                          (setf best-hunt-bells (list h))
                          (setf little-p path-little-p))
                         ((eq path-type best-path-type)
                          (push h best-hunt-bells)
                          (unless path-little-p
                            (setf little-p nil))))
                   (finally (setf (method-little-p method) little-p)
                            (setf (method-class method)
                                  (case best-path-type
                                    (:plain (plain-class-type method best-hunt-bells))
                                    (:treble-dodging (treble-dodging-class-type method all-cross-sections))
                                    (otherwise best-path-type))))))))
  method)

(defun jump-allowed-path-little-p (method bell)
  ;; bell must be a hunt bell
  (iter (with all-positions := (lognot (ash -1 (method-stage method))))
        (with seen-positions := (ash 1 bell))
        (for r :in (method-plain-lead method))
        (when (eql (setf seen-positions
                         (logior (ash 1 (position-of-bell bell r)) seen-positions))
                   all-positions)
          (return nil))
        (finally (return t))))

(defun path-attributes-non-jump (method bell)
  ;; method must not contain jump changes, bell must be a hunt bell
  ;; returns three values:
  ;;   - the path-type, a keyword
  ;;   - whether or not it is little, a generalized boolean
  ;;   - a list of indices into the lead for cross sections if it is :treble-dodging,
  ;;     and absent or nil otherwise
  (iter (with stage := (method-stage method))
        (with positions-counts := (make-array stage :initial-element 0))
        (with last := (list bell))
        (with reversed-path := last)
        (with places-count := 0)
        ;; The following works because non-jump changes are involutions.
        (for i :initially bell :then (aref (row-bells c) i))
        (for p :previous i)
        (for c :in (method-changes method))
        (incf (aref positions-counts i))
        (if-first-time nil (push i reversed-path))
        (minimize i :into min)
        (maximize i :into max)
        (after-each (when (eql i p) (incf places-count)))
        (finally (let* ((excursion (1+ (- max min)))
                        (stationaryp (eql excursion 1))
                        (littlep (< excursion stage))
                        (same-count (reduce (lambda (m n) (and (equal m n) m))
                                            positions-counts
                                            :start min :end (1+ max)))
                        (lead-head-end-min-p (and (eql (first reversed-path) min)
                                                  (eql (first last) min)))
                        (path (reverse reversed-path))
                        (reversible (iter (with start := reversed-path)
                                          (when (equal reversed-path path)
                                            (return t))
                                          (setf (rest last) reversed-path)
                                          (pop last)
                                          (pop reversed-path)
                                          (setf (rest last) nil)
                                          (until (eq reversed-path start))))
                        (path-type (cond (stationaryp :treble-place)
                                         ((not reversible) :hybrid)
                                         ((not same-count) :alliance)
                                         ((not (eql places-count 2)) :treble-place)
                                         ((eql same-count 2) :plain)
                                         (t :treble-dodging))))
                   (return (values path-type
                                   littlep
                                   (and (eq path-type :treble-dodging)
                                        (cross-sections (method-lead-length method)
                                                        excursion
                                                        (and (not lead-head-end-min-p) path)))))))))

(defun cross-sections (lead-length excursion path)
  ;; assumes excusion divides lead-length, and path, if non-nil, is a reasonable treble
  ;; dodging path of length lead-length
  (iter (with section-length := (/ lead-length excursion))
        (with mid := (/ excursion 2))
        (with offset := (iter (for p :in path)
                              (for pv :previous p)
                              (for j :from -1)
                              (when (eql pv p) (return j))
                              (finally (return -1)))) ; path is null
        (for i :from 1 :below excursion)
        (unless (eql i mid)
          (collect (mod (+ (* i section-length) offset) lead-length)))))

(defun plain-class-type (method skip)
  (iter (for b :from 0 :below (method-stage method))
        (unless (member b skip)
          (iter (with diff := 0)
                (for c :in (method-changes method))
                (for i :first (bell-at-position c b) :then (bell-at-position c i))
                (for p :previous i :initially b)
                (for d := (- i p))
                (cond ((zerop d) (setf diff 0))
                      ((zerop diff) (setf diff d))
                      ((not (eql d diff)) (return-from plain-class-type :bob)))))
        (finally (return :place))))

(defun treble-dodging-class-type (method cross-sections)
  (unless cross-sections
    (return-from treble-dodging-class-type :treble-bob))
  (labels ((internal-places-p (change)
             (iter (for i :in-vector (row-bells change) :from 1 :below (- (stage change) 1)
                        :with-index j)
                   (thereis (eql i j)))))
    (iter (with first := nil)
          (for cs :in cross-sections)
          (for c := (nth cs (method-changes method)))
          (if-first-time (setf first (internal-places-p c))
                         (if (internal-places-p c)
                             (unless first (return :delight))
                             (when first (return :delight))))
          (finally (return (if first :surprise :treble-bob))))))


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

(with-bell-property-resolver
  (defparameter +conventional-symmetry-scanner+
    (ppcre:create-scanner ",(:?[-x]|\\p{bell}+)$" :case-insensitive-mode t)))

(defun method-conventionally-symmetric-p (method)
  "Returns true if and only if the method has an even lead length and conventional
palindromic symmetry with apices at its half-lead and lead-end. Note that this means
it is false for methods such as Grandsire. Signals a @code{type-error} if @var{method} is
not a @code{method}. Signals a @code{no-place-notation-error} if @var{method}'s stage or
place notation are not set. Signals a @code{parse-error} if @var{method}'s place notation
cannot be interpreted at its stage.
@example
@group
 (method-conventionally-symmetric-p
   (lookup-method-by-title \"Advent Surprise Major\"))
     @result{} t
 (method-conventionally-symmetric-p
   (lookup-method-by-title \"Grandsire Caters\"))
     @result{} nil
@end group
@end example"
  (if-let ((length (method-lead-length method)))
    (or (and (eql length 2) (not (method-contains-jump-changes-p method)))
        (and (evenp (method-lead-length method))
             (or (not (null (ppcre:scan +conventional-symmetry-scanner+
                                        (method-place-notation method))))
                 (multiple-value-bind (first second)
                     (split-palindromic-changes (%get-changes method)
                                                length
                                                (method-contains-jump-changes-p method))
                   (declare (ignore first))
                   (eql (length second) 1)))))
    (error 'no-place-notation-error :method method)))

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
          ;; to follow an open paren, or for a dot to follow a comma, a cross or another
          ;; dot, and that all dots are parts of digraphs encoded specially.
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
  ;; their own encodings. Returns an ASCII85 encoded string.
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
      (binascii:encode-ascii85 result))))

;; code used with an older version of Roan to generate the character frequencies
;;
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

(defun canonical-rotation-key (changes)
  (encode-place-notation (canonicalize-place-notation (canonical-rotation changes))
                         (stage (first changes))))

(defun method-canonical-rotation-key (method)
  "If @var{method} has its stage and place notation set returns a string uniquely
identifying, using @code{equal}, the changes of a lead of this method, invariant under
rotation. That is, if, and only if, two methods are rotations, possibly trivially so, of
one another their @code{method-canonical-rotation-key}s will always be @code{equal}. While
a string, the value is essentially an opaque type and should generally not be displayed to
an end user or otherwise have its structure depended upon, though it can be printed and
read back in again. While, within one version of Roan, this key can be counted on to be
the same in different sessions and on different machines, it may change between versions
of Roan. If @var{method} does not have both its stage and place notation set
@code{method-canonical-rotation-key} returns @code{nil}.

Signals a @code{type-error} if @var{method} is not a @code{method}. Signals a
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
    (canonical-rotation-key changes)))


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
sources, including Appendix B of
@url{http://www.methods.org.uk/method-collections/xml-zip-files/method%20xml%201.0.pdf,
Peter Niblett's XML format documentation}.

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
         (unless *read-eval*
           (error 'print-not-readable :object fch-group))
         (format stream "#.(FCH-GROUP ~S~:[~*~; T ~:[NIL~;T~]~])"
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
even stage royal or above but with any of the bells conventionally called the seven (and
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
solidus (@samp{/}); those before the solidus are in course and those after it out of
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
       (funcall (if (method-contains-jump-changes-p method)
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
the seventh (that is, the bell roan denotes by @code{6}) and above fixed. Returns three
values: a summary of the courses that are false; for methods that have Plain Bob lead ends
and lead heads and the usual palindromic symmetry, the false course head groups that are
present; and a description of the incidence of falseness.

The first value is a list of course heads, @code{row}s that have the treble and tenors
fixed, such that the plain course is false against the courses starting with any of these
course heads. Rounds is included only if the falseness occurs between rows at two
different positions within the plain course. Course heads for major have just the
tenor (that is, the bell represented in Roan by the integer @code{7}) fixed, while course
heads for higher stages have all of the seventh and above (that is, bells represented in
Roan by the integers @code{6} and larger) fixed in their rounds positions.

If @var{method} has Plain Bob lead ends and lead heads, and the usual palindromic
symmetry, the second value returned is a list of @code{fch-group} objects, and otherwise
the second value is @code{nil}. Note also that for methods that are completely clean in
the context used by this function, for example plain royal methods, an empty list also
will be returned. These two cases can be disambiguated by examining the first value
returned.

There is some ambiguity in the interpretation of ``A'' falseness. In Roan a method is
only said to have ``A'' falseness if its plain course is false. That is, the trivial
falseness implied by a course being false against itself and against its reverse by
virtue of containing exactly the same rows is not reported as ``A'' falseness. ``A''
falseness is only reported if there is some further, not-trivial falseness between rows
at two different positions within the plain course.

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
    (labels ((add-fch (lead row other-lead other-row)
               (unless (aref incidence lead other-lead)
                 (setf (aref incidence lead other-lead)
                       (make-hash-set :size +incidence-initial-hash-set-size+)))
               (let ((fch (permute-by-inverse row other-row)))
                 (hash-set-nadjoin (aref incidence lead other-lead) fch)
                 (hash-set-nadjoin summary fch))))
      (iter (with start := (if (eql stage 8) 7 6))
            (with lead-len := (method-lead-length method))
            (for row :in (method-plain-course method))
            (for i :from 0)
            (push (cons (floor i lead-len) row)
                  (gethash (iter (with bells := (row-bells row))
                                 (with result := (position 0 bells))
                                 (for i :from start :to limit)
                                 (setf result (logior (ash result +row-signature-shift+)
                                                      (position i bells)))
                                 (finally (return result)))
                           table)))
      (iter (for (nil list) :in-hashtable table)
            (iter (for ((lead . row) . rest) :on list)
                  (when rest
                    (iter (for (other-lead . other-row) :in rest)
                          (add-fch lead row other-lead other-row)
                          (add-fch other-lead other-row lead row))))))
    (dotimes (i limit)
      (dotimes (j limit)
        (fch-list-f (aref incidence i j))))
    (fch-list-f summary)
    (values summary (falseness-groups method summary) incidence)))



;;; Calls

(defstruct (call (:copier nil) (:predicate nil))
  "===summary===
Roan provides an immutable @code{call} object that describes a change ringing call, such
as a bob or single, that modifies a lead of a @code{method}. A @code{call} usually has a
fragment of place notation representing changes that are added to the the sequence of
changes constituting the lead, typically replacing some existing changes in the lead.

A @code{call} has an offset, which specifies where in the lead the changes are added,
replaced or deleted; this offset can be indexed from the beginning or the end of a lead,
which frequently allows the same call to be used for similar methods with possibly
different lead lengths. It is also possible to index from a postion within the lead rather
than the beginning or end by supplying a fraction; again, this allows using, for example,
half-lead calls with similar methods with different lead lengths.

Typically a @code{call} replaces exactly as many changes as it supplies. However it is
possible to replace none, in which case the @code{call} adds to the lead length; to only
replace changes with a zero length sequence of changes, in which case the @code{call}
shortens the lead by deleting changes; or even to add more or fewer changes than it
replaces.

Typically a call only affects the lead of a method to which is is applied. In exceptional
cases, most notably doubles variations, it may also affect the subsequent lead. To support
such use a @code{call} may have a following place notation fragment and a following
replacement length. Such use is always restricted to being positioned at the beginning of
the subsequent lead, and in the main lead the call must replace changes all the way to the
end of the lead. Note that by starting the call at the end of the lead this could be
simply adding changes, or even doing nothing.

A @code{call} is applied to a lead with the function @code{call-apply}. This can take
multiple @code{call}s, all of which are applied to the same lead. They must not, however,
overlap. The @code{call-apply} function returns two values. The first is a list of the
changes of the lead, modified by the @code{call}(s). The second, if not @code{nil}, is
another @code{call} to be applied to the following lead, and is only non-nil when a
@code{call} does apply also to the subsequent lead.

Two @code{call}s may be compared with @code{equalp}.

Examples of @code{call}s:
@itemize
@item
The usual bob for Cambridge Surprise is @code{(call \"4\")}.
@item
The usual single for Grandsire is @code{(call \"3.123\" :offset 2)}.
@item
The usual bob for Erin Caters is @code{(call \"7\" :from-end nil)}.
@item
A 58 half-lead bob for Bristol Major is @code{(call \"5\" :fraction 1/2)}.
@item
A bob in April Day Doubles is @code{(call \"3.123\" :following \"3\")}.
@item
A call for surprise that shortens the lead by omitting the first two
blows, so that ringing of the lead commences at the backstroke snap is
@code{(call nil :from-end nil :replace 2)}.
@end itemize
===endsummary===
An immutable object describing a change ringing call, such as a bob or single."
  (place-notation nil :read-only t :type (or null string))
  (offset 0 :read-only t :type (integer 0))
  (from-end t :read-only t)
  (fraction nil :read-only t :type (or null (rational (0) (1))))
  (replace 0 :read-only t :type (integer 0))
  (following nil :read-only t)
  (changes #() :read-only t :type simple-vector))

(defmethod print-object ((call call) stream)
  (cond (*print-readably*
         (prin1 `(call ,(call-place-notation call)
                       ,@(unless (call-from-end call) '(:from-end nil))
                       ,@(when-let ((offset (call-offset call))) `(:offset ,offset))
                       ,@(when-let ((fraction (call-fraction call))) `(:fraction ,fraction))
                       ,@(when-let ((replace (call-replace call))) `(:replace ,replace))
                       ,@(when-let ((following (call-following call)))
                                   `(:following ,(call-place-notation following)
                                     :following-replace ,(call-replace following))))
                stream))
        (*print-escape*
         (print-unreadable-object (call stream :type t :identity t)
           (format stream "~A ~@{~A~^ ~}"
                   (call-place-notation call)
                   (call-offset call)
                   (not (not (call-from-end call)))
                   (call-fraction call)
                   (call-replace call))
           (when-let ((f (call-following call)))
             (format stream " ~@{~A~^ ~}" (call-place-notation f) (call-replace f)))))
        (t (format stream "Call~@[-~A~]~:[~;*~]"
                   (call-place-notation call) (call-following call))))
  call)

(defconstant +call-changes-vector-length+ (+ (- +maximum-stage+ +minimum-stage+) 1))

(defmacro %get-call-changes (vector stage)
  `(svref ,vector (- ,stage +minimum-stage+)))

(defmacro get-call-changes (call stage)
  `(%get-call-changes (call-changes ,call) ,stage))

(defun call (place-notation &key (from-end t) offset fraction replace
                              (following nil following-supplied-p)
                              (following-replace nil following-replace-supplied-p))
  "Creates and returns a @code{call}, which modifies the changes of a lead of a
@code{method}. The @var{place-notation} argument is a string of place notation, the
changes corresponding to which will be added to or replace changes in a a lead of the
@code{method} when applying the @code{code}. The @var{place-notation} may be @code{nil},
in which case no changes are added or replace existing ones. The @var{offset}, a
non-negative integer, is the position at which to begin modifying the lead, and is
measured from the beginning of the lead if the generalized boolean @var{from-end} is
false, and from the end, otherwise. This can be further modifed by @var{fraction} which is
multiplied by the lead length; the offset is counted forward or backward from that
product. The @code{fraction}, if non-nill, must be a ratio greater than @code{0} and less
than @code{1}, whose denominator evenly divides the lead length. The non-negative integer
@var{replace} is the number of changes in the lead to be deleted or replaced. It is
typically equal to the length of @var{changes}, which results in exact replacement of
changes in the lead, but may be greater or less than that length, in which case the
resulting lead is of a different length than a plain lead.

If either or both of @var{following} or @var{following-replace} are supplied the call is
intended to also apply to the subsequent lead. These operate just like
@var{place-notation} and @var{replace}, but on the subsequent lead, and always at the
begining of that lead. This use also depends upon the caller of @code{call-apply} making
correct use of its second return value.

If @var{replace} is not supplied or is @code{nil} it defaults to the number of changes
represented by the @var{place-notation}. If @var{offset} is not supplied or is @code{nil},
it defaults to @code{0} if @var{from-end} is false, and otherwise to the value of
@var{replace}, which may itself have been defaulted from the value of
@var{place-notation}. The default value of @var{from-end} is @code{t}. The default value
of @var{fraction} is @code{nil}. If @var{following} is supplied but
@var{following-replace} is not, @var{following-replace} defaults to the number of changes
represetned by @var{following}. If @var{following-replace} is supplied but @var{following}
is not, @var{following} defaults to @code{nil}.

A @code{parse-error} is signaled if either @var{place-notation} or @var{following} is
non-@code{nil} but not interpretable as place notation at the stage of @var{method}. A
@code{type-error} is signaled if @var{offset} is supplied and is neither @code{nil} nor a
non-negative integer; if @var{replace} is supplied and is neither @code{nil} nor a
non-negative integer; @var{fraction} is supplied and is neither @code{nil} nor a ratio
between @code{0} and @code{1}, exclusive; or if @var{following-replace} is supplied and is
neither @code{nil} nor a non-negative integer."
  (check-type* place-notation (or null string))
  (check-type* offset (or null (integer 0)))
  (check-type* fraction (or null (rational (0) (1))))
  (check-type* replace (or null (integer 0)))
  (check-type* following (or null string))
  (check-type* following-replace (or null (integer 0)))
  (when (equal place-notation "")
    (setf place-notation nil))
  (when (equal following "")
    (setf following nil))
  (iter (with primary-instance)
        (with following-instance)
        (with primary-vector := (make-array +call-changes-vector-length+ :initial-element nil))
        (with following-vector := (make-array +call-changes-vector-length+ :initial-element nil))
        (for stage :from +minimum-stage+ :to +maximum-stage+)
        (handler-case
            (let ((primary-changes (and place-notation (parse-place-notation place-notation
                                                                             :stage stage)))
                  (following-changes (and following (parse-place-notation following
                                                                          :stage stage))))
              (unless (or primary-instance following-instance)
                (setf primary-instance primary-changes)
                (setf following-instance following-changes))
              (setf (%get-call-changes primary-vector stage) primary-changes)
              (setf (%get-call-changes following-vector stage) following-changes))
          (parse-error ()))
        (finally
         (when (and place-notation (null primary-instance))
           (simple-parse-error "Call's place notation, ~A, cannot be interpreted at any stage."
                               place-notation))
         (when (and following (null following-instance))
           (simple-parse-error "Call's :FOLLOWING, ~A, cannot be interepted as place notation at any stage."
                               following))
         (unless replace
           (setf replace (length primary-instance)))
         (unless following-replace
           (setf following-replace (length following-instance)))
         (let ((result (make-call :place-notation place-notation
                                  :offset (or offset (if from-end replace 0))
                                  :from-end from-end
                                  :fraction fraction
                                  :replace replace
                                  :changes primary-vector
                                  :following (and (or following-supplied-p
                                                      following-replace-supplied-p)
                                                  (make-call :place-notation following
                                                             :offset 0
                                                             :from-end nil
                                                             :replace following-replace
                                                             :changes following-vector)))))
           (when (and (null primary-instance) (zerop replace)
                      (null following-instance) (zerop following-replace))
             (warn "Vacuous call contains no changes and zero length replacement: ~S." result))
           (return result)))))

(define-condition call-application-error (simple-error)
  ((call :initarg :call :reader call-application-error-call)
   (method :initarg :method :reader call-application-error-method)
   (details :initarg :details :reader call-application-error-details))
  (:documentation "Signaled when an anaomalous condition is detected while trying to
apply a @code{call} to a @code{method}. Contains three potentially useful slots
accessible with @code{call-application-error-call}, @code{call-application-error-method}
and @code{call-application-error-details}."))

(defun call-application-error (call method message &rest args)
  (let ((details (apply #'format nil message args)))
    (error 'call-application-error
           :format-control "Error when applying ~S to ~S: ~A."
           :format-arguments (list call method details)
           :details details
           :call call
           :method method)))

(defun call-apply (method &rest calls)
  "Applies zero or more @var{calls} to a lead of @var{method}. Returns two values, the
first a list of @code{row}s constituting the changes of the modified lead and the second
@code{nil} or a @code{call}, such that the call should be applied to the succeeding lead.
This second value is only non-nil for complex calls that affect two consecutive leads, as
are encountered in doubles variations. One or more of the @var{calls} may be @code{nil},
in which case they are ignored, just as if they had not been supplied. If no non-nil
@var{calls} are supplied returns a list of the changes constituting a plain lead of
@var{method}.

When multiple @var{calls} are supplied the indices of all are computed relative to the
length and position within the plain lead, before the application of any others of the
calls. For example, a half-lead call that replaces the 7th's in Cambridge Major continues
to replace that change even if an earlier call removes or adds several changes.

Signals a @code{type-error} if @var{method} is not a @code{method} or if any of the
@var{calls} are neither a @code{call} nor @code{nil}. Signals a @code{parse-error} if
@var{method} does not have its stage or place-notation defined. Signals a
@code{call-application-error} in any of the following circumstances: if the stage of
@var{method} is such that the place notation or following place notation of one or more of
the @var{calls} is inapplicable; if an attempt is made to apply a fractional lead
@code{call} where the denominator of the fraction does not evenly divide the lead length;
if the @code{call} would be positioned, or replace changes, that lie outside the lead; if
a @code{call} with following changes does not replace changes up to the end of the first
lead, or an attempt is made to applly two or more @code{call}s with following place
notation to the same lead."
  (let* ((stage (or (method-stage method)
                    (simple-parse-error "~S does not have its stage defined" method)))
         (result (cons nil (or (method-changes method)
                               (simple-parse-error "~S does not have its place notation defined"
                                                   method))))
         (p result)
         (i 0)
         (end (method-lead-length method)))
    (labels ((err (call message &rest args)
               (apply #'call-application-error call method message args))
             (start (call)
               (+ (cond ((call-fraction call)
                         (let ((result (* (method-lead-length method) (call-fraction call))))
                           (if (integerp result)
                               result
                               (err call "can't have a call at ~A of a lead length of ~A"
                                    (call-fraction call) (method-lead-length method)))))
                        ((call-from-end call) (method-lead-length method))
                        (t 0))
                  (funcall (if (call-from-end call) #'- #'identity) (call-offset call))))
             (capply (start call)
               (cond ((< start i) (err call "overlapping calls"))
                     ((> start end) (err call "call does not start within lead")))
               (setf p (nthcdr (- start i) p))
               (setf i start)
               (let* ((e (+ i (call-replace call)))
                      (changes (copy-list (get-call-changes call stage)))
                      (q (last changes)))
                 (when (> e end)
                   (err call "can't replace changes past the end of the lead"))
                 (setf (rest p) (nconc changes (rest (nthcdr (call-replace call) p))))
                 (setf p q)
                 (setf i e)
                 (= e end))))
      ;; (unless result
      ;;   (err nil "method is insufficiently defined to apply calls to"))
      (iter (for c :in calls)
            (when c
              (when (or (and (call-place-notation c)
                             (null (get-call-changes c stage)))
                        (when-let ((sub-call (call-following c)))
                          (and (call-place-notation sub-call)
                               (null (get-call-changes sub-call stage)))))
                (err c "call is not applicable to ~A methods" (stage-name stage)))
              (collect (cons (start c) c) :into alist))
            (finally (iter (with following := nil)
                           (for (s . c) :in (sort alist #'< :key #'car))
                           (for e := (capply s c))
                           (when following
                             (err c "can't have multiple calls with following changes"))
                           (setf following (call-following c))
                           (when (and following (not e))
                             (err c "following changes found where call does not replace to end of lead"))
                           (finally (return-from call-apply (values (rest result)
                                                                    following)))))))))


;;; Method lookup

(define-constant +method-library-magic-string+ "067F9B80-A01E-11E9-9AA9-C48E8FF8F245"
  ;; used to confirm a method library file is such
  :test #'equal)

(defconstant +default-method-libary-size+ 22000)

(define-constant +method-source+
    "https://cccbr.github.io/methods-library/xml/CCCBR_methods.xml.zip"
  :test #'equal)

(define-constant +method-source-entry+ "CCCBR_methods.xml" :test #'equal)

(defparameter *method-library-path*
    (merge-pathnames (make-pathname :name "method-library" :type "data")
                     (asdf:system-source-directory :roan)))

(defparameter *method-library* nil)

(defstruct method-library
  metadata
  no-name-count
  (methods (make-array +default-method-libary-size+ :adjustable t :fill-pointer 0))
  (rotation-keys nil)     ; if non-nil, a hash-table from canonical-rotation-keys to methods
  (additional-data nil))  ; if non-nil a hash-table from methods to property-lists

(defun lookup-methods (&key (name nil name-supplied)
                         (jump nil jump-supplied)
                         (differential nil differential-supplied)
                         (little nil little-supplied)
                         (class nil class-supplied)
                         (stage nil stage-supplied))
  "===summary===
Roan provides a library of method definitions, derived from the
@url{https://cccbr.github.io/methods-library/index.html,Central Council of Church Bell
Ringers Methods Library}. These are augmented with a handful of other methods not yet in
the CCCBR Library, jump methods and common alternative names for a few
methods (@ref{lookup-method-info}). As delivered with Roan this library is only up to
date as of the date a version of Roan was released. However, if a network connection is
available, the library can be updated to the most recent version made available by the
Council by using @code{update-method-library}. The Council typically updates their library
weekly.

The library can be interrogated with the @code{lookup-methods},
@code{lookup-method-by-title} and @code{lookup-methods-by-notation} functions.
Additional information such as dates and places of first peals containing the methods
is available for some of the methods using @code{lookup-method-info}.
===endsummary===
The @code{lookup-methods} function returns a list of named @code{method}s whose name,
classification and/or stage match those provided. If only a subset of these properties
are provided, the return list will contain all known methods that have the provided
ones.

If @var{name} is provided, it should be a string or @code{nil}, and all the methods
returned will have that name. The Central Council of Church Bell Ringers
@url{https://cccbr.github.io/method_ringing_framework/, Framework for Method
Ringing} (FMR), appendix C defines the form method names may take, and a mechanism for
comparing them that is more complex than simply comparing strings for equality. For
example, @code{\"London No.3\"} and @code{\"London no 3\"} are considered the same names.
The @code{lookup-methods} function uses this mechanism. @xref{comparable-method-name}.

The @var{name} may also contain @samp{*} wildcard characters. Such a wildcard matches a
series of zero or more consecutive characters. Since the @samp{*} is not a character
allowed in method names by the FMR there is no ambiguity: occurrences of @samp{*} in
@var{name} are always wildcard characters. Wildcards are applicable only to @var{name},
and not to any of the other arguments to @code{lookup-methods}.

If @var{stage} is provided, it should be a @code{stage}, that is a small integer. All the
methods that are returned will have that stage. While a @code{method} object can have
an indeterminate stage, represented by @code{nil}, all the methods returned by
@code{lookup-methods} will have a definite stage, and @code{nil} is not an allowed
value for the @var{stage} argument.

If @var{class} is provided, it should be @code{nil} or one of the keywords @code{:bob},
@code{:place}, @code{:surprise}, @code{:delight}, @code{:treble-bob},
@code{:treble-place}, @code{:alliance}, @code{:hybrid} or @code{:blank}. With the
exception of @code{:blank}, all the methods returned will have the specified class. The
value @code{:blank} matches either @code{nil}, meaning no explicit class, or
@code{:hybrid}; when writing a method's title according to the FMR the hybrid class and no
class are indistinguishable, since ``hybrid'' is not included in the title.

If supplied, the generalized booleans @var{little}, @var{differential} and @var{jump}
indicate that the returned methods should or should not have these properties. If these
parameters are not supplied all otherwise matching methods in the library will be returned
without regard to whether or not they have these properties.

If the title of a method is known, it can be found in the library by using
@code{lookup-method-by-title}. The @var{title} should be a string. If a @code{method} with
that title is in the library, it is returned. Otherwise @code{nil} is returned, unless the
generalized Boolean @var{errorp} it true (it is false by default), in which case an error
is signaled. In general there should never be two or more different methods in the library
with the same title. Matching on the title is done using the FMR's mechanism for comparing
names. Wildcards cannot be used with @code{lookup-method-by-title}.

If the place notation of a method is known, and its name in the library is sought,
@code{lookup-methods-by-notation} is available. The @var{notation-or-changes} should
be either a string, in which case it viewed as place notation, or a list of @code{rows},
representing changes all of the same stage. The @var{stage} should be a @code{stage}; if
not provided or @code{nil} the current value of @code{*default-stage*} is used. If
@var{notation-or-changes} is a list of changes, the value of @var{stage} is ignored,
the stage of those changes being used instead. Two lists are returned. The first is of
methods that have the provided place notation (or corresponding changes). The second is of
methods that are rotations of methods with the given place notation. Either or both lists
may be empty if no suitable methods are found in the library.

There is no guarantee of what order methods are in the lists returned by
@code{lookup-methods} or @code{lookup-methods-by-notation}. Instances of the ``same''
method returned by different invocations of these functions will typically not be
@code{eq}.

A @code{type-error} is signaled if @var{stage} is not a @code{stage} (or, in the case of
@code{lookup-methods-by-notation}, @code{nil}); @var{name} is not a string or @code{nil};
@var{notation-or-changes} is neither a string nor a non-empty list of @code{row}s;
@var{changes} is not a non-empty list of @code{row}s; or if @var{class} is not one the
allowed values. A @code{parse-error} is signaled if @var{notation-or-changes} is a string
and is not parseable as place notation at @var{stage}. An @code{error} is signaled if
@var{changes} is a list of @code{row}s, but they are not all of stage @var{stage} (or of
@code{*default-stage*} if @var{stage} is @code{nil}). A @code{method-library-error} is
signaled if the method library file cannot be read or is of the wrong format.
@example
@group
 (mapcar #'method-place-notation
         (lookup-methods :name \"Advent\"
                         :class :surprise
                         :stage 8))
     @result{} (\"36x56.4.5x5.6x4x5x4x7,8\")
 (mapcar #'method-title
         (lookup-methods :name \"london no 3\"
                         :class :surprise
                         :stage 10))
     @result{} (\"London No.3 Surprise Royal\")
 (method-place-notation
   (lookup-method-by-title \"Advent Surprise Major\"))
     @result{} \"36x56.4.5x5.6x4x5x4x7,8\"
 (lookup-methods :name \"No such method\")
     @result{} nil
@end group
@group
 (mapcar #'method-title
         (lookup-methods :name \"Cambridge*\"
                         :class :surprise
                         :stage 8))
     @result{} (\"Cambridge Blue Surprise Major\"
                \"Cambridge Surprise Major\"
                \"Cambridgeshire Surprise Major\")
@end group
@group
 (multiple-value-bind (n r)
     (lookup-methods-by-notation \"36x56.4.5x5.6x4x5x4x7,8\" 8)
       (list
         (mapcar #'method-title n)
         (mapcar #'method-title r)))
     @result{} ((\"Advent Surprise Major\") nil)
 (multiple-value-bind (n r)
     (lookup-methods-by-notation \"1.3\" 3)
       (list
         (mapcar #'method-title n)
         (mapcar #'method-title r)))
     @result{} ((\"Reverse Original Singles\")
                (\"Original Singles\"))
 (method-place-notation
   (lookup-method-by-title \"Original Singles\"))
     @result{} \"3.1\"
@end group
@end example"
  (check-type* name (or string null))
  (check-type* class (member nil :blank :bob :place :surprise :delight :treble-bob
                             :treble-place :alliance :hybrid :hunt))
  (check-type* stage (or stage null))
  (when (and stage-supplied (null stage))
    (error 'simple-type-error :format-control "If :stage is supplied to methods-lookup, it must be non-nil"))
  (read-method-library)
  (let ((methods (method-library-methods *method-library*))
        (has-name-start (method-library-no-name-count *method-library*)))
    (multiple-value-bind (prefix name-scanner impossible) (name-recognizers name)
      (unless impossible
        (labels ((matchp (comparable-name method)
                   (and (or (not name-supplied)
                            (if name-scanner
                                (ppcre:scan name-scanner comparable-name)
                                (equal prefix comparable-name)))
                        (or (null stage)
                            (eql (method-stage method) stage))
                        (or (not class-supplied)
                            (if (eq class :blank)
                                (member (method-class method) '(nil :hybrid :hunt))
                                (eq  (method-class method) class)))
                        (or (not little-supplied)
                            (if (method-little-p method) little (not little)))
                        (or (not differential-supplied)
                            (if (method-differential-p method) differential (not differential)))
                        (or (not jump-supplied)
                            (if (method-jump-p method) jump (not jump))))))
          (cond ((not name-supplied)
                 (iter (for (cn . m) :in-vector methods)
                       (when (matchp cn m)
                         (collect (copy-method m)))))
                ((null name)
                 (iter (for (cn . m) :in-vector methods :below has-name-start)
                       (when (matchp cn m)
                         (collect (copy-method m)))))
                ((null prefix)
                 (iter (for (cn . m) :in-vector methods :from has-name-start)
                       (when (matchp cn m)
                         (collect (copy-method m)))))
                (t (when-let ((mid (find-name-prefix prefix methods)))
                     (nconc (iter (for i :from (- mid 1) :downto 0)
                                  (for (cn . m) := (aref methods i))
                                  (while (starts-with-subseq prefix cn :test #'string-equal))
                                  (when (matchp cn m)
                                    (collect (copy-method m))))
                            (iter (for i :from mid :below (length methods))
                                  (for (cn . m) := (aref methods i))
                                  (while (starts-with-subseq prefix cn :test #'string-equal))
                                  (when (matchp cn m)
                                    (collect (copy-method m)))))))))))))

(defun name-recognizers (s)
  (cond ((null s) nil)
        ((zerop (length s)) (values nil nil t))
        ((not (find #\* s)) (or (comparable-method-name s) (values nil nil t)))
        ((ppcre:scan "^ |  | $" s) (values nil nil t))
        (t (let ((items nil) (first-time-p t) (prefix nil))
             (when (eql (char s 0) #\*)
               (push '(:greedy-repetition 0 nil :everything) items)
               (setf first-time-p nil))
             (ppcre:do-register-groups (content stars) ("([^*]+?)(\\*+|$)" s)
               (let ((fragment (comparableize-string content)))
                 (unless fragment
                   (return-from name-recognizers (values nil nil t)))
                 (when first-time-p
                   (setf prefix fragment)
                   (setf first-time-p nil))
                 (push fragment items)
                 (when (not (zerop (length stars)))
                   (push '(:greedy-repetition 0 nil :everything) items))))
             (values prefix (ppcre:create-scanner `(:sequence :start-anchor
                                                              ,@(nreverse items)
                                                              :end-anchor)))))))

(defun find-name-prefix (prefix methods)
  (iter (with start := 0)
        (with end := (length methods))
        (while (> end start))
        (for i := (+ start (floor (- end start) 2)))
        (for comparable-name := (car (aref methods i)))
        (cond ((starts-with-subseq prefix comparable-name :test #'string-equal)
               (return i))
              ((string-lessp prefix comparable-name)
               (setf end i))
              (t (setf start (+ i 1))))))

(defun lookup-method-by-title (title &optional errorp)
  "===merge: lookup-methods 1"
  (check-type* title string)
  (when (find #\* title)
    (error "Wildcards cannot be used when looking up a method by title (~S)" title))
  (multiple-value-bind (name jump differential little class stage)
      (parse-method-title title)
    (unless stage
      (setf stage *default-stage*))
    (let ((result (lookup-methods :name name :jump jump :differential differential
                                  :little little :class class :stage stage)))
      (unless class
        (unionf result (lookup-methods :name name :jump jump :differential differential
                                       :class (if jump :hunt :hybrid)
                                       :stage stage)))
      (cond ((rest result)
             (warn "Multiple methods found by lookup-method-by-title, only returning one of them (~{~A~^, ~})"
                   (mapcar #'method-title result)))
            ((first result))
            (errorp (error "Can't find ~S in the method library" title))))))

(defun lookup-methods-by-notation (notation-or-changes &optional (stage *default-stage*))
  "===merge: lookup-methods 2"
  (when (listp notation-or-changes)
    (check-type* (first notation-or-changes) row)
    (setf stage (stage (first notation-or-changes))))
  (let* ((canonical-place-notation (canonicalize-place-notation notation-or-changes :stage stage))
         (canonical-rotation-key (canonical-rotation-key (if (stringp notation-or-changes)
                                                             (parse-place-notation canonical-place-notation :stage stage)
                                                             notation-or-changes))))
    (read-method-library :load-rotations t)
    (iter (for m :in (gethash canonical-rotation-key (method-library-rotation-keys *method-library*)))
          (if (equal (method-place-notation m) canonical-place-notation)
              (collect (copy-method m) :into non-rotated)
              (collect (copy-method m) :into rotated))
          (finally (return (values non-rotated rotated))))))

(defun lookup-method-info (title-or-method key)
  "Roan's method library also stores metadata about many of the methods it contains. Each
kind of such metadata is described by a keyword, which is passed to this function as
@var{key}. The @var{title-or-method} may be a string or a @code{method}. If a string, it
is the title of the method about which the metadata is sought. If the metadata indicated
by @var{key} is available for the method it is returned; the type of the return value
depends upon the kind of metadata sought. If no such metadata is available, including
if @var{key} is a not yet supported type of metadata or if @code{title-or-method} does
not correspond to any method in the library, @code{nil} is returned.

Currently supported values for @var{key} are
@table @code
@item :first-towerbell-peal
Returns a string describing the first performance of the method on tower bells. No
distinction if made between ringing the method on its own or ringing it in spliced.

@item :first-handbell-peal
Returns a string describing the first performance of the method on hand bells. No
distinction if made between ringing the method on its own or ringing it in spliced.

@item :complib-id
Returns an integer, which is used to index information about the method on
@url{https://complib.org/,Composition Library}. This can also be used to distinguish those
methods added to those from the Central Council, as the added methods do not have a
@code{:complib-id}, while all those from the Council do.
@end table
Others may be added in future versions of Roan.

Signals a @code{type-error} if @var{title-or-method} is neither a string nor a
@code{method}, or if @var{key} is not a keyword.
@example
@group
 (lookup-method-info \"Advent Surprise Major\"
                     :first-towerbell-peal)
     @result{} \"1988-07-31 Boston, MA (Advent)\"
 (lookup-method-info
   (first (lookup-methods-by-notation \"36x56.4.5x5.6x4x5x4x7,8\"))
   :complib-id)
     @result{} 20042
 (lookup-method-info \"Advent Surprise Major\"
                     :no-such-info)
     @result{} nil
@end group
@end example"
  (check-type* title-or-method (or string method))
  (check-type* key keyword)
  (read-method-library :load-additional-data t)
  (let ((result (getf (gethash (if (stringp title-or-method)
                                   title-or-method
                                   (method-title title-or-method))
                               (method-library-additional-data *method-library*))
                      key)))
    (if (stringp result)
        (copy-sequence 'string result)
        result)))

(defun update-method-library (&optional force)
  "Queries the remote server containing the CCCBR's Methods Library. If that remote
file has changed since the one Roan's library was built from was downloaded, it fetches
the new one and uses it to build an updated Roan method library. If the generalized
boolean @var{force} is true it fetches the remote file and rebuilds Roan's library
without regard to whether the remote one has changed. If the library is updated, returns
an integer, the number of methods the updated library contains; if the library is not
updated because the remote version hasn't changed returns @code{nil}.

May signal any of a variety of file system or network errors if network access is not
available, or unreliable, or if there are other difficulties downloading and processing
the remote file."
  (when (or force
            (null (probe-file *method-library-path*))
            (not (equal (method-library-etag *method-library-path*)
                        (get-headers +method-source+))))
    (multiple-value-bind (zip-file etag last-modified)
        (download-zipped-methods +method-source+)
      (unwind-protect
           (let ((xml-file (unzip-xml-file zip-file)))
             (unwind-protect
                  (progn
                    (convert-xml-method-file xml-file *method-library-path* +method-source+ etag last-modified)
                    (read-method-library :force t)
                    (length (method-library-methods *method-library*)))
               (delete-file xml-file)))
        (delete-file zip-file)))))

(defun method-library-details ()
  "Returns eight values describing the current Roan method libary. All are strings. They
are:
@enumerate
@item
A description of the CCCBR Method Library, extracted from the file from which the Roan
library was constructed

@item
The date and time the file on the remote server was last modified, according to that
server.

@item
The ``entity tag'' (ETag) of the remote file, as provided by the server. This is an opaque
identifier that changes for each version of the remote file. Querying the current Etag is
how @code{update-method-library} decides whether or not the Roan method library needs
updating.

@item
The URL used to fetch the remote file from which the Roan library was built.

@item
The @var{source-id} provided in the remote file, that is a CCCBR version stamp.

@item
The date the CCCBR library was built, according to the contents of the file downloaded
from the remote server. This may or may not be the same as the date the file on the
remote server was last modified.

@item
A unique identifier for the current version of the Roan library. This will change
whenever the Roan library is rebuilt, even if the resulting contents are unchanged.

@item
The date and time the current version of the Roan library was built.
@end enumerate"
  (read-method-library)
  (let ((data (method-library-metadata *method-library*)))
    (apply #'values (mapcar #'(lambda (k) (copy-sequence 'string (getf data k)))
                            '(:description :last-modified :etag :source :source-id
                              :source-date :local-uuid :local-modified)))))

(defconstant +method-library-format-version+ 2)

(define-condition method-library-error (file-error)
  ((description :reader method-library-error-description :initarg :description))
  (:documentation "Signaled when a method library file cannot be read. Contains two
potentially useful slots accessible with @code{file-error-pathname} and
@code{method-library-error-description}.")
  (:report (lambda (condition stream)
             (format stream "Method library file ~S ~A"
                     (file-error-pathname condition)
                     (method-library-error-description condition)))))

(defun read-method-library (&key load-rotations load-additional-data force)
  ;; load-rotations and load-additional-data only apply if they are not already loaded
  (when (or force
            (null *method-library*)
            (and load-rotations (null (method-library-rotation-keys *method-library*)))
            (and load-additional-data (null (method-library-additional-data *method-library*))))
    (labels ((library-error (format &rest args)
               (error 'method-library-error
                      :pathname *method-library-path*
                      :description (format nil "~?" format args))))
      (let ((path (probe-file *method-library-path*)))
        (cond ((null path) (library-error "can't be found"))
              ((null *method-library*) (setf force t))
              ((not force)
               (when (method-library-rotation-keys *method-library*)
                 (setf load-rotations nil))
               (when (method-library-additional-data *method-library*)
                 (setf load-additional-data nil))))
        (with-roan-file (in path)
          (let ((metadata (read in)))
            (check-type* metadata cons)
            (unless (equal (pop metadata) +method-library-magic-string+)
              (library-error "does not appear to be formatted as a method library"))
            (let ((format-version (getf metadata :format-version)))
              (cond ((or (not format-version) (< format-version +method-library-format-version+))
                     (library-error "is in an older, no longer supported format (~S, ~S)"
                                    format-version +method-library-format-version+))
                    ((> format-version +method-library-format-version+)
                     (library-error "is in a newer format than that supported by this version of Roan (~S, ~S)"
                                    format-version +method-library-format-version+))))
            (when (and *method-library*
                       (not (equal (getf metadata :local-uuid)
                                   (getf (method-library-metadata *method-library*) :local-uuid))))
              (setf force t))
            (let ((lib (or (and (not force) *method-library*)
                           (make-method-library :metadata metadata))))
              (when load-rotations
                (setf (method-library-rotation-keys lib)
                      (make-hash-table :test #'equal
                                       :size (array-dimension (method-library-methods lib) 0))))
              (when load-additional-data
                (setf (method-library-additional-data lib)
                      (make-hash-table :test #'equalp
                                       :size (array-dimension (method-library-methods lib) 0))))
              (iter (for (name comparable-name classification notation rotation-key title . additional-data)
                         := (read in nil))
                    (for i :from 0)
                    (while classification) ; name can be nil
                    (counting (null name) :into n)
                    (for m := (if force
                                  (let ((new (make-instance 'method
                                                            :name name
                                                            :classification classification
                                                            :place-notation notation)))
                                    (vector-push-extend (cons comparable-name new) (method-library-methods lib))
                                    new)
                                  (cdr (aref (method-library-methods lib) i))))
                    (when load-rotations
                      (push m (gethash rotation-key (method-library-rotation-keys lib))))
                    (when load-additional-data
                      (setf (gethash title (method-library-additional-data lib)) additional-data))
                    (finally (setf (method-library-no-name-count lib) n)))
              (setf *method-library* lib))))))))

(defun method-library-etag (file)
  (if *method-library*
      (getf (method-library-metadata *method-library*) :etag)
      (let ((path (probe-file file)))
        (if (null path)
            (error 'method-library-error :pathname file :description "can't be found")
            (with-roan-file (in path)
              (let ((metadata (read in)))
                (check-type* metadata cons)
                (unless (equal (pop metadata) +method-library-magic-string+)
                  (error 'method-library-error
                         :pathname file
                         :description "does not appear to be formatted as a method library"))
                (getf metadata :etag)))))))

(defconstant +status-ok+ 200)

(defun download-zipped-methods (url)
  (multiple-value-bind (stream status headers uri socket-stream must-close reason)
      (drakma:http-request url :method :get :want-stream t)
    (declare (ignore uri socket-stream must-close))
    (unless (eql status +status-ok+)
      (error "Unexpected status of GET request on ~A: ~A (~D)." url reason status))
    (apply #'values
           (fad:with-output-to-temporary-file
               (out :element-type '(unsigned-byte 8)
                    :template (namestring (make-pathname :name "roan-methods-temp-%" :type "zip")))
             (let ((in (flex:flexi-stream-stream stream)))
               (iter (for b := (read-byte in nil))
                     (while b)
                     (write-byte b out))))
           (multiple-value-list (headers-extract headers)))))

(defun unzip-xml-file (zipfile)
  (fad:with-output-to-temporary-file
      (out :element-type '(unsigned-byte 8)
           :template (namestring (make-pathname :name "roan-methods-temp-%" :type "xml")))
    (zip:with-zipfile (z zipfile)
      (zip:zipfile-entry-contents
       (zip:get-zipfile-entry +method-source-entry+ z)
       out))))

(defun get-headers (url)
  (multiple-value-bind (reply status headers uri socket-stream must-close reason)
      (drakma:http-request url :method :head)
    (declare (ignore reply uri socket-stream must-close))
    (unless (eql status +status-ok+)
      (error "Unexpected status of HEAD request on ~A: ~A (~D)." url reason status))
    (headers-extract headers)))

(defun headers-extract (headers)
  (values (string-trim " \"" (drakma:header-value :etag headers))
          (drakma:header-value :last-modified headers)))

(define-constant +extra-methods+
    '(("New Grandsire Bob Doubles" "5.1.5.1,1.3")
      ("New Grandsire Bob Triples" "7.1.7.1.7.1,1.3")
      ("New Grandsire Bob Caters" "9.1.9.1.9.1.9.1,1.3")
      ("New Grandsire Bob Cinques" "E.1.E.1.E.1.E.1.E.1,1.3")
      ("Cloister Bob Doubles" "5.1.3.1.3.1")
      ("Cloister Bob Triples" "7.1.3.1.3.1")
      ("Cloister Bob Caters" "9.1.3.1.3.1")
      ("Cloister Bob Cinques" "E.1.3.1.3.1")
      ("St Helen's Bob Doubles" "5.1.3.1.3.1")
      ("St Helen's Bob Triples" "7.1.3.1.3.1")
      ("St Helen's Bob Caters" "9.1.3.1.3.1")
      ("St Helen's Bob Cinques" "E.1.3.1.3.1")
      ("Mersey Ferry Treble Jump Minor" "(13)4.(35)x(64)3.(42)x" t)
      ("Double Oxford Treble Jump Minor" "x(24)x(35)x5,2" t)
      ("Cambridge Treble Jump Minor" "x3x(24)x2x(35)x4x5,2" t)
      ("London Treble Jump Minor" "3x3.(24)x2x(35).4x4.3,2" t)
      ("Bourne Treble Jump Minor" "x3x(24)x2x(35)x34x3,2" t)
      ("Norwich Treble Jump Minor" "x34x(24)x2x(35)x34x1,6" t)
      ("York Treble Jump Minor" "x3x(24)x2x(35).4x4.3,2" t)
      ("Durham Treble Jump Minor" "x3x(24)x2x(35).4x34.1,2" t)
      ("Beverley Treble Jump Minor" "x3x(24)x2x(35).4x34.5,2" t)
      ("Surfleet Treble Jump Minor" "x3x(24)x2x(35).4x2.5,2" t)
      ("Wells Treble Jump Minor" "3x3.(24)x2x(35).4x34.1,2" t)
      ("Stedman Jump Triples" "7.(13).(13).(13).(13).(13).7.(31).(31).(31).(31).(31)")
      ("Jump Stedman Jump Doubles" "3.1.5.(31).(31).(31).(31).(31).5.3.1.3.1.3.5.(13).(13).(13).(13).(13).5.1.3.1")
      ("Coal Minor" "34x34.1x34x6")
      ("Boat Race Delight Minor" "x34x1x2x3x4x5,6"))
  :test #'equal)

(defvar *xml-file*)

(defun convert-xml-method-file (xml-file library-file url etag last-modified)
  (check-type* url string)
  (let* ((*xml-file* (merge-pathnames xml-file))
         (dom (get-single-element-by-tag-name (plump:parse *xml-file*) "collection"))
         (methods (make-array +default-method-libary-size+ :adjustable t :fill-pointer 0)))
    (dolist (method-set (plump:get-elements-by-tag-name dom "methodSet"))
      (let* ((properties (get-single-element-by-tag-name method-set "properties"))
             (stage (or (parse-integer (text (get-single-element-by-tag-name properties "stage"))
                                       :junk-allowed t)
                        (method-library-parse-error "non-numeric stage")))
             (prototype (parse-prototype stage properties)))
        (dolist (method (plump:get-elements-by-tag-name method-set "method"))
          (add-library-method methods prototype
                              (text (get-single-element-by-tag-name method "notation"))
                              (text (get-single-element-by-tag-name method "name"))
                              `(:complib-id ,(parse-method-id method)
                                            ,@(when-let ((p (parse-peal method "firstTowerbellPeal")))
                                                `(:first-towerbell-peal ,p))
                                            ,@(when-let ((p (parse-peal method "firstHandbellPeal")))
                                                `(:first-handbell-peal ,p)))))))
    (iter (for (title notation hunt) :in +extra-methods+)
          (for m := (method-from-title title notation))
          (when hunt
            (setf (method-class m) (if (method-jump-p m) :hunt :hybrid)))
          (add-library-method methods m))
    (with-roan-file (out library-file :direction :output :if-exists :supersede)
      (let ((*print-array* t)
            (*print-base* 10)
            (*print-length* nil)
            (*print-level* nil)
            (*print-pretty* nil)
            (*print-radix* nil)
            (*print-readably* nil))
        (format out ";;; Roan method library~%~S~%"
                `(,+method-library-magic-string+
                  :format-version ,+method-library-format-version+
                  :description ,(parse-method-library-description dom)
                  :local-uuid ,(format nil "~A" (uuid:make-v1-uuid))
                  :source-id ,(plump:attribute dom "uuid")
                  :etag ,etag
                  :last-modified ,last-modified
                  :source ,url
                  :source-date ,(plump:attribute dom "date")
                  :local-modified ,(local-time:format-timestring nil (local-time:now))))
        (iter (for m :in-vector (sort methods
                                      (lambda (x y)
                                        (declare (type (or string null) x y))
                                        (cond ((null y) nil)
                                              ((null x) t)
                                              (t (string-lessp x y))))
                                      :key #'second))
              (prin1 m out)
              (terpri out))))))

(defun add-library-method (vector method &optional place-notation (name nil name-supplied) info)
  (cond (name-supplied
         (when (equal name "") (setf name nil))
         (setf (method-name method) name))
        (t (setf name (method-name method))))
  (let ((changes (parse-place-notation (or place-notation (method-place-notation method))
                                       :stage (method-stage method)))
        (comparable-name (and name (comparable-method-name name))))
    (unless (or comparable-name (null name))
      (warn "Method name ~A does not appear to accord with the Framework for Method Ringing" name)
      (return-from add-library-method))
    (vector-push-extend
     `(,name
       ,(and name (comparable-method-name name))
       ,(%method-classification method)
       ,(canonicalize-place-notation changes)
       ,(canonical-rotation-key changes)
       ,(method-title method)
       ,@info)
     vector)))

(defun method-library-parse-error (&optional msg &rest args)
  (simple-parse-error "Error reading method libary ~S, ~:[is it perhaps the wrong format?~;~:*~?~]"
                      *xml-file* msg args))

(defun get-single-element-by-tag-name (node tag &optional (error-if-absent t))
  (unless node
    (method-library-parse-error))
  (let ((elements (plump:get-elements-by-tag-name node tag)))
    (when (or (rest elements) (and error-if-absent (not (first elements))))
      (method-library-parse-error))
    (first elements)))

(defun text (node)
  (if (plump:element-p node)
      (plump:text node)
      (method-library-parse-error)))

(defun parse-prototype (stage properties)
  (let ((classification (get-single-element-by-tag-name properties "classification")))
    (method :stage stage
            :little (plump:attribute classification "little")
            :differential (plump:attribute classification "differential")
            :jump (plump:attribute classification "jump")
            :class (class-from-name (text classification)))))

(defun parse-method-library-description (dom)
  (let* ((collection-name (get-single-element-by-tag-name dom "collectionName"))
         (notes (plump:next-element collection-name)))
    (or (and (string-equal (plump:tag-name notes) "notes")
             (format nil "~A. ~A" (text collection-name) (text notes)))
        (method-library-parse-error "unexpected format, bad description"))))

(defun parse-method-id (method)
  (or (ppcre:register-groups-bind (s) ("m(\\d+)" (plump:attribute method "id"))
        (and s (parse-integer s)))
      (method-library-parse-error "unknown id format (~S)" (plump:attribute method "id"))))

(defun parse-peal (xml tag)
  (when-let ((peal (first (last (plump:get-elements-by-tag-name xml tag)))))
    (let ((date (text (get-single-element-by-tag-name peal "date")))
          (town (get-single-element-by-tag-name peal "town" nil))
          (county (get-single-element-by-tag-name peal "county" nil))
          (region (get-single-element-by-tag-name peal "region" nil))
          (building (get-single-element-by-tag-name peal "building" nil))
          (address (get-single-element-by-tag-name peal "address" nil)))
      (labels ((text-list (&rest args)
                 (iter (for a :in args)
                       (when a
                         (collect (text a))))))
        (format nil "~A ~{~A~^, ~}~@[ (~{~A~^, ~})~]"
                date
                (text-list town county region)
                (text-list building address))))))


;;; Blueline drawing

(define-constant +blueline-column-spacing+ 10)
(define-constant +blueline-dot-size+ 2.2)
(define-constant +blueline-figures-size+ "14px" :test #'equal)

(define-constant +blueline-first-hunt-bell-colors+
    '(((nil . nil) . "rgb(235,173,173)") ((nil . t) . "rgb(214,92,92)")
      ((t . nil) . "rgb(255,153,153)") ((t . t) . "rgb(255,51,51)"))
  :test #'equalp)
(define-constant +blueline-first-hunt-bell-width+ 0.8)
(define-constant +blueline-horizontal-increment+ 16)
(define-constant +blueline-horizontal-margin+ 14)
(define-constant +blueline-inter-cycle-gap+ 16)
(define-constant +blueline-labels-circle-radius+ 10)
(define-constant +blueline-labels-circle-x-offset+ 4.5)
(define-constant +blueline-labels-circle-y-offset+ -4.5)
(define-constant +blueline-labels-left-margin+ 8)
(define-constant +blueline-labels-right-margin+ 40)
(define-constant +blueline-labels-size+ "80%" :test #'equal)
(define-constant +blueline-labels-vertical-offset+ 5)
(define-constant +blueline-no-figure-ratio+ 0.5)
(define-constant +blueline-place-notation-character-width+ 6)
(define-constant +blueline-place-notation-margin+ 9)
(define-constant +blueline-place-notation-offset+ 10)
(define-constant +blueline-place-notation-size+ "75%" :test #'equal)
(define-constant +blueline-plus-length+ 3.2)
(define-constant +blueline-vertical-margin+ 16)
(define-constant +blueline-vertical-offset+ 14)
(define-constant +blueline-working-bell-color+ "rgb(51,51,255)" :test #'equalp)
(define-constant +blueline-working-bell-width+ 1.4)

(define-constant +blueline-figures-x-offset+ (round +blueline-horizontal-increment+ 4))
(define-constant +blueline-figures-y-offset+ (round (* +blueline-vertical-offset+ 0.3)))

(defun bell-list-p (x)
  (and (listp x) (every (lambda (b) (typep b 'bell)) x)))

(defparameter *blueline-default-parameters* '())

(defun blueline (destination method &rest keys &key layout hunt-bell working-bell
                                                 figures place-notation place-bells)
  " Draws the blue line of @var{method} as a Scalable Vector Graphics (SVG) image. The
@var{method} should have its stage and place notation set. While Roan only writes SVG
format images, many other pieces of software, such as
@url{https://imagemagick.org/,ImageMagick}, are able to convert SVG images to other
formats.

The @var{destination} can be
@itemize
@item
A text stream, open for writing:
the SVG will be written to this stream, and the stream is returned as the value of the
call to @code{blueline}.

@item
The symbol @code{t}:
the SVG will be written to @code{*standard-output*}, and the value of
@code{*standard-output*} is returned as the value of the
call to @code{blueline}.

@item
A pathname:
an SVG file will be written to this pathname, which will be opened with
@code{if-exists :supersede}, and the truename of the resulting file is returned.

@item
A string with a fill pointer:
the SVG will be appended to this string, as by @code{vector-push-extend}, which is
returned.

@item
The symbol @code{nil}:
the SVG will be written to a new string, which is returned.
@end itemize

Several keyword parameters can be used to control details of the image produced
@table @var
@item layout
Controls the distribution of leads into columns. For differentials, or methods with
multiple, equal length cycles of working bells, each cycle always starts a new column.
Within a cycle the value of @var{layout} controls the number of leads in a column. If it
is a non-negative integer, this is the maximum number of rows in a column; though if the
lead length exceeds this value each column will contain one lead. If @code{nil} this is
no limit to the number of leads in a column, each cycle of working bells then filling a
column.The special value @code{:grid} may also be supplied, in which case only a single
column is used for a single lead, with all the bells blue lines combined into it as a grid.
The default value for @var{layout} is @code{100}.

@item hunt-bell
Controls which hunt bells are displayed specially. Those not displayed specially, are
treated as working bells. If a @code{bell}, that is, a small, non-negative integer less
than the stage of @var{method}, this is the hunt bell displayed specially; a list of
@code{bell}s may also be supplied, for multiple hunt bells. If a supplied @code{bell} is
not actually a hunt bell of @var{method} it is ignored. The keyword @code{first} is
equivalent to supplying whatever the smallest hunt bell of @var{method} is. The keyword
@code{:all} is equivalent to supplying a list of all the hunt bells of @var{method}. The
keyword @code{:working} treats all of the hunt bells as working bells. If @var{hunt-bell}
is @code{nil} no hunt bells are displayed. The default value for @var{hunt-bell} is
@code{:first}.

@item working-bell
Controls which working bell of each cycle is drawn first, the others following on in the
order in which they are rung. This can be a @code{bell}, or a list thereof, or one of the
keywords @code{:natural}, @code{:largest} or @code{:smallest}. If @code{:natural} for
each cycle the largest bell that makes a place across the lead end is chosen; if there
is no such bell in a cycle the largest bell in that cycle is used. For methods with
Grandsire-like palindromic symmetry the first row of the lead is used instead of the
lead end. The default value for @var{working-bell} is @code{:natural}.

@item figures
If non-null figures will also be drawn, in addition to the blue line. If @code{t} they will
be drawn for all leads. If @code{:lead} only for the first lead of each cycle. If
@code{:half} and the @var{method} has the usual palindromic symmetry around the half lead,
with one additional change at the lead end, they will only be drawn for the first
half-lead; otherwise @code{:half} is equivalent to @code{:lead}. If @code{:head} the
figures will only be drawn for the first lead head in each column. The default value for
@var{figures} is @code{nil}.

@item place-notation
if non-null the place notation will be drawn to the left of the blue lines. If @code{t} it
will be drawn for the first lead in each column. If @code{:lead} it will only be drawn for
the first columnn. If @code{:half} and the @var{method} has the usual palindromic symmetry
around the half lead, with one additional change at the lead end, it will only be drawn
for the first half lead, plus at the lead end; otherwise @code{:half} is equivalent to
@code{:lead}. The default value for @code{place-notation} is @code{nil}.

@item place-bells
May have a value of @code{nil}, @code{:dot} or @code{:label}. If non-null dots are drawn
where each place bell starts, and if @code{:label} a label is drawn to the right of the
blue line at each place bell's start. The default value for @var{place-bells} is
@code{:label}.
@end table

For an example, execute something like the following, and open the resulting file in a
browser:
@example
@group
 (blueline #P\"/tmp/bastow.svg\"
           (lookup-method-by-title \"Bastow Little Bob Minor\")
           :layout 12
           :figures :lead
           :place-notation :half)
@end group
@end example

Default values for the keyword arguments to this function can be set by assigning a
property list of keywords and values to the variable @code{*blueline-default-parameters*}.
@example
@group
 (equal
   (blueline nil
             (lookup-method-by-title \"Advent Surprise Major\")
             :layout nil
             :figures t
             :place-notation :lead)
   (let ((*blueline-default-parameters*
            '(:layout nil :figures t :place-notation :lead)))
     (blueline nil (lookup-method-by-title \"Advent Surprise Major\"))))
   @result{} t
@end group
@end example


Signals a @code{type-error} if @var{destination} is not a stream, pathname, string with a
fill pointer or one of the symbols @code{t} or @code{nil}; if @var{method} is not a
@code{method}; if @var{layout} is not non-negative integer, @code{nil} or the keyword
@code{:grid}; if @var{hunt-bell} is not a @code{bell}, list of bells, @code{nil} or one
one of the keywords @code{:first}, @code{:all} or @code{:working}. if @var{working-bell}
is not a @code{bell}, list of bells, or one of the symbols @code{:natural},
@code{:largest} or @code{smallest}; if @var{figures} is not one of the keywords
@code{:none}, @code{:head}, @code{:half}, @code{:lead} or @code{:always}; if
@var{place-notation} is not one of the keywords @code{:none}, @code{:half}, @code{:lead}
or @code{:always}; or if @var{place-bells} is not @code{nil} or one of the keywords
@code{:dot} or@code{:label}. Signals a @code{no-place-notation-error} if @var{method}
doesn't have both its stage and place notation set. Can signal various errors if an I/O
error occurs trying to write to a stream or create a file."
  (declare (ignorable layout hunt-bell working-bell figures place-notation place-bells))
  (apply (lambda (&key
                    (layout 100)
                    (hunt-bell :first)
                    (working-bell :natural)
                    (figures nil)
                    (place-notation nil)
                    (place-bells :label))
           (unless (method-changes method)
             (error 'no-place-notation-error :method method))
           (check-type* layout (or (integer 0) null (eql :grid)))
           (check-type* hunt-bell (or bell (satisfies bell-list-p) (member nil :first :all :working)))
           (check-type* working-bell (or bell (satisfies bell-list-p) (member :natural :largest :smallest)))
           (check-type* figures (member nil t :head :half :lead))
           (check-type* place-notation (member nil t :half :lead))
           (check-type* place-bells (member nil :dot :label))
           (labels ((dispatch (stream)
                      (%blueline stream method layout hunt-bell working-bell figures
                                 place-notation place-bells)))
             (etypecase destination
               (stream (dispatch destination))
               ((eql t) (dispatch *standard-output*))
               (null (with-output-to-string (s) (dispatch s)))
               (string (with-output-to-string (s destination) (dispatch s)) destination)
               (pathname (with-roan-file (s destination :direction :output :if-exists :supersede)
                           (dispatch s)
                           (truename s))))))
         (append keys *blueline-default-parameters*)))

(defvar *blueline-stream*)
(defvar *blueline-method*)
(defvar *blueline-row-height*)
(defvar *blueline-figures-lead-head*)

(defun %blueline (stream method layout hunt-bell working-bell figures place-notation place-bells)
  (when (integerp hunt-bell)
    (setf hunt-bell (list hunt-bell)))
  (when (integerp working-bell)
    (setf working-bell (list working-bell)))
  (when (and (or (eq figures :half) (eq place-notation :half))
             (or (method-contains-jump-changes-p method)
                 (oddp (method-lead-length method))
                 (not (test-ordinary-palindrome (nbutlast (method-changes method))
                                                (- (/ (method-lead-length method) 2) 1)))))
    (when (eq figures :half)
      (setf figures :lead))
    (when (eq place-notation :half)
      (setf place-notation :lead)))
  (when (and (eq layout :grid) (eq place-notation t))
    (setf place-notation :lead))
  (let* ((gridp (eq layout :grid))
         (*blueline-method* method)
         (*blueline-figures-lead-head* (rounds (method-stage method)))
         (*blueline-row-height* (ceiling (* (if (or figures place-notation) 1 +blueline-no-figure-ratio+)
                                            +blueline-vertical-offset+)))
         (max-leads-per-column (cond ((null layout) (floor (method-course-length method)
                                                           (method-lead-length method)))
                                     (gridp 1)
                                     (t (max (floor layout (method-lead-length method)) 1))))
         (cycles (if (and gridp (not hunt-bell))
                     (list (iter (for i :below (method-stage method)) (collect i)))
                     (mapcar (rcurry #'rotate-cycle working-bell) (method-working-bells method))))
         (height 0)
         (width 0)
         (x +blueline-horizontal-margin+))
    (when gridp
      (unless hunt-bell
        (setf hunt-bell :first))
      (setf place-bells nil))
    (multiple-value-bind (primary-hunts secondary-hunts) (partition-hunt-bells hunt-bell)
      (let* ((columns (with-output-to-string (*blueline-stream*)
                        (dolist (c (sort (append (mapcar #'list secondary-hunts) cycles)
                                         (if (member working-bell '(:natural :largest)) #'> #'<)
                                         :key #'first))
                          (iter (for p :on c :by (curry #'nthcdr max-leads-per-column))
                               (while p)
                               (if-first-time nil (unless gridp (incf x +blueline-column-spacing+)))
                               (for (values x-after-pn new-x h)
                                    := (draw-column x (first c) p
                                                    (min (length p) max-leads-per-column)
                                                    primary-hunts figures place-notation
                                                    place-bells))
                               (cond (gridp (setf figures nil) ; only draw them once in the grid
                                            (setf primary-hunts nil)
                                            (setf x x-after-pn))
                                     (t (setf x new-x)))
                               (maxf width new-x)
                               (maxf height h)
                               (when (member figures '(:half :lead))
                                 (setf figures nil))
                               (when (member place-notation '(:half :lead))
                                 (setf place-notation nil))
                               (finally (incf width +blueline-horizontal-margin+))))
                          (incf x +blueline-inter-cycle-gap+))))
        (format stream
                "<?xml version='1.0' encoding='UTF-8' standalone='no'?>
<svg xmlns='http://www.w3.org/2000/svg' preserveaspectratio='xMidYMid meet' height='~D' width='~D'>
<style>.blueline{fill:none;stroke:~A;stroke-width:~D;stroke-linecap:round;stroke-linejoin:miter;}
.hunt{stroke:~A;stroke-width:~D;}
.dot{fill:black;stroke:none;}
.figure{font-family:sans-serif;font-size:~A;}
.notation{font-family:sans-serif;font-size:~A;font-weight:lighter;font-style:italic;fill:dimgray;}
.label{font-family:sans-serif;font-size:~A;font-weight:bolder;fill:slategray;}
circle{stroke:slategray;}</style>
<rect height='~D' width='~D' fill='white'/>
~A</svg>~%"
                height
                width
                +blueline-working-bell-color+
                +blueline-working-bell-width+
                (cdr (assoc (cons (not (null gridp)) (not (null figures)))
                            +blueline-first-hunt-bell-colors+
                            :test #'equal))
                (if gridp +blueline-working-bell-width+ +blueline-first-hunt-bell-width+)
                +blueline-figures-size+
                +blueline-place-notation-size+
                +blueline-labels-size+
                height
                width
                columns))))
  stream)

(defun partition-hunt-bells (target)
  (when-let ((all (method-hunt-bells *blueline-method*)))
    (case target
      (:first (values (list (first all)) (rest all)))
      (:all (values all nil))
      (:working (values nil all))
      ((nil) (values nil nil))
      (t (let ((result (intersection all target)))
           (values result (set-difference all result)))))))

(defun rotate-cycle (cycle target)
  (iter (with start := (case target
                         (:largest (apply #'max cycle))
                         (:smallest (apply #'min cycle))
                         (:natural (iter (with apex := (let* ((changes (method-changes *blueline-method*))
                                                              (length (length changes))
                                                              (segment
                                                               (and (evenp length)
                                                                    (> length 2)
                                                                    (split-palindromic-changes
                                                                     changes
                                                                     length
                                                                     (method-jump-p *blueline-method*)))))
                                                         (first (if (and segment (null (rest segment)))
                                                                    segment
                                                                    (last changes)))))
                                         (with sorted := (sort (copy-seq cycle) #'<))
                                         (for b :in sorted)
                                         (when (eql (bell-at-position apex b) b)
                                           (return b))
                                         (finally (return (first (last sorted))))))
                         (t (apply #'max (or (intersection cycle target) cycle)))))
        (for x :on cycle)
        (for p :previous x)
        (when (eql (first x) start)
          (when p
            (setf (rest p) nil)
            (setf (rest (last x)) cycle))
          (return x))))

(defun draw-column (x bell starts leads hunts figures place-notation place-bells)
  (let* ((lead-height (* (method-lead-length *blueline-method*) *blueline-row-height*))
         (holes (cons bell hunts))
         (label-left (+ x
                        (* +blueline-horizontal-increment+ (method-stage *blueline-method*))
                        +blueline-labels-left-margin+))
         (label-right label-left))
    (when place-notation
      (let ((w (draw-place-notation x
                                    (+ +blueline-vertical-margin+)
                                    (if (eq place-notation t) leads 1)
                                    (eq place-notation :half))))
        (incf x (+ w +blueline-place-notation-margin+))
        (incf label-left w)
        (incf label-right w)))
    (when figures
      (iter (for started :initially nil :then t)
            (for i :below (if (eq figures t) leads 1))
            (draw-figures x
                          (+ +blueline-vertical-margin+ (* i lead-height))
                          figures holes (not started))))
    (dolist (b hunts)
      (draw-line b x +blueline-vertical-margin+ t leads))
    (draw-line (first starts) x +blueline-vertical-margin+ nil leads)
    (when place-bells
      (iter (for i :from 0 :to leads)
            (for p :in (append starts (list bell)))
            (for y := (+ +blueline-vertical-margin+ (* i lead-height)))
            (when (eq place-bells :label)
              (maxf label-right (draw-label p label-left y)))
            (if-first-time (when figures (next-iteration)))
            (draw-dot (+ x (* p +blueline-horizontal-increment+)) y)))
    (values x label-right (+ (* 2 +blueline-vertical-margin+) (* leads lead-height)))))

(defun blueline-format (string &rest args)
  (apply #'format *blueline-stream* string args))

(defun draw-place-notation (x y leads half-lead-only)
  (incf y +blueline-place-notation-offset+)
  (iter (with len := (method-lead-length *blueline-method*))
        (with half := (and half-lead-only (/ len 2)))
        (for c :in (method-changes *blueline-method*))
        (for p := (place-notation-string (list c)))
        (collect p :into pn)
        (maximize (length p) :into n)
        (finally (return (iter (with wid := (* n +blueline-place-notation-character-width+))
                               (initially (incf x wid))
                               (repeat leads)
                               (iter (for p :in pn)
                                     (for i :from 1)
                                     (when (or (null half) (eql i len) (<= i half))
                                       (blueline-format "<text class='notation' x='~D' y='~D' text-anchor='end'>~A</text>~%"
                                                        x y p))
                                     (incf y *blueline-row-height*))
                               (return (+ wid +blueline-place-notation-margin+)))))))

(defun draw-figures (x y where holes first)
  (decf x +blueline-figures-x-offset+)
  (incf y +blueline-figures-y-offset+)
  (labels ((draw (row &optional (skip-holes t))
             (when row
               (iter (for b :in-vector (row-bells row) :with-index i)
                     (unless (and skip-holes (member b holes))
                       (blueline-format "<text class='figure' x='~D' y='~D'>~A</text>"
                                        (+ x (* i +blueline-horizontal-increment+))
                                        y
                                        (bell-name b))))
               (blueline-format "~%"))
             (incf y +blueline-vertical-offset+)))
    (draw (and first *blueline-figures-lead-head*) nil)
    (unless (eq where :head)
      (iter (for r :in (rest (method-plain-lead *blueline-method*)))
            (repeat (if (eq where :half)
                        (- (ceiling (method-lead-length *blueline-method*) 2) 1)
                        (method-lead-length *blueline-method*)))
            (draw (permute *blueline-figures-lead-head* r))
            (finally (unless (eq where :half)
                       (draw (permute *blueline-figures-lead-head*
                                      (method-lead-head *blueline-method*))))))))
  (setf *blueline-figures-lead-head*
        (permute *blueline-figures-lead-head* (method-lead-head *blueline-method*))))

(defun draw-line (bell x y hunt-p leads)
  (let ((curr nil) (prev nil) (in-polyline nil))
    (labels ((loc (p)
               (+ x (* p +blueline-horizontal-increment+)))
             (inc-row ()
               (incf y *blueline-row-height*))
             (start ()
               (blueline-format "<polyline class='blueline~:[~; hunt~]' points='" hunt-p)
               (setf in-polyline t))
             (point (p &optional (space t))
               (blueline-format "~:[~; ~]~D,~D" space (loc p) y)
               (inc-row))
             (end ()
               (blueline-format "'></polyline>~%")
               (setf in-polyline nil))
             (line (x1 y1 x2 y2)
               (blueline-format "<line class='blueline~:[~; hunt~]' x1='~D' y1='~D' x2='~D' y2='~D'></line>"
                                hunt-p x1 y1 x2 y2))
             (single (p)
               (let ((c (loc p)))
                 (line (- c +blueline-plus-length+) y (+ c +blueline-plus-length+) y)
                 (line c (- y +blueline-plus-length+) c (+ y +blueline-plus-length+)))
               (blueline-format "~%")
               (inc-row))
             (next-row (b)
               (shiftf prev curr b)
               (when prev
                 (cond ((not (<= -1 (- curr prev) 1)) (if in-polyline (end) (single prev)))
                       (in-polyline (point curr))
                       (t (start)
                          (point prev nil)
                          (point curr))))))
      (iter (with rows := (method-plain-lead *blueline-method*))
            (repeat leads)
            (iter (for r :in rows)
                  (next-row (position-of-bell bell r)))
            (setf bell (position-of-bell bell (method-lead-head *blueline-method*)))
            (finally (next-row bell)
                     (if in-polyline (end) (single curr)))))))

(defun draw-dot (x y)
  (blueline-format "<circle cx='~D' cy='~D' r='~D' class='dot'></circle>~%" x y +blueline-dot-size+))

(defun draw-label (start x y)
  (blueline-format "<text class='label' x='~D' y='~D'>~A</text>~
                    <circle cx='~D' cy='~D' r='~D' stroke='black' stroke-width='1.4' fill='none'></circle>~%"
                   x
                   (+ y +blueline-labels-vertical-offset+)
                   (bell-name start)
                   (+ x +blueline-labels-circle-x-offset+)
                   (+ y +blueline-labels-vertical-offset+ +blueline-labels-circle-y-offset+)
                   +blueline-labels-circle-radius+)
  (+ x +blueline-labels-right-margin+))
