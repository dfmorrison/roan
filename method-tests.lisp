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

(in-package :roan/test)

#+SBCL
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(defparameter *no-such-keyword* '(:no-such-keyword t))

(define-test test-comparable-names ()
  (labels ((test-name (expected &rest names)
             (dolist (n names)
               (assert-equal expected (comparable-method-name n)))))
    (test-name "advent" "Advent" "ADVENT" "aDvEnT" "advent")
    (test-name "new cambridge" "New Cambridge" "new Cambridge" "New cambridge" "New Cambridge")
    (test-name "e mc2" "E=mc²" "e & MC₂")
    (test-name "makcentm" "mäkčeň™")
    (test-name nil "ελληνικά" "Too  Many Spaces" " Leading Space" "Trailing " "")
    (test-name "x tm01234567890123456789" "X !\"&'(),-./=%?£$€™⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉")
    (let ((s (format nil "~120{x~}" '(ignore))))
      (test-name s s (string-upcase s) (string-capitalize s)))
    (test-name nil (format nil "~121{x~}" '(ignore))))
  (assert-error 'type-error (comparable-method-name 'Cambridge))
  (assert-error 'type-error (comparable-method-name #\C))
  (assert-error 'type-error (comparable-method-name nil))
  (assert-error 'type-error (comparable-method-name 40))
  (assert-error 'type-error (comparable-method-name '("Cambridge"))))

(defun equal-methods-p (m1 m2)
  (and (equal (method-name m1) (method-name m2))
       (eql (%method-classification m1) (%method-classification m2))
       (equal (method-place-notation m1) (method-place-notation m2))))

(define-test test-method ()
  (labels ((test-m (m s n c p &optional lt d j)
             (assert-eql s (method-stage m))
             (assert-equal n (method-name m))
             (assert-eq c (method-class m))
             (assert-equal p (method-place-notation m))
             (assert-eq lt (method-little-p m))
             (assert-eq d (method-differential-p m))
             (assert-eq j (method-jump-p m))))
    (let ((*default-stage* 17))
      (test-m (method) 17 nil nil nil)
      (test-m (method :name "foo") 17 "foo" nil nil)
      (test-m (method :stage 9) 9 nil nil nil)
      (test-m (method :stage nil) nil nil nil nil)
      (test-m (method :class nil) 17 nil nil nil)
      (test-m (method :class :hybrid) 17 nil :hybrid nil)
      (test-m (method :place-notation "x3") 17 nil nil "x3")
      (test-m (method :little t) 17 nil nil nil t)
      (test-m (method :differential t) 17 nil nil nil nil t)
      (test-m (method :jump t) 17 nil nil nil nil nil t)
      (test-m (method :name "fooX"
                      :class :treble-place
                      :stage 7
                      :jump t
                      :differential t
                      :little t)
              7 "fooX" :treble-place nil t t t)
      (test-m (method :name "Cambridge"
                      :class :surprise
                      :stage 6
                      :place-notation "x3x4x2x3x4x5,2")
              6 "Cambridge" :surprise "x3x4x2x3x4x5,2"))
    (assert-error 'type-error (method :name t :stage 8))
    (assert-error 'type-error (method :class 17 :stage 8))
    (assert-error 'type-error (method :stage :foo))
    (assert-error 'type-error (method :place-notation 0 :stage 8))
    (let ((method (method :stage 8)))
      (test-m method 8 nil nil nil)
      (assert-equal "Pudsey" (setf (method-name method) "Pudsey"))
      (test-m method 8 "Pudsey" nil nil)
      (assert-eql 10 (setf (method-stage method) 10))
      (test-m method 10 "Pudsey" nil nil)
      (setf (method-stage method) 8 (method-place-notation method) "x5x6x3x4x5x6x7,2")
      (test-m method 8 "Pudsey" nil "x5x6x3x4x5x6x7,2")
      (assert-eq :surprise (setf (method-class method) :surprise))
      (test-m method 8 "Pudsey" :surprise "x5x6x3x4x5x6x7,2")
      (assert-eq nil (setf (method-name method) nil))
      (test-m method 8 nil :surprise "x5x6x3x4x5x6x7,2")
      (setf (method-stage method) nil
            (method-place-notation method) nil
            (method-class method) nil)
      (test-m method nil nil nil nil)
      (assert-eq t (setf (method-little-p method) :yes))
      (test-m method nil nil nil nil t nil nil)
      (assert-eq t (setf (method-differential-p method) 1))
      (test-m method nil nil nil nil t t nil)
      (assert-eq t (setf (method-jump-p method) t))
      (test-m method nil nil nil nil t t t)
      (assert-eql nil (setf (method-differential-p method) nil))
      (test-m method nil nil nil nil t nil t)
      (assert-eql nil (setf (method-jump-p method) nil))
      (test-m method nil nil nil nil t nil nil)
      (assert-eql nil (setf (method-little-p method) nil))
      (test-m method nil nil nil nil nil nil nil)
      (assert-error 'type-error (setf (method-name method) 18))
      (assert-error 'type-error (setf (method-name method) :foo))
      (assert-error 'type-error (setf (method-name method) t))
      (assert-error 'type-error (setf (method-name method) '("Cambridge")))
      (assert-error 'type-error (setf (method-class method) 1))
      (assert-error 'type-error (setf (method-class method) :foo))
      (assert-error 'type-error (setf (method-class method) t))
      (assert-error 'type-error (setf (method-class method) "Surprise"))
      (assert-error 'type-error (setf (method-class method) '(:surprise)))
      (assert-error 'type-error (setf (method-stage method) (- +minimum-stage+ 1)))
      (assert-error 'type-error (setf (method-stage method) (+ +maximum-stage+ 1)))
      (assert-error 'type-error (setf (method-stage method) :minor))
      (assert-error 'type-error (setf (method-stage method) t))
      (assert-error 'type-error (setf (method-stage method) "Major"))
      (assert-error 'type-error (setf (method-stage method) '(8))))
    ;; (labels ((test-print (s1 sc &rest args)
    ;;            (let ((m (apply #'method args)))
    ;;              (assert-equalp s1 (subseq (format nil "~S" m) 0 (length s1)))
    ;;              (assert-equal sc (format nil "~A" m)))))
    ;;   (test-print "#<METHOD Advent Surprise Major 36x56.4.5x5.6x4x5x4x7,8 "
    ;;               "Advent Surprise Major"
    ;;               :name "Advent" :class :surprise :stage 8
    ;;               :place-notation "36x56.4.5x5.6x4x5x4x7,8")
    ;;   (test-print "#<METHOD Flopsis Caters " "Flopsis Caters"
    ;;               :name "Flopsis" :stage 9)
    ;;   (test-print "#<METHOD Jump Differential Little " "Unnamed Jump Differential Little"
    ;;               :differential t :little t :stage nil :jump t)
    ;;   (test-print "#<METHOD Grandsire Cinques" "Grandsire Cinques"
    ;;               :name "Grandsire" :class :bob :stage 11)
    ;;   (test-print "#<METHOD Little Grandsire Triples" "Little Grandsire Triples"
    ;;               :name "Little Grandsire" :little t :class :bob :stage 7
    ;;               :place-notation "3,1.7.1.5.1")))
    (labels ((test-print-readably (&rest args)
               (let* ((m (apply #'method args))
                      (*print-readably* t)
                      (*read-eval* t)
                      (r (read-from-string (format nil "~S" m))))
                 (assert-true (equal-methods-p m r))
                 (let ((*read-eval* nil))
                   (assert-error 'print-not-readable (format nil "~S" m))))))
      (test-print-readably
       :name "Lincolnshire" :class :surprise :stage 12
       :place-notation "x3x4x5x6x7x8x9x0x8x9x70xE,2")
      (test-print-readably
       :name "!ü8" :class :alliance :stage 20 :little t :jump t :differential t
       :place-notation "this is a string, even if not really place notation")
      (let ((*default-stage* 5))
        (test-print-readably))
      (test-print-readably :stage nil))
    ;; The malarky with with a dynamic variable below is so Lisp doesn't get smart and
    ;; warn us about the problem at compile time.
    (assert-error 'error (apply #'method *no-such-keyword*))))

(define-test test-title ()
  (labels ((test-title (s &rest args)
             (let* ((m (apply #'method args))
                    (m2 (method-from-title (method-title m)))
                    (m3 (method :stage nil))
                    (m4 (method :stage 14 :name "Mumble" :class :treble-place
                                :little t :differential t :jump t)))
               (assert-equal s (method-title m))
               (assert-true (equal-methods-p m m2))
               (assert-equal s (setf (method-title m3) s))
               (assert-true (equal-methods-p m m3))
               (assert-equal s (setf (method-title m4) s))
               (assert-true (equal-methods-p m m4))
               (assert-true (equal-methods-p m3 m4)))))
    (test-title "Cambridge Surprise Major"
                :name "Cambridge" :class :surprise :stage 8)
    (test-title "Stedman Cinques" :name "Stedman" :class nil :stage 11)
    (test-title "Snarl Jump Differential Little Treble Place Triples"
                :name "Snarl" :class :treble-place :stage 7
                :little t :differential t :jump t)
    (test-title "!ü8 Jump Little Bob Royal"
                :name "!ü8" :class :bob :stage 10 :little t :differential nil :jump t)
    (test-title "Minimus" :stage 4)
    (test-title "Caters" :stage 9)
    (test-title "Delight Royal" :class :delight :stage 10)
    (test-title "Superlative Minor Surprise Major"
                :name "Superlative Minor" :class :surprise :stage 8)
    ;; The special cases.
    (test-title "Grandsire Maximus" :name "Grandsire" :class :bob :stage 12)
    (test-title "Reverse Grandsire Triples" :name "Reverse Grandsire" :class :bob :stage 7)
    (test-title "Double Grandsire Triples" :name "Double Grandsire" :class :bob :stage 7)
    (test-title "Little Grandsire Caters" :name "Little Grandsire" :class :bob :stage 9 :little t)
    (test-title "Grandsire Minimus" :name "Grandsire" :class :place :stage 4)
    (test-title "Reverse Grandsire Minimus" :name "Reverse Grandsire" :class :place :stage 4)
    (test-title "Union Triples" :name "Union" :class :bob :stage 7)
    (test-title "Reverse Union Triples" :name "Reverse Union" :class :bob :stage 7)
    (test-title "Double Union Triples" :name "Double Union" :class :bob :stage 7)
    (test-title "Little Union Triples" :name "Little Union" :class :bob :stage 7 :little t)
    (test-title "Union Bob Doubles" :name "Union" :class :bob :stage 5))
  (assert-equal "StrangeCase Treble Bob Maximus"
                (method-title (method-from-title"StrangeCase tReBlE bOb mAximUs")))
  (assert-equal "Meson Maximus" (method-title (method :name "Meson" :stage 12
                                                      :class :hybrid :little t)))
  (assert-true (equal-methods-p (method :name "Meson" :stage 12)
                                (method-from-title "Meson Maximus")))
  (assert-false (equal-methods-p (method :name "Meson" :stage 12 :little t :class :hybrid)
                                 (method-from-title "Meson Maximus")))
  (let ((*default-stage* 8))
    (assert-equal "Seething Surprise Major"
                  (method-title (method-from-title "Seething Surprise"))))
  (assert-error 'type-error (method-from-title nil))
  (assert-error 'type-error (method-from-title 0))
  (assert-error 'type-error (method-from-title :cambridge-surprise-major))
  (assert-error 'type-error (method-from-title "Rutland Surprise Major" 0))
  (assert-error 'type-error (method-from-title "Watford Surprise Major" :x3x4))
  (let ((method (method-from-title "Lincolnshire Major")))
    (assert-error 'type-error (setf (method-title method) nil))
    (assert-error 'type-error (setf (method-title method) :plain-bob-minor))))

(define-test test-method-changes ()
  (let ((method (method :name "Little Bob"
                        :stage 6
                        :place-notation "x1x4,2")))
    (assert-equalp '(!214365 !132546 !214365 !132465 !214365 !132546 !214365 !124365)
                   (method-changes method))
    (let ((c1 (method-changes method)) (c2 (method-changes method)))
      (assert-true (equalp c1 c2))
      (assert-false (eq c1 c2))
      (setf (car (last c1)) :foo)
      (assert-false (equalp c1 c2)))
    (clear-method-traits method)
    (assert-equalp '(!214365 !132546 !214365 !132465 !214365 !132546 !214365 !124365)
                   (method-changes method))
    (assert-true (equalp (method-changes method)
                         (progn (clear-method-traits method) (method-changes method))))
    (setf (method-place-notation method) "4.36.25x" (method-name method) nil)
    (assert-equalp '(!132465 !213546 !124356 !214365) (method-changes method))
    (setf (method-stage method) 8)
    (assert-equalp '(!13246587 !21354687 !12435768 !21436587) (method-changes method))
    (setf (method-place-notation method) nil)
    (assert-eq nil (method-changes method))
    (setf (method-place-notation method) "x")
    (assert-equalp '(!21436587) (method-changes method))
    (setf (method-place-notation method) nil)
    (assert-eq nil (method-changes method))))

(define-test test-canonicalize-method-place-notation ()
  (let ((method (method-from-title "Avon Delight Maximus"
                                   "-5.-14..5Tx5.30.4x.70.1t.36-9.30.8-18.9t-18-T,1")))
    (assert-equal "x5x4.5x5.30.4x70.1.36x9.30.8x8.9x8x1,T"
                  (canonicalize-method-place-notation method))
    (assert-equal "x5x4.5x5.30.4x70.1.36x9.30.8x8.9x8x1,T"
                  (method-place-notation method))
    (canonicalize-method-place-notation method :cross #\X :upper-case nil :elide :interior)
    (assert-equal "X5X4.5X5.30.4X70.1t.36X9.30.8X8.9X8X1t,1t"
                  (method-place-notation method))))

(define-test test-contains-jump-changes-p ()
  (assert-false (method-contains-jump-changes-p
                 (method :place-notation "x3x4x2x3x4x5,2" :stage 6)))
  (assert-true (method-contains-jump-changes-p
                (method :place-notation "x3x(24)x2x(35)x4x5,2" :stage 6)))
  (assert-false (method-contains-jump-changes-p (method :stage 6)))
  (assert-false (method-contains-jump-changes-p
                 (method :place-notation "x3x(24)x2x(35)x4x5,2" :stage nil)))
  (assert-error 'type-error (method-contains-jump-changes-p nil))
  (assert-error 'type-error (method-contains-jump-changes-p "Cambridge Surprise Minor")))

(define-test test-lead-head ()
  (assert-equalp !15738264 (method-lead-head (method :stage 8
                                              :place-notation "x3x4x5x6x2x3x4x7,2")))
  (assert-equalp !12534 (method-lead-head (method :stage 5 :place-notation "3,1.5.1.5.1")))
  (assert-eq nil (method-lead-head (method :stage 12)))
  (assert-eq nil (method-lead-head (method :place-notation "x3x4x5x6x2x3x4x7,2" :stage nil))))

(define-test test-plain-lead ()
  (assert-equalp '(!123456 !214365 !124635 !216453 !261435 !624153 !621435 !264153 !624513
                   !265431 !256413 !524631 !256431 !524613 !542631 !456213 !546123 !451632
                   !456123 !541632 !514623 !156432 !516342 !153624)
                 (method-plain-lead (method :stage 6 :place-notation "x3x4x2x3x4x5,2")))
  (assert-eq nil (method-plain-lead (method :stage 12)))
  (assert-eq nil (method-plain-lead (method :place-notation "x3x4x5x6x2x3x4x7,2" :stage nil)))
  (let* ((m (method :place-notation "x1x4,2"))
         (p1 (method-plain-lead m))
         (p2 (method-plain-lead m)))
    (assert-true (equal p1 p2))
    (assert-false (eq p1 p2))
    (setf (car (last p1)) :foo)
    (assert-false (equal p1 p2))))

(define-test test-method-lead-length ()
  (assert-eql 24  (method-lead-length (method-from-title "Cambridge Surprise Minor"
                                                         "x3x4x2x3x4x5,2")))
  (assert-eql 1 (method-lead-length (method :stage 4 :place-notation "x")))
  (iter (for stage :from 5 :to +maximum-stage+)
        (if (evenp stage)
            (assert-eql 8 (method-lead-length (method :stage stage :place-notation "x1x4,2")))
            (assert-eql 6 (method-lead-length (method :stage stage
                                                      :place-notation (format nil "3.1.3,~A"
                                                                              (bell-name (1- stage))))))))
  (assert-eq nil (method-lead-length (method-from-title "Cambridge Surprise Minor")))
  (assert-eq nil (method-lead-length (method :stage nil :place-notation "x1x4,2")))
  (assert-error 'type-error (method-lead-length "Cambridge Surprise Minor")))

(define-test test-method-lead-count()
  (assert-eql 5 (method-lead-count (method-from-title "Cambridge Surprise Minor"
                                                      "x3x4x2x3x4x5,2")))
  (assert-eql 1 (method-lead-count (method-from-title "Cromwell Tower Block Minor"
                                                      "3x3.4x2x3x4x3,6")))
  (assert-eql 6 (method-lead-count (method-from-title "Bexx Differential Bob Minor"
                                                      "x1x1x23,2")))
  (assert-eql 3 (method-lead-count (method :stage 10
                                           :place-notation "x34x4x2x23x4x5x6x7x8x9,1")))
  (assert-eql 2 (method-lead-count (method :stage 12 :place-notation "x6x6x6")))
  (assert-eq nil (method-lead-count (method-from-title "Cambridge Surprise Minor")))
  (assert-eq nil (method-lead-count (method :stage nil :place-notation "x1x4,2")))
  (assert-error 'type-error (method-lead-count "Cambridge Surprise Minor")))

(define-test test-method-course-length ()
  (assert-eql 120 (method-course-length (method-from-title "Cambridge Surprise Minor"
                                                           "x3x4x2x3x4x5,2")))
  (assert-eql 24 (method-course-length (method-from-title "Cromwell Tower Block Minor"
                                                          "3x3.4x2x3x4x3,6")))
  (assert-eql 72 (method-course-length (method-from-title "Bexx Differential Bob Minor"
                                                          "x1x1x23,2")))
  (assert-eql 120 (method-course-length (method :stage 10
                                           :place-notation "x34x4x2x23x4x5x6x7x8x9,1")))
  (assert-eql 12 (method-course-length (method :stage 12 :place-notation "x6x6x6")))
  (assert-eq nil (method-course-length (method-from-title "Cambridge Surprise Minor")))
  (assert-eq nil (method-course-length (method :stage nil :place-notation "x1x4,2")))
  (assert-error 'type-error (method-course-length "Cambridge Surprise Minor")))

(define-test test-method-plain-course ()
  (assert-equalp '(!123456 !213465 !214356 !124365
                   !142635 !412653 !416235 !146253
                   !164523 !614532 !615423 !165432
                   !156342 !516324 !513642 !153624
                   !135264 !315246 !312564 !132546)
                 (method-plain-course (method :stage 6 :place-notation "34.2,1")))
  (assert-eq nil (method-plain-course (method-from-title "Cambridge Surprise Minor")))
  (assert-eq nil (method-plain-course (method :stage nil :place-notation "x1x4,2")))
  (assert-error 'type-error (method-plain-course "Cambridge Surprise Minor")))

(define-test test-method-true-plain-course-p ()
  (assert-true (method-true-plain-course-p (method :stage 8
                                                   :place-notation "x3x4x25x36x4x5x6x7,2")))
  (assert-false (method-true-plain-course-p (method :stage 8
                                                    :place-notation "x3x456x56x6x2x5.34.256x1,2")))
  (assert-error 'no-place-notation-error (method-true-plain-course-p (method :stage nil
                                                                             :place-notation "x1")))
  (assert-error 'no-place-notation-error (method-true-plain-course-p (method :stage 8)))
  (assert-error 'no-place-notation-error (method-true-plain-course-p (method :stage nil
                                                                             :place-notation "x1")
                                                                     t))
  (assert-error 'no-place-notation-error (method-true-plain-course-p (method :stage 8) t))
  (assert-false (method-true-plain-course-p (method :stage nil :place-notation "x1") nil))
  (assert-false (method-true-plain-course-p (method :stage 8) nil)))

(define-test test-method-hunt/working-bells ()
  (labels ((testm (stage place-notation hunt-bells working-bells)
             (let ((method (method :stage stage :place-notation place-notation)))
               (assert-equal hunt-bells (method-hunt-bells method))
               (assert-equal working-bells (method-working-bells method))
               (let ((h1 (method-hunt-bells method))
                     (h2 (method-hunt-bells method))
                     (w1 (method-working-bells method))
                     (w2 (method-working-bells method)))
                 (when h1
                   (assert-true (equal h1 h2))
                   (assert-false (eq h1 h2))
                   (setf (car (last h1)) 99)
                   (assert-false (equal h1 h2))
                   (assert-false (equal (last h1) (last h2))))
                 (when w1
                   (assert-true (equal w1 w2))
                   (assert-false (eq w1 w2))
                   (assert-true (equal (first w1) (first w2)))
                   (assert-true (equal (last w1) (last w2)))
                   (assert-false (eq (first w1) (first w2)))
                   (assert-false (eq (last w1) (last w2)))
                   (setf (car (last (last w1))) 99)
                   (assert-false (equal w1 w2))
                   (assert-false (equal (last w1) (last w2)))))
               (setf (method-stage method) nil)
               (assert-true (null (method-hunt-bells method)))
               (assert-true (null (method-working-bells method)))
               (setf (method-stage method) stage)
               (assert-equal hunt-bells (method-hunt-bells method))
               (assert-equal working-bells (method-working-bells method))
               (setf (method-place-notation method) nil)
               (assert-true (null (method-hunt-bells method)))
               (assert-true (null (method-working-bells method))))))
    (testm 5 "3,1.5.1.5.1" '(0 1) '((2 3 4)))
    (testm 12 "x1" nil '((0 2 4 6 8 10 11 9 7 5 3 1)))
    (testm 6 "-1256-1236-12-14-36-12" nil '((0 1 4) (2 5 3)))
    (testm 12 "-34-14-12.5T.30.12-70.5T.16-90.167T-ET-7T-14.36.5T,12"
           '(0 8 9) '((1 4 3 2 5) (6 10) (7 11)))
    (testm 8 "x5x4.5x5.36.4x4.5x4x1,2" '(0 1 2 3 4 5 6 7) nil)))

(defun rotate-place-notation (stage notation n)
  (let ((changes (parse-place-notation notation :stage stage)))
    (when (> n (length changes))
      (throw 'too-long nil))
    (place-notation-string (rotate changes n))))

(define-test test-method-rotations-p ()
  (labels ((equal-key (m1 m2)
             (equalp (method-canonical-rotation-key m1)
                     (method-canonical-rotation-key m2)))
           (test (stage notation &optional other)
             (let ((unrotated (method :stage stage :place-notation notation)))
               (assert-true (method-rotations-p unrotated unrotated))
               (when (<= (+ stage 2) +maximum-stage+)
                 (assert-false (method-rotations-p (method :stage (+ stage 2) :place-notation notation)
                                                   unrotated))
                 (assert-false (equal-key (method :stage (+ stage 2) :place-notation notation)
                                          unrotated)))
               (catch 'too-long
                 (iter (for i :from 0)
                       (for m := (method :stage stage
                                         :place-notation (rotate-place-notation stage notation i)))
                       (for other-m := (and other
                                            (method :stage stage
                                                    :place-notation (rotate-place-notation stage other i))))
                       (assert-true (method-rotations-p m unrotated))
                       (assert-true (method-rotations-p unrotated m))
                       (assert-true (equal-key m unrotated))
                       (when other-m
                         (assert-false (method-rotations-p unrotated other-m))
                         (assert-false (method-rotations-p m other-m))
                         (assert-false (equal-key m other-m))))))))
    (test 8 "34.56.3.6.5x2.36.2x4.3.6x6.7,2" "34.56.3.6.5x2.36.2x4.3.6x6.7,8")
    (test 12 "7x7x67x678,x" "7x7x67x78,x")
    (test 8 "-16-16-18-78-38-38-18-12" "-16-16-18-78-38-38-18-12.1")
    (iter (for i :from 6 :to 24 :by 2) (test i "x1x4,2" "x1x4,1"))
    (iter (for i :from 5 :to 23 :by 2)
          (test i (format nil "3.1.~A.3.1.3,1" (bell-name (- i 1))))))
  (let ((m (method :stage 8 :place-notation "x1x4,8"))
        (m2 (method :stage nil :place-notation "x1x4,8"))
        (m3 (method :stage 8)))
    (assert-error 'type-error (method-rotations-p m nil))
    (assert-error 'type-error (method-rotations-p nil m))
    (assert-error 'type-error (method-rotations-p m 17))
    (assert-error 'type-error (method-rotations-p :method m))
    (assert-error 'type-error (method-canonical-rotation-key nil))
    (assert-error 'type-error (method-canonical-rotation-key 17))
    (assert-error 'no-place-notation-error (method-rotations-p m m2))
    (assert-error 'no-place-notation-error (method-rotations-p m2 m2))
    (assert-error 'no-place-notation-error (method-rotations-p m2 m))
    (assert-error 'no-place-notation-error (method-rotations-p m m3))
    (assert-error 'no-place-notation-error (method-rotations-p m3 m))
    (assert-error 'no-place-notation-error (method-rotations-p m3 m3))
    (assert-eq nil (method-canonical-rotation-key m2))
    (assert-eq nil (method-canonical-rotation-key m3))
    (setf (method-stage m2) 6)
    (assert-error 'parse-error (method-rotations-p m m2))
    (assert-error 'parse-error (method-rotations-p m2 m))
    (assert-error 'parse-error (method-rotations-p m2 m2))
    (assert-error 'parse-error (method-canonical-rotation-key m2))))

#|

(define-test test-classification ()
  (assert-equal '(6 7 8 9) (method-principal-hunt-bells
                            (method :stage 12 :place-notation "7x7x67x678,x")))
  (assert-equal '(4 5 6 7) (method-secondary-hunt-bells
                            (method :stage 8 :place-notation "x4x4,2")))
  (labels ((test-one (stage notation expect
                            &optional (principal nil hunts-supplied) secondary)
             (let ((method (method :stage stage :place-notation notation)))
               (assert-equal expect (method-classification method))
               (when hunts-supplied
                 (assert-equal principal (method-principal-hunt-bells method))
                 (assert-equal secondary (method-secondary-hunt-bells method)))))
           (test-rotated (stage notation expect n)
             (test-one stage (rotate-place-notation stage notation n) expect))
           (test-several (stage notation expect principal secondary)
             (test-one stage notation expect principal secondary)
             (catch 'too-long
               (iter (for i :from 1)
                     (test-rotated stage notation expect i)))))
    (macrolet ((deftest (stage notation principal secondary &rest classification)
                 `(test-several ',stage ',notation ',classification ',principal ',secondary)))
      (deftest 4 "3.1.2.1,4" (0) () :place)
      (deftest 5 "5.1.5.1.5,125" (0) () :bob)
      (deftest 5 "3.1.5.123.5,125" (0) () :bob)
      (deftest 5 "3.145.5.1.5,125" (0) () :place)
      (deftest 5 "5.1.5.3.125,125" (0) (1) :slow-course)
      (deftest 5 "5.1.5.1.125,125" (0) (4) :bob)
      (deftest 5 "3.1.5.1.5.1.5.1.5.1" (0 1) () :bob)
      (deftest 5 "3.1.5.3.1.3,1" () () :principle)
      (deftest 5 "145.3.125.3.1.3.1.3.5.1,123" () () :principle :differential)
      (deftest 5 "3.1.125.3.345,123" (0) () :bob :differential)
      (deftest 5 "5.1.5.145,125" (0) () :bob :differential :little)
      (deftest 5 "3.1.123,125" (0) (1) :slow-course :little)
      (deftest 6 "x1x1x1,2" (0) () :bob)
      (deftest 6 "x4x23.34.25,2" (0) (1) :slow-course)
      (deftest 6 "x34x4x5x23x345x5,2" (0) () :treble-place)
      (deftest 6 "x1x4,2" (0) () :bob :little)
      (deftest 6 "x1x1.5,6" (0) () :place :little)
      (deftest 6 "x3x4x2x3x4x5,2" (0) () :surprise)
      (deftest 6 "x3x4x2x1.34x2.3,2" (0) () :delight)
      (deftest 6 "x34x1x5x1x4x5,2" (0) () :treble-bob)
      (deftest 8 "34.56.3.6.5x2.3.2x34.56,2" (0) () :surprise :little)
      (deftest 6 "x3x4x2x3.4.25.34.5,2" (0) () :treble-place)
      (deftest 6 "2x2.4x2x4,6" (0) () :treble-place :little)
      (deftest 6 "x1x2x1x34x3,6" (0) () :alliance)
      (deftest 6 "x1.4.1x1x2.5,2" (0) () :alliance)
      (deftest 6 "x1.4.1x1.5,2" (0) () :alliance :little)
      (deftest 6 "x1.4.1x1.5,4" (1 2) (0) :alliance)
      (deftest 6 "34.45x3x345,2" (0) () :bob :differential)
      (deftest 6 "x34x4x25x3.4x34.5,6" (0) (5) :surprise :differential)
      (deftest 8 "x3x4x5x6x2x3.56.4.56x56.47.56.3x2x6x5x4x3x2" (0) () :hybrid)
      (deftest 8 "x5x4.5x5x4x5x36x,2" (4 5) () :hybrid :differential :little)
      (deftest 8 "x5x4.5x56.1x4x56,2" (0) () :delight :little)
      (deftest 12 "7x7x67x678,x" (6 7 8 9) () :hybrid :differential :little)
      (deftest 6 "x3x(24)x2x(35)x4x5,2" () () :jump)))
  (labels ((test-distinct (list1 list2)
             (assert-true (and (equal list1 list2)
                               (iter (for sub1 :on list1)
                                     (for sub2 :on list2)
                                     (when sub1
                                       (never (eq sub1 sub2)))))))
           (test-distinct-properties (stage notation classification principal secondary)
             (let* ((method (method :stage stage :place-notation notation))
                    (c1 (method-classification method))
                    (c2 (method-classification method))
                    (p1 (method-principal-hunt-bells method))
                    (p2 (method-principal-hunt-bells method))
                    (s1 (method-secondary-hunt-bells method))
                    (s2 (method-secondary-hunt-bells method)))
               (test-distinct classification c1)
               (test-distinct c1 c2)
               (test-distinct principal p1)
               (test-distinct p1 p2)
               (test-distinct secondary s1)
               (test-distinct s1 s2))))
    (test-distinct-properties 8 "x3x4x2x3x4.5,23"
                              '(:alliance :differential :little) '(0) nil)
    (test-distinct-properties 8 "x1x1x1x1,6"
                              '(:bob) '(0 1 2 3 4) '())
    (test-distinct-properties 8 "x4x4,2"
                              '(:bob :little) '(0) '(4 5 6 7)))
  (assert-eq nil (method-classification (method :stage 10 :name "Mumble")))
  (assert-eq nil (method-classification (method :stage nil :place-notation "x1x4,2")))
  (assert-eq nil (method-classification (method ::name "Mumble")))
  (assert-eq nil (method-classification (method)))
  (assert-error 'type-error (method-classification nil))
  (assert-error 'type-error (method-classification :method))
  (assert-error 'type-error (method-principal-hunt-bells nil))
  (assert-error 'type-error (method-principal-hunt-bells :method))
  (assert-error 'type-error (method-secondary-hunt-bells nil))
  (assert-error 'type-error (method-secondary-hunt-bells :method)))

(define-test test-set-method-classified-name ()
  (macrolet ((test (stage notation name result)
               `(let ((m (method :stage ,stage :place-notation ,notation)))
                  (set-method-classified-name m ,name)
                  (assert-equal ,result (method-name m)))))
    (test 4 "3.1.2.1,4" "Double Court" "Double Court Place")
    (test 5 "5.1.5.1.5,125" "Plain" "Plain Bob")
    (test 5 "3.1.5.123.5,125" "Huntspill" "Huntspill Bob")
    (test 5 "3.145.5.1.5,125" "Shipway" "Shipway Place")
    (test 5 "5.1.5.3.125,125" "Rugby" "Rugby Slow Course")
    (test 5 "5.1.5.1.125,125" "Boxford" "Boxford Bob")
    (test 5 "3.1.5.1.5.1.5.1.5.1" "Grandsire" "Grandsire")
    (test 5 "3.1.5.3.1.3,1" "Stedman" "Stedman")
    (test 5 "145.3.125.3.1.3.1.3.5.1,123" "Borleigh" "Borleigh Differential")
    (test 5 "3.1.125.3.345,123" "Andover" "Andover Differential Bob")
    (test 5 "5.1.5.145,125" "Baldrick" "Baldrick Differential Little Bob")
    (test 5 "3.1.123,125" "Kempton" "Kempton Little Slow Course")
    (test 6 "x1x1x1,2" "Plain" "Plain Bob")
    (test 6 "x4x23.34.25,2" "Baker Street" "Baker Street Slow Course")
    (test 6 "x34x4x5x23x345x5,2" "Apollo" "Apollo Treble Place")
    (test 6 "x1x4,2" "" "Little Bob")
    (test 6 "x1x1.5,6" "Little Little Little Little Little Little Penultimus"
          "Little Little Little Little Little Little Penultimus Little Place")
    (test 6 "x3x4x2x3x4x5,2" "Cambridge" "Cambridge Surprise")
    (test 6 "x3x4x2x1.34x2.3,2" "All Fools" "All Fools Delight")
    (test 6 "x34x1x5x1x4x5,2" "Albion" "Albion Treble Bob")
    (test 8 "34.56.3.6.5x2.3.2x34.56,2" "Shepperton" "Shepperton Little Surprise")
    (test 6 "x3x4x2x3.4.25.34.5,2" "Arthingworth" "Arthingworth Treble Place")
    (test 6 "2x2.4x2x4,6" "Unnamed" "Unnamed Little Treble Place")
    (test 6 "x1x2x1x34x3,6" "Algarkirk" "Algarkirk Alliance")
    (test 6 "x1.4.1x1x2.5,2" "Unnamed" "Unnamed Alliance")
    (test 6 "x1.4.1x1.5,2" "Unnamed" "Unnamed Little Alliance")
    (test 6 "x1.4.1x1.5,4" "Unnamed" "Unnamed Alliance")
    (test 6 "34.45x3x345,2" "Birmingham St Martin" "Birmingham St Martin Differential Bob")
    (test 6 "x34x4x25x3.4x34.5,6" "Huntly Castle" "Huntly Castle Differential Surprise")
    (test 8 "x3x4x5x6x2x3.56.4.56x56.47.56.3x2x6x5x4x3x2" "Crick" "Crick Hybrid")
    (test 8 "x5x4.5x5x4x5x36x,2" "Seven Stars" "Seven Stars Differential Little Hybrid")
    (test 8 "x5x4.5x56.1x4x56,2" "Great Orme" "Great Orme Little Delight")
    (test 12 "7x7x67x678,x" "Charm" "Charm Differential Little Hybrid")
    (test 6 "x3x(24)x2x(35)x4x5,2" "Cambridge" "Cambridge Jump"))
  (let ((m (method :stage 6 :name "Cambridge Surprise" :place-notation "x3x4x2x3x4x5,2"))
        result)
    (assert-no-error 'error (setf result (set-method-classified-name m "Cambridge")))
    (assert-equal "Cambridge Surprise" (method-name m))
    (assert-eq m result)
    (assert-no-error 'error (set-method-classified-name m "Cambridge" nil))
    (assert-equal "Cambridge Surprise" (method-name m))
    (assert-eq m result)
    (assert-no-error 'error (set-method-classified-name m "Cambridge" t))
    (assert-equal "Cambridge Surprise" (method-name m))
    (assert-eq m result)
    (assert-error 'error (setf result (set-method-classified-name m "Grantabridge")))
    (assert-equal "Cambridge Surprise" (method-name m))
    (assert-no-error 'error (set-method-classified-name m "Grantabridge" nil))
    (assert-equal "Grantabridge Surprise" (method-name m))
    (assert-eq m result)
    (assert-error 'error (setf result (set-method-classified-name m "Cambridge" t)))
    (assert-equal "Grantabridge Surprise" (method-name m)))
  (let ((m (method :stage nil)))
    (assert-eq m (set-method-classified-name m "Foo"))
    (assert-eq nil (method-name m))
    (assert-eq nil (method-stage m))
    (setf (method-stage m) 10)
    (assert-eq m (set-method-classified-name m "Foo"))
    (assert-eq nil (method-name m))
    (assert-eql 10 (method-stage m))
    (setf (method-stage m) nil)
    (setf (method-place-notation m) "x1x4,2")
    (assert-eq m (set-method-classified-name m "Foo"))
    (assert-eq nil (method-name m))
    (assert-eql nil (method-stage m))
    (assert-error 'type-error (set-method-classified-name m 17))
    (assert-error 'type-error (set-method-classified-name m 'foo))
    (setf (method-stage m) 7)
    (assert-error 'parse-error (set-method-classified-name m "Foo")))
  (assert-error 'type-error (set-method-classified-name nil "Foo"))
  (assert-error 'type-error (set-method-classified-name 'method "Foo")))

(define-test test-cccbr-name ()
  (assert-equal "Slink" (cccbr-name "Slink Differential Little Place"))
  (assert-equal "Slink Differential Little Place Maximus"
                (cccbr-name "Slink Differential Little Place Maximus"))
  (assert-equal "" (cccbr-name "Little Bob"))
  (assert-equal "Cambridge" (cccbr-name "Cambridge Surprise"))
  (assert-equal "Cambridge" (cccbr-name "Cambridge surprise"))
  (assert-equal "cambridge" (cccbr-name "cambridge Surprise"))
  (assert-equal "Cambridge" (cccbr-name (method :name "Cambridge Surprise" :stage 8)))
  (assert-equal "Cambridge" (cccbr-name (method :name "Cambridge Surprise" :stage nil)))
  (iter (for class-name :in '("Bob" "Place" "Treble Bob" "Surprise" "Delight" "Treble Place"
                         "Alliance" "Hybrid"))
        (assert-equal "Spitalfields Festival"
                      (cccbr-name (format nil "Spitalfields Festival ~A" class-name))))
  (assert-equal "Cambridge" (cccbr-name "Cambridge       Surprise"))
  (assert-equal "Cambridge" (cccbr-name "Cambridge  Differential     Little    Surprise"))
  (assert-error 'type-error (cccbr-name nil))
  (assert-error 'type-error (cccbr-name 17))
  (assert-error 'type-error (cccbr-name '("Cambridge Surprise")))
  (assert-error 'type-error (cccbr-name '|Cambridge Surprise|)))

|#

(define-test test-method-lead-head-code ()
  (iter (for (title code) :on '("Plain Bob Minimus" :a
                                "Plain Bob Doubles" :p
                                "Plain Bob Minor" :a
                                "Plain Bob Cinques" :p
                                "Plain Bob Sixteen" :a
                                "York Surprise Minor" :a
                                "Cambridge Surprise Sixteen" :b
                                "Cassiobury Surprise Major" :c
                                "Sgurr Surprise Royal" :d
                                "Little Bob Twenty-two" :e
                                "Barford Surprise Maximus" :f
                                "Bristol Surprise Royal" :g
                                "Advent Surprise Major" :h
                                "Deva Surprise Major" :j
                                "Double Norwich Court Bob Major" :k
                                "Cornwall Surprise Major" :l
                                "Bristol Surprise Major" :m
                                "Anglia Surprise Royal" :c1
                                "Ripon Surprise Maximus" :c2
                                "Horsleydown Surprise Fourteen" :c3
                                "Gainsborough Little Bob Fourteen" :d1
                                "Anglia Alliance Maximus" :d2
                                "Cambridgeshire Court Bob Cinques" :j1
                                "Bristol Surprise Fourteen" :j2
                                "Bristol Surprise Sixteen" :j4
                                "Ariel Surprise Maximus" :k1
                                "Claret Surprise Maximus" :k2
                                "Leonis Surprise Fourteen" :k3
                                "Strathclyde Surprise Sixteen" :k4
                                "Baldrick Little Bob Doubles" :p1
                                "Baldrick Little Bob Triples" :q1
                                "Baldrick Little Bob Caters" :q1
                                "Baldrick Little Bob Cinques" :q1
                                "Miserden Little Bob Cinques" :p2
                                "Ashford Little Bob Doubles" :q
                                "Ashford Little Bob Triples" :q
                                "Ashford Little Bob Caters" :q
                                "Twerton Little Bob Caters" :q2
                                "Corley Bob Doubles" :r
                                "Little Penultimus Little Place Cinques" :r
                                "Alpha Bob Caters" :r1
                                "Byfield Bob Doubles" :s
                                "Plymouth Bob Triples" :s1
                                "Double Glasgow Surprise Major" nil
                                "Itchingfield Slow Bob Doubles" nil
                                "Longford Bob Doubles" nil
                                "Lynx Differential Maximus" nil
                                )
             :by #'cddr)
        (assert-eq code (method-lead-head-code (lookup-method-by-title title))))
  (assert-error 'type-error (method-lead-head-code nil))
  (assert-error 'type-error (method-lead-head-code "London Surprise Major"))
  (assert-error 'parse-error (method-lead-head-code (method :stage 5 :place-notation "x1x1x6,2"))))

(define-test test-fch-groups ()
  (assert-eql 40 (hash-table-count *fch-groups-by-name*))
  (assert-eql 840 (hash-table-count *fch-groups-by-course-head*))
  (iter (for (r g) :in-hashtable *fch-groups-by-course-head*)
        (assert-true (typep r 'row))
        (assert-true (typep g 'fch-group))
        (case (fch-group-parity g)
          ((nil) (assert-eql 8 (stage r)))
          ((:in-course) (assert-eql 10 (stage r)) (assert-true (in-course-p r)))
          ((:out-of-course) (assert-eql 10 (stage r)) (assert-false (in-course-p r)))
          (otherwise (assert-false 'bad-parity)))
        (counting (and (null (fch-group-parity g)) (in-course-p r)) :into major-in-course)
        (counting (and (null (fch-group-parity g)) (not (in-course-p r)))
                  :into major-out-of-course)
        (counting (eq (fch-group-parity g) :in-course) :into higher-in-course)
        (counting (eq (fch-group-parity g) :out-of-course) :into higher-out-of-course)
        (finally (assert-eql 360 major-in-course)
                 (assert-eql 360 major-out-of-course)
                 (assert-eql 60 higher-in-course)
                 (assert-eql 60 higher-out-of-course)))
  (multiple-value-bind (capital lower-case)
      (iter (for i :from (char-code #\A) :to (char-code #\Z))
            (when-let ((g (fch-group (code-char i))))
              (collect g :into cap))
            (when-let ((g (fch-group (string-downcase (code-char i)))))
              (collect g :into lower))
            (finally (return (values cap lower))))
    (assert-eql 22 (length capital))
    (assert-eql 6 (length lower-case))
    (let ((all (hash-set)))
      (iter (for g :in capital)
            (for s := (apply #'hash-set (fch-group-elements g)))
            (assert-eql (length (fch-group-elements g)) (hash-set-count s))
            (assert-true (hash-set-empty-p (hash-set-intersection s all)))
            (setf all (hash-set-nunion s all)))
      (iter (for g :in lower-case)
            (for s := (apply #'hash-set (fch-group-elements g)))
            (assert-eql (length (fch-group-elements g)) (hash-set-count s))
            (assert-true (every #'(lambda (x) (or (not (tenors-fixed-p x)) (not (in-course-p x))))
                                (fch-group-elements g)))
            (assert-true (hash-set-empty-p (hash-set-intersection s all)))
            (setf all (hash-set-nunion s all)))
      (assert-eql 720 (hash-set-count all))
      (assert-true (every #'(lambda (x) (eql (bell-at-position x 0) 0)) (hash-set-elements all)))
      (assert-true (every #'(lambda (x) (tenors-fixed-p x 7)) (hash-set-elements all))))
    (iter (for g :in (append capital lower-case))
          (assert-true (every #'(lambda (x) (eq (fch-group x) g))
                              (fch-group-elements g)))
          (assert-true (notany #'(lambda (x) (eq (fch-group (alter-stage x 10)) g))
                              (fch-group-elements g)))))
  (multiple-value-bind (in-course out-of-course)
      (iter (for i :from (char-code #\A) :to (char-code #\Z))
            (when-let ((g (fch-group (code-char i) t nil)))
              (collect g :into in-course))
            (when-let ((g (fch-group (format nil "~C1" (code-char i)) t nil)))
              (collect g :into in-course))
            (when-let ((g (fch-group (format nil "~C2" (code-char i)) t nil)))
              (collect g :into in-course))
            (when-let ((g (fch-group (string-downcase (code-char i)) t nil)))
              (collect g :into in-course))
            (when-let ((g (fch-group (format nil "~(~C~)1" (code-char i)) t nil)))
              (collect g :into in-course))
            (when-let ((g (fch-group (format nil "~(~C~)2" (code-char i)) t nil)))
              (collect g :into in-course))
            (when-let ((g (fch-group (code-char i) t t)))
              (collect g :into out-of-course))
            (when-let ((g (fch-group (format nil "~C1" (code-char i)) t t)))
              (collect g :into out-of-course))
            (when-let ((g (fch-group (format nil "~C2" (code-char i)) t t)))
              (collect g :into out-of-course))
            (when-let ((g (fch-group (string-downcase (code-char i)) t t)))
              (collect g :into out-of-course))
            (when-let ((g (fch-group (format nil "~(~C~)1" (code-char i)) t t)))
              (collect g :into out-of-course))
            (when-let ((g (fch-group (format nil "~(~C~)2" (code-char i)) t t)))
              (collect g :into out-of-course))
            (finally (return (values in-course out-of-course))))
    (assert-eql 22 (length in-course))
    (assert-eql 19 (length out-of-course))
    (let ((all (hash-set)))
      (iter (for g :in in-course)
            (for s := (apply #'hash-set (fch-group-elements g)))
            (assert-eql (length (fch-group-elements g)) (hash-set-count s))
            (assert-true (every #'in-course-p (fch-group-elements g)))
            (assert-true (hash-set-empty-p (hash-set-intersection s all)))
            (setf all (hash-set-nunion s all)))
      (iter (for g :in out-of-course)
            (for s := (apply #'hash-set (fch-group-elements g)))
            (assert-eql (length (fch-group-elements g)) (hash-set-count s))
            (assert-true (every #'(lambda (x) (not (in-course-p x))) (fch-group-elements g)))
            (assert-true (hash-set-empty-p (hash-set-intersection s all)))
            (setf all (hash-set-nunion s all)))
      (assert-eql 120 (hash-set-count all))
      (assert-true (every #'(lambda (x) (eql (bell-at-position x 0) 0)) (hash-set-elements all)))
      (assert-true (every #'(lambda (x) (tenors-fixed-p x)) (hash-set-elements all))))
    (iter (for g :in (append in-course out-of-course))
          (assert-true (every #'(lambda (x) (eq (fch-group x) g))
                              (fch-group-elements g)))
          (assert-true (every #'(lambda (x) (eq (fch-group (alter-stage x 12)) g))
                              (fch-group-elements g)))
          (assert-true (every #'(lambda (x) (eq (fch-group (alter-stage x 24)) g))
                              (fch-group-elements g)))
          (assert-true (notany #'(lambda (x) (eq (fch-group (alter-stage x 8)) g))
                               (fch-group-elements g)))))
  (assert-true (member !2436578 (fch-group-elements (fch-group "B")) :test #'equalp))
  (assert-true (member !2436578 (fch-group-elements (fch-group "B" nil)) :test #'equalp))
  (assert-true (member !2436578 (fch-group-elements (fch-group "B" nil nil)) :test #'equalp))
  (assert-false (member !3246578 (fch-group-elements (fch-group "B")) :test #'equalp))
  (assert-true (member !243657890 (fch-group-elements (fch-group "B" t)) :test #'equalp))
  (assert-true (member !243657890 (fch-group-elements (fch-group "B" t nil)) :test #'equalp))
  (assert-false (member !243657890 (fch-group-elements (fch-group "B" t t)) :test #'equalp))
  (assert-false (member !324657890 (fch-group-elements (fch-group "B" t)) :test #'equalp))
  (assert-false (member !324657890 (fch-group-elements (fch-group "B" t nil)) :test #'equalp))
  (assert-false (member !324657890 (fch-group-elements (fch-group "B" t t)) :test #'equalp))
  (assert-false (member !324567890 (fch-group-elements (fch-group "B" t)) :test #'equalp))
  (assert-false (member !324567890 (fch-group-elements (fch-group "B" t nil)) :test #'equalp))
  (assert-true (member !324567890 (fch-group-elements (fch-group "B" t t)) :test #'equalp))
  (assert-true (member !3245678 (fch-group-elements (fch-group "B")) :test #'equalp))
  (assert-true (member !3245678 (fch-group-elements (fch-group "B" nil)) :test #'equalp))
  (assert-true (member !3245678 (fch-group-elements (fch-group "B" nil nil)) :test #'equalp))
  (assert-false (member !324567890 (fch-group-elements (fch-group "B" t)) :test #'equalp))
  (assert-false (member !324567890 (fch-group-elements (fch-group "B" t nil)) :test #'equalp))
  (assert-true (member !324567890 (fch-group-elements (fch-group "B" t t)) :test #'equalp))
  (assert-true (member (rounds 8) (fch-group-elements (fch-group "A")) :test #'equalp))
  (assert-true (member !13254768 (fch-group-elements (fch-group "A")) :test #'equalp))
  (assert-true (member (rounds 10) (fch-group-elements (fch-group "A" t)) :test #'equalp))
  (assert-eq nil (fch-group "A" t t))
  (assert-equal "B" (fch-group-name (fch-group !2436578)))
  (assert-eq nil (fch-group-parity (fch-group !2436578)))
  (assert-equal "E" (fch-group-name (fch-group !3246578)))
  (assert-equal "f" (fch-group-name (fch-group !5462378)))
  (assert-equal "K" (fch-group-name (fch-group !5234678)))
  (assert-equal "K1" (fch-group-name (fch-group !523467890)))
  (assert-eq :out-of-course (fch-group-parity (fch-group !523467890)))
  (assert-equal "U" (fch-group-name (fch-group !3526478)))
  (assert-equal "U2" (fch-group-name (fch-group !352647890)))
  (assert-eq :in-course (fch-group-parity (fch-group !352647890)))
  (assert-equal "a" (fch-group-name (fch-group !2354678)))
  (assert-equal "a2" (fch-group-name (fch-group !235467890ET)))
  (assert-eq nil (fch-group-parity (fch-group !2436578)))
  (assert-eq nil (fch-group-parity (fch-group !3246578)))
  (assert-eq nil (fch-group !1325476890))
  (assert-eq nil (fch-group !21435678))
  (assert-eq nil (fch-group !2143567890))
  (assert-eq nil (fch-group !12435687))
  (assert-eq nil (fch-group "W"))
  (assert-eq nil (fch-group "W" t))
  (assert-eq nil (fch-group "W" t t))
  (assert-eq nil (fch-group "w"))
  (assert-eq nil (fch-group "w" t))
  (assert-eq nil (fch-group "w" t t))
  (assert-eq nil (fch-group !1243657))
  (assert-error 'type-error (fch-group 12436578))
  (locally
      #+SBCL (declare (sb-ext:muffle-conditions warning))
    ;; SBCL is too clever and moans about these at compile time, hence the declaration
    (assert-error 'type-error (fch-group-name nil))
    (assert-error 'type-error (fch-group-parity nil))
    (assert-error 'type-error (fch-group-elements nil))
    (assert-error 'type-error (fch-group-name :foo))
    (assert-error 'type-error (fch-group-parity :foo))
    (assert-error 'type-error (fch-group-elements :foo))
    (assert-error 'type-error (fch-group-name 7))
    (assert-error 'type-error (fch-group-parity 8))
    (assert-error 'type-error (fch-group-elements 9))
    (assert-error 'type-error (fch-group-name !12436578))
    (assert-error 'type-error (fch-group-parity !1243657890))
    (assert-error 'type-error (fch-group-elements !1243657890ET)))
  (assert-error 'error (fch-group !12436578 t))
  (assert-error 'error (fch-group !12436578 t nil))
  (assert-error 'error (fch-group !12436578 t t))
  (assert-error 'error (fch-group !12436578 nil))
  (with-sink-stream
    (assert-prints "B" (princ (fch-group !12436578)))
    (assert-prints "B" (princ (fch-group !1243657890)))
    (assert-prints "a1" (princ (fch-group !1234657890)))
    (assert-prints "L2" (princ (fch-group "L2" t)))
    (assert-prints "a2" (princ (fch-group "a2" t t)))
    (let ((*print-readably* t))
      (assert-prints "#.(FCH-GROUP \"B\")" (prin1 (fch-group !12436578)))
      (assert-prints "#.(FCH-GROUP \"B\" T NIL)" (prin1 (fch-group !1243657890)))
      (assert-prints "#.(FCH-GROUP \"a1\" T T)" (prin1 (fch-group !1234657890)))
      (assert-prints "#.(FCH-GROUP \"L2\" T NIL)" (prin1 (fch-group "L2" t)))
      (assert-prints "#.(FCH-GROUP \"a2\" T T)" (prin1 (fch-group "a2" t t)))))
  (labels ((group-list (name &rest args)
             (when-let ((g (apply #'fch-group name args)))
               (list g)))
           (test-strings (major higher-1 higher-2 higher-3 &rest strings)
             (iter (for list :in strings)
                   (for i :from 0)
                   (collect (mapcan #'group-list list) :into g8)
                   (collect (mapcan #'(lambda (x) (group-list x t (oddp i))) list) :into g10+)
                   (collect (mapcan #'(lambda (x) (group-list x t (evenp i))) list) :into g10-)
                   (finally
                    (assert-equal major (apply #'fch-groups-string g8))
                    (assert-equal higher-1 (apply #'fch-groups-string g10+))
                    (assert-equal higher-2 (apply #'fch-groups-string g10-))
                    (assert-equal higher-3 (apply #'fch-groups-string (append g10- g10+)))
                    (when (and (flatten g8) (flatten g10+))
                      (assert-error 'mixed-stage-fch-groups-error
                                    (apply #'fch-groups-string (append g8 g10+))))
                    (when (and (flatten g8) (flatten g10-))
                      (assert-error 'mixed-stage-fch-groups-error
                                    (apply #'fch-groups-string (append g10- g8))))))))
    (test-strings "BDc" "BD/" "/BDc" "BD/BDc"
                  '("B" "D" "c"))
    (test-strings "BDc" "BD/BDc" "BD/BDc" "BD/BDc"
                  '("B" "D" "c") '("D" "B" "c") '("D" "c" "B") '("B" "c" "D") '("B" "D" "c"))
    (test-strings nil nil nil nil '())
    (test-strings "ABCDEFGHIKMNX" "ABCDEFGHIKL1MN/" "/BCDEFHa2" "ABCDEFGHIKL1MN/BCDEFHa2"
                  '("a2" "N" "M" "X" "L1" "K" "I" "H" "G" "F" "E" "D" "C" "B") '() '("X" "A" "G")))
  (assert-equal "AKUdX" (fch-groups-string (vector (fch-group "d") (fch-group "K"))
                                           (list (fch-group "X") (fch-group "A"))
                                           (hash-set (fch-group "K") (fch-group "U")))))

#|

;;; Needs to have method-lead-head-code defined before it will work

(define-test test-method-falseness ()
  (labels ((test-fch-summary (title result)
             (multiple-value-bind (ignore-1 groups ignore-2)
                 (method-falseness (lookup-method-by-title title))
               (declare (ignore ignore-1 ignore-2))
               (assert-equal result (fch-groups-string groups)))))
    (test-fch-summary "Cambridge Surprise Major" "BDEe")
    (test-fch-summary "Bristol Surprise Major" "c")
    (test-fch-summary "Budapest Surprise Major" "BcdY")
    (test-fch-summary "Colston Arms Surprise Major" "DKLOTa")
    (test-fch-summary "Double Darrowby Surprise Major" "BCDEFKLMPTUabcd")
    (test-fch-summary "Othorpe Surprise Major" "BDXY")
    (test-fch-summary "Heywood Alliance Major" "a")
    (test-fch-summary "Double Glasgow Surprise Major" nil)
    (test-fch-summary "Little Bob Major" nil)
    (test-fch-summary "Cumberland Bob Major" nil)
    (test-fch-summary "Normandy Surprise Major" "ABa")
    (test-fch-summary "Crick Hybrid Major" nil)
    (test-fch-summary "Cambridge Surprise Royal" "BD/B")
    (test-fch-summary "Old Surprise Royal" "/BK1a1c")
    (test-fch-summary "Zorin Surprise Royal" "T/BDa1c")
    (test-fch-summary "Cambridge Surprise Maximus" "D/B")
    (test-fch-summary "Swindon Surprise Maximus" "/Bc")
    (test-fch-summary "Cambridge Surprise Fourteen" "D/B")
    (test-fch-summary "Tauron Surprise Fourteen" "/Oc")
    (test-fch-summary "Cambridge Surprise Sixteen" "D/B")
    (test-fch-summary "Phobos Moon Surprise Sixteen" "D/BDa2"))
  (assert-equalp (hash-set !13254678 !13246578 !12436578 !14326578 !14625378 !13245678
                           !12435678 !14325678 !14253678 !13524678 !12543678 !15342678
                           !12536478 !12463578 !12365478 !14265378 !13625478 !16345278
                           !12435768 !12354768 !13452768 !14253768 !13524768 !15234768
                           !15342768 !14236758 !13256748 !12456738 !15263748 !13654728
                           !16352748 !12345768 !13254768 !12364758 !13462758 !14263758
                           !16452738 !13427568 !12647358 !12357468 !13527468 !15237468
                           !12347658 !13547628 !15327648 !14367258 !12567438 !16327458
                           !13274568 !13572468 !15374268 !13275648 !12573648 !15372648
                           !13476528 !15276348 !13672548 !14372658 !16274538 !12734568
                           !16745238 !15734268 !12745638 !13756248 !12763458 !17254368
                           !17543628 !17236548 !17365428 !17243658 !17654328)
                 (apply #'hash-set (method-falseness (lookup-method-by-title "Belfast Surprise Major"))))
  (assert-equalp (hash-set !1325467890ET !1462537890ET !1324567890ET
                           !1432567890ET !1254367890ET !1236547890ET)
                 (apply #'hash-set (method-falseness (lookup-method-by-title  "Cambridge Surprise Maximus"))))
  (iter (with incidence := (third (multiple-value-list
                                   (method-falseness
                                    (lookup-method-by-title "Cambridge Surprise Royal")))))
        (for i :from 0 :below 9)
        (iter (for j :from 0 :below 9)
              (when-let ((x (aref incidence i j)))
                (setf (aref incidence i j) (apply #'hash-set x))))
        (finally
         (assert-equalp (make-array '(9 9) :initial-contents
                                    `((nil nil nil nil ,(hash-set !1432567890 !1254367890)
                                           ,(hash-set !1462537890 !1324567890) nil nil nil)
                                      (nil nil nil ,(hash-set !1243657890) nil ,(hash-set !1325467890 !1254367890) nil nil
                                           nil)
                                      (nil nil nil nil nil nil nil nil nil)
                                      (nil ,(hash-set !1243657890) nil nil nil nil nil ,(hash-set !1462537890 !1236547890)
                                           ,(hash-set !1325467890 !1432567890))
                                      (,(hash-set !1432567890 !1254367890) nil nil nil nil nil nil nil
                                       ,(hash-set !1324567890 !1236547890))
                                      (,(hash-set !1462537890 !1324567890) ,(hash-set !1325467890 !1254367890) nil nil nil
                                       nil nil ,(hash-set !1243657890) nil)
                                      (nil nil nil nil nil nil nil nil nil)
                                      (nil nil nil ,(hash-set !1462537890 !1236547890) nil ,(hash-set !1243657890) nil nil
                                           nil)
                                      (nil nil nil ,(hash-set !1325467890 !1432567890) ,(hash-set !1324567890 !1236547890)
                                           nil nil nil nil)))
                        incidence)))
  (assert-error 'type-error (method-falseness nil))
  (assert-error 'type-error (method-falseness "Cambridge Surprise Major"))
  (assert-error 'parse-error (method-falseness (method :stage 8 :place-notation "9.2,0")))
  (assert-error 'no-place-notation-error (method-falseness (method)))
  (assert-error 'no-place-notation-error (method-falseness (method :stage 12)))
  (assert-error 'no-place-notation-error (method-falseness (method :stage nil :place-notation "x1x4,2")))
  (assert-error 'inappropriate-method-error (method-falseness (lookup-method "Stedman Triples")))
  (assert-error 'inappropriate-method-error (method-falseness (lookup-method "Stedman Cinques")))
  (assert-error 'inappropriate-method-error (method-falseness (lookup-method "Grandsire Caters")))
  (assert-error 'inappropriate-method-error (method-falseness (lookup-method "Grandsire Major")))
  (assert-error 'inappropriate-method-error (method-falseness (lookup-method "Grandsire Royal")))
  (assert-error 'inappropriate-method-error (method-falseness (lookup-method "Temple Meads Differential Surprise Royal"))))

|#

(define-test test-lookup-methods ()
  (labels ((just-one (list)
             (assert-eql 1 (length list))
             (first list))
           (test-m (title-or-wild name stage notation &optional class little differential jump)
             (iter (with  wild := (find #\* title-or-wild))
                   (with nm := (if wild title-or-wild name))
                   (for m :in (list (just-one (lookup-methods :name nm :jump jump
                                                              :differential differential
                                                              :little little :class class
                                                              :stage stage))
                                    (unless wild
                                      (lookup-method-by-title title-or-wild))
                                    (just-one (lookup-methods-by-notation notation stage))
                                    (just-one (lookup-methods-by-notation (parse-place-notation notation :stage stage)))))
                   (unless m (next-iteration))
                   (unless wild
                     (assert-equal title-or-wild (method-title m)))
                   (assert-equal notation (method-place-notation m))
                   (assert-equal name (method-name m))
                   (assert-eql stage (method-stage m))
                   (assert-eq class (method-class m))
                   (assert-eq little (method-little-p m))
                   (assert-eq differential (method-differential-p m))
                   (assert-eq jump (method-jump-p m)))))
    (test-m "Advent Surprise Major" "Advent" 8 "36x56.4.5x5.6x4x5x4x7,8" :surprise)
    (test-m "Syon Gipsy Royal" "Syon Gipsy" 10 "x,4x3x1x5x1x34x1x34" :hybrid)
    (test-m "Stedman Cinques" "Stedman" 11 "3.1.E.3.1.3,1")
    (test-m "Little Bob Minor" nil 6 "x1x4,2" :bob t)
    (test-m "Plain Bob Major" "Plain" 8 "x1x1x1x1,2" :bob)
    (test-m "Plain Bob Triples" "Plain" 7 "7.1.7.1.7.1.7,127" :bob)
    (test-m "Grandsire Triples" "Grandsire" 7 "3,1.7.1.7.1.7.1" :bob)
    (test-m "Union Triples" "Union" 7 "3.1.7.1.7.1.7.1.7.1.7.1.5.1" :bob)
    (test-m "Lynx Differential Maximus" "Lynx" 12 "x49x49.3670x,T" nil nil t)
    (test-m "London Link Differential Sixteen" "London Link" 16 "5T.1.5T.1.70.3Bx,D" :hybrid t t)
    (test-m "Bexxx Differential Bob Minor" "Bexxx" 6 "x1x1x23,2" :bob nil t)
    (test-m "Amaranth Little Place Major" "Amaranth" 8 "34.1.34,8" :place t)
    (test-m "Sail & Anchor Treble Bob Minor" "Sail & Anchor" 6 "x5x1x2x1x2.234.345.2.234.34.1x2x1x5x2" :treble-bob)
    (test-m "www.ouscr.org.uk Treble Place Minor" "www.ouscr.org.uk" 6 "x1x1x1x1x1x4,2" :treble-place)
    (test-m "Adv*nt"  "Advent" 8 "36x56.4.5x5.6x4x5x4x7,8" :surprise)
    (test-m "*advent"  "Advent" 8 "36x56.4.5x5.6x4x5x4x7,8" :surprise)
    (test-m "*a*d*v*e*n*t"  "Advent" 8 "36x56.4.5x5.6x4x5x4x7,8" :surprise)
    (test-m "ADVE****T"  "Advent" 8 "36x56.4.5x5.6x4x5x4x7,8" :surprise)
    (test-m "*aDvEnT"  "Advent" 8 "36x56.4.5x5.6x4x5x4x7,8" :surprise)
    (test-m "adam and*" "Adam and Jack's Northern Adventure" 8 "x56x4x56x3x34x5.34x34.5,8" :surprise)
    (test-m "ad* AND*" "Adam and Jack's Northern Adventure" 8 "x56x4x56x3x34x5.34x34.5,8" :surprise)
    (test-m "Whitehall Surprise Major" "Whitehall" 8 "34x5.6x56x36x34x3.4x56.1,8" :surprise)
    (test-m "White Hall Surprise Major" "White Hall" 8 "34x5.4x256x23x4x5.34x236.7,2" :surprise)
    (test-m "El Pueblo de Nuestra Señora la Reina de los Ángeles de Porciúncula Alliance Major"
            "El Pueblo de Nuestra Señora la Reina de los Ángeles de Porciúncula"
            8 "x3x4x5x6x2x3x47x56x7,2" :alliance)
    (test-m "*ñora la reina DE los*Porciúncula"
            "El Pueblo de Nuestra Señora la Reina de los Ángeles de Porciúncula"
            8 "x3x4x5x6x2x3x47x56x7,2" :alliance)
    (test-m "*ora la reina DE los*Porc*cula"
            "El Pueblo de Nuestra Señora la Reina de los Ángeles de Porciúncula"
            8 "x3x4x5x6x2x3x47x56x7,2" :alliance)
    (test-m "Panserbjørn Surprise Major" "Panserbjørn" 8 "x34x4x56x6x2x5.34x4.5,2" :surprise)
    (test-m "*pAN*bjørn*" "Panserbjørn" 8 "x34x4x56x6x2x5.34x4.5,2" :surprise)
    (test-m "Panserb*n" "Panserbjørn" 8 "x34x4x56x6x2x5.34x4.5,2" :surprise)
    (test-m "Nu.Q™ Alliance Maximus" "Nu.Q™" 12 "x3x4x5x6x7x8x9x0xEx1x9,2" :alliance)
    (test-m "*Q™" "Nu.Q™" 12 "x3x4x5x6x7x8x9x0xEx1x9,2" :alliance)
    (test-m "Nu.Q*" "Nu.Q™" 12 "x3x4x5x6x7x8x9x0xEx1x9,2" :alliance)
    (test-m "Janáček Surprise Major" "Janáček" 8 "x36x4x5x6x34x3x56x3,2" :surprise)
    (test-m "*janáček*" "Janáček" 8 "x36x4x5x6x34x3x56x3,2" :surprise)
    (test-m "JAN*K" "Janáček" 8 "x36x4x5x6x34x3x56x3,2" :surprise)
    (test-m "E=mc² Surprise Major" "E=mc²" 8 "5x3.6x5x3.4x4.5x2x7,2" :surprise)
    (test-m "*&mc²" "E=mc²" 8 "5x3.6x5x3.4x4.5x2x7,2" :surprise)
    (test-m "E*mc*" "E=mc²" 8 "5x3.6x5x3.4x4.5x2x7,2" :surprise))
  (labels ((test-n (keys min &rest exemplars)
             (let ((results (apply #'lookup-methods keys)))
               (assert-true (>= (length results) min))
               (when exemplars
                 (iter (with titles := (mapcar #'method-title results))
                       (for e :in exemplars)
                       (assert-true (member e titles :test #'equal)))))))
    (test-n '(:name "Advent") 3
            "Advent Surprise Major"
            "Advent Alliance Major"
            "Advent Delight Major")
    (test-n '(:name "Advent*") 5
            "Advent Surprise Major"
            "Advent Alliance Major"
            "Advent Delight Major"
            "Advent Sunday Surprise Minor"
            "Adventurers' Fen Surprise Maximus")
    (test-n '(:name nil) 9
            "Little Bob Minor"
            "Little Bob Major"
            "Little Bob Royal"
            "Little Bob Maximus"
            "Little Bob Fourteen"
            "Little Bob Sixteen"
            "Little Bob Eighteen"
            "Little Bob Twenty"
            "Little Bob Twenty-Two")
    (test-n nil 21637)
    (test-n '(:stage 16) 49 "Leda Little Alliance Sixteen")
    (test-n '(:class :hybrid) 119 "Seven Stars Major")
    (test-n '(:differential t) 244 "Thames Lynx Differential Maximus")
    (test-n '(:little :yup :class :delight) 22 "Colborne Little Delight Major")
    (test-n '(:jump t) 0)
    (test-n '(:differential t :little t :stage 12 :name "*y*") 5
            "Mercury Differential Little Place Maximus"
            "Poppy Differential Little Treble Place Maximus"
            "Ruby Differential Little Bob Maximus"
            "Slinky Differential Little Treble Place Maximus"
            "Trinity Differential Little Alliance Maximus")
    (test-n '(:stage 9 :class :bob) 145 "Shipway Court Bob Caters")
    (test-n '(:stage 9 :class :place) 3 "Little Little Little Penultimus Little Place Caters"))
  (let ((all (lookup-methods))
        (named (lookup-methods :name "*"))
        (unnamed (lookup-methods :name nil)))
    (assert-eql (length all) (+ (length named) (length unnamed))))
  (assert-eq (lookup-methods :name "") nil)
  (assert-equal '("Mumble Little" nil nil nil nil 12)
                (multiple-value-list (parse-method-title "Mumble Little Maximus")))
  (assert-error 'type-error (lookup-methods :stage :major))
  (assert-error 'type-error (lookup-methods :stage 100))
  (assert-error 'type-error (lookup-methods :stage 0))
  (assert-error 'type-error (lookup-methods :stage nil))
  (assert-error 'type-error (lookup-methods-by-notation "x" :major))
  (assert-error 'type-error (lookup-methods-by-notation "x" 100))
  (assert-error 'type-error (lookup-methods-by-notation "x" 0))
  (assert-error 'type-error (lookup-methods :name :cambridge))
  (assert-error 'type-error (lookup-methods-by-notation :whatever 8))
  (assert-error 'type-error (lookup-methods-by-notation '(1 2 3) 8))
  (assert-error 'parse-error (lookup-methods-by-notation "Not your father's place notation" 8))
  (assert-error 'parse-error (lookup-methods-by-notation "x3x4x5x6x7x8x9,2" 8)))

(define-test test-update-method-library ()
  (let ((*method-library* nil)
        (*method-library-path* (fad:with-output-to-temporary-file (s))))
    (unwind-protect
         (progn
           (fad:copy-file (merge-pathnames (make-pathname :name "test-library" :type "data")
                                           (asdf:system-source-directory :roan))
                          *method-library-path*
                          :overwrite t)
           (let ((old (multiple-value-list (method-library-details))))
             (assert-true (< (length (lookup-methods)) 10))
             (assert-eq (lookup-methods :name "Cambridge") nil)
             (assert-true (> (update-method-library) 21000))
             (assert-true (>= (length (lookup-methods :name "Cambridge")) 13))
             (let ((new (multiple-value-list (method-library-details))))
               (assert-false (equalp new old))
               ;; Race condition if we call this just when the CCCBR stuff gets updated.
               ;; Who cares?
               (assert-false (update-method-library))
               (assert-equal new (multiple-value-list (method-library-details)))
               (assert-true (> (update-method-library t) 21000))
               (assert-false (equalp new (multiple-value-list (method-library-details)))))))
      (when (probe-file *method-library-path*)
        (delete-file *method-library-path*)))))

#|

(defmacro with-temp-filename ((name-var &optional preserve) &body body)
  `(%with-temp-filename #'(lambda (,name-var) ,@body) ,preserve))

(defun %with-temp-filename (thunk preserve)
  (let (temp temp-name)
    (unwind-protect
         (progn
           (setf temp (cl-fad:open-temporary))
           (setf temp-name (pathname temp))
           (close temp)
           (unless preserve
             (delete-file temp-name))
           (setf temp nil)
           (funcall thunk temp-name))
      (when temp
        (close temp))
      (when (probe-file temp-name)
        (delete-file temp-name)))))

(defparameter *artificial-database-contents*
  '((foo6 6 "Foo" "x3x3x3x3x3x3x3x3x3x3x3x3,4" "fribble" "frob")
    (foo8 8 "Foo" "x3x3x3x3x3x3x3x3x3x3x3x3,4" nil nil)
    (star 7 "*" "1" nil "star")
    (foo10 10 "Foo" "x3x3x3x3x3x3x3x3x3x3x3x3,4" "floop" nil)
    (star2 7 "x * x" "1" nil nil)
    (foo10* 10 "Foo*" "x3x3x3x3x3x3x3x3x3x3x3x3,4" nil "ickyicky")
    (question 7"?" "1" nil nil)
    (backslash 7 "\\\\" "1" "stuff" nil)
    (foo12 12 "Foo" "x3x3x3x3x3x3x3x3x3x3x3x3,4" nil "frobozo")
    (backslash2 7 "\\\\\\\\xxx\\\\\\\\" "1" nil "thingie")
    (backslash3 9 "\\\\\\\\xxx\\\\\\\\" "1" nil nil)
    (question2 7 "x  ???  x   ???   x" "1" nil nil)))

(define-test test-using-artificial-database ()
  (with-temp-filename (path t)
    (with-methods-database (connection :database path)
      (sqlite:with-transaction connection
        (sqlite:execute-non-query connection "create table methods (stage int, name text,
                                              notation text, canonicalRotation text,
                                              firstTower text, firstHand text)")
        (sqlite:execute-non-query connection "create table version (updated text non null, etag text)")
        (iter (for (nil stage name notation tower hand) :in *artificial-database-contents*)
              (sqlite:execute-non-query connection
                                        "insert into methods values (?, ?, ?, ?, ?, ?)"
                                        stage
                                        name
                                        notation
                                        (with-initial-format-characters
                                          (place-notation-string (canonical-rotation
                                                                  (parse-place-notation notation :stage stage))
                                                                 :comma t))
                                        tower
                                        hand))
        (sqlite:execute-non-query connection "insert into version values (current_timestamp, null)")))
    (prepare-database path "the-etag")
    (macrolet ((test-method (key method)
                 (once-only (method)
                   `(destructuring-bind (stage name notation tower hand)
                        (cdr (assoc ,key *artificial-database-contents*))
                      (assert-typep 'method ,method)
                      (when (typep ,method 'method)
                        (assert-eql stage (method-stage ,method))
                        (assert-equal name (method-name ,method))
                        (assert-equal notation (method-place-notation ,method))
                        (assert-equal tower (method-property ,method :first-tower))
                        (assert-equal hand (method-property ,method :first hand)))))))
      (let ((m (lookup-methods-by-name "Foo" :stage 6 :database path)))
        (assert-eql 1 (length m))
        (test-method 'foo6 (first m)))
      (let ((m (lookup-methods-by-notation "x3x3x3x3x3x3x3x3x3x3x3x3,4" :stage 6 :database path)))
        (assert-eql 1 (length m))
        (test-method 'foo6 (first m)))
      (let ((m (lookup-methods-by-name "Foo" :stage 8 :database path)))
        (assert-eql 1 (length m))
        (test-method 'foo8 (first m)))
      (let ((m (lookup-methods-by-notation "x3x3x3x3x3x3x3x3x3x3x3x3,4" :stage 8 :database path)))
        (assert-eql 1 (length m))
        (test-method 'foo8 (first m)))
      (let ((m (lookup-methods-by-name "Foo" :stage 10 :database path)))
        (assert-eql 1 (length m))
        (test-method 'foo10 (first m)))
      (let ((m (lookup-methods-by-notation "x3x3x3x3x3x3x3x3x3x3x3x3,4" :stage 10 :database path)))
        (assert-eql 2 (length m))
        (test-method 'foo10 (find "Foo" m :key #'method-name :test #'equal))
        (test-method 'foo10* (find "Foo*" m :key #'method-name :test #'equal)))
      (let ((m (lookup-methods-by-notation "x3x3x3x3x3x3x3x3x3x3x3x3,4" :stage 10 :database path :limit 1)))
        (assert-eql 1 (length m))
        (test-method (if (find "Foo" m :key #'method-name :test #'equal) 'foo10 'foo10*) (first m)))
      (let ((m (lookup-methods-by-name "Foo" :stage 12 :database path)))
        (assert-eql 1 (length m))
        (test-method 'foo12 (first m)))
      (let ((m (lookup-methods-by-notation "x3x3x3x3x3x3x3x3x3x3x3x3,4" :stage 12 :database path)))
        (assert-eql 1 (length m))
        (test-method 'foo12 (first m)))
      (iter (for i :from +minimum-stage+ :to +maximum-stage+)
            (unless (member i '(6 8 10 12))
              (assert-eq nil (lookup-methods-by-name "Foo" :stage i :database path))
              (unless (or (oddp i) (< i 4))
                (assert-eq nil (lookup-methods-by-notation "x3x3x3x3x3x3x3x3x3x3x3x3,4"
                                                           :stage i :database path)))))
      (assert-eql 6 (length (lookup-methods-by-name "*" :stage 7 :database path)))
      (assert-eql 1 (length (lookup-methods-by-name "\\*" :stage 7 :database path)))
      (assert-eql 2 (length (lookup-methods-by-name "*\\**" :stage 7 :database path)))
      (assert-eql 2 (length (lookup-methods-by-name "?" :stage 7 :database path)))
      (assert-eql 1 (length (lookup-methods-by-name "\\?" :stage 7 :database path)))
      (assert-eql 2 (length (lookup-methods-by-name "*\\?*" :stage 7 :database path)))
      (assert-eql 2 (length (lookup-methods-by-name "\\\\*" :stage 7 :database path)))
      (assert-eql 2 (length (lookup-methods-by-name "?\\\\*" :stage 7 :database path)))
      (assert-eql 0 (length (lookup-methods-by-name "\\?\\\\" :stage 7 :database path)))
      (assert-eql 0 (length (lookup-methods-by-name "*\\\\*\\**" :stage 7 :database path)))
      (assert-eql 3 (length (lookup-methods-by-name "**********x***********" :stage 7 :database path)))
      (assert-eql 6 (length (lookup-methods-by-notation "1" :stage 7 :database path)))
      (assert-eql 1 (length (lookup-methods-by-notation "1" :stage 9 :database path)))
      (assert-eql 0 (length (lookup-methods-by-notation "1" :stage 11 :database path)))
      (assert-eql 6 (length (lookup-methods-from-changes '(!1325476) :database path)))
      (assert-eql 1 (length (lookup-methods-from-changes '(!132547698) :database path)))
      (assert-eql 0 (length (lookup-methods-from-changes '(!132547698E0) :database path))))))

#-(or clisp lispworks6 lispworks7)
(defun ringing-org-accessible-p ()
  (values (ignore-errors
            (multiple-value-bind (reply status)
                (drakma:http-request "http://www.ringing.org/"
                                     :method :head
                                     :connection-timeout 3)
              (declare (ignore reply))
              (eql status 200)))))

#-(or clisp lispworks6 lispworks7)
(define-constant +test-database-urls+
    '("http://test.ringing.org/html/test-roan-methods-1.zip"
      "http://test.ringing.org/html/test-roan-methods-2.zip")
  :test #'equal)

#-(or clisp lispworks6 lispworks7)
(define-test test-update-methods-database ()
  (let ((accessible (ringing-org-accessible-p)))
    (assert-true accessible)
    (when accessible
      (with-temp-filename (db1)
        (assert-true (update-methods-database :database db1 :url (first +test-database-urls+)))
        (let ((attributes (multiple-value-list (methods-database-attributes db1))))
          (assert-eql (parse-timestamp "2017-06-18 22:10:23") (second attributes))
          (assert-eql 22 (fourth attributes))
          (assert-equalp #(0 0 0 0 1 1 2 1 4 1 3 1 2 1 2 1 2 0 0 0 0 0 0 0 0)
                         (fifth attributes))
          (assert-false (update-methods-database :database db1 :url (first +test-database-urls+)))
          (assert-true (update-methods-database :database db1 :url (first +test-database-urls+) :force t))
          (assert-equalp attributes (multiple-value-list (methods-database-attributes db1)))
          (with-temp-filename (db2)
            (assert-true (update-methods-database :database db2 :url (second +test-database-urls+)))
            (every #'(lambda (x y) (not (equalp x y)))
                   (multiple-value-list (methods-database-attributes db2))
                   attributes))
          (let ((m1 (lookup-methods-by-name "Meall nan Eun Surprise" :stage 10 :database db1))
                (m2 (lookup-methods-by-name "Grandsire" :stage 8 :database db1)))
            (assert-eql 1 (length m1))
            (assert-eql 1 (length m2)))
          (let ((m2 (lookup-methods-by-name "Grandsire" :stage 8 :database db1
                                             :update :force :url (second +test-database-urls+)))
                (m1 (lookup-methods-by-name "Meall nan Eun Surprise" :stage 10 :database db1)))
            (assert-eql 1 (length m1))
            (assert-eql 0 (length m2)))
          (let ((m2 (lookup-methods-by-notation "3,1x1x1x1x" :stage 8 :database db1
                                                 :update :force :url (first +test-database-urls+)))
                (m1 (lookup-methods-by-name "Meall nan Eun Surprise" :stage 10 :database db1)))
            (assert-eql 1 (length m1))
            (assert-eql 1 (length m2)))
          (let ((m1 (lookup-methods-by-name "Meall nan Eun Surprise" :stage 10 :database db1))
                (m2 (lookup-methods-from-changes (parse-place-notation "3.1x1x1x1x1x1x1x8" :stage 8)
                                                  :database db1
                                                  :update :force :url (second +test-database-urls+))))
            (assert-eql 1 (length m1))
            (assert-eql 0 (length m2))))))))

|#

(define-test test-call ()
  (labels ((check-call (call changes offset from-end fraction replace &key stage following)
             (assert-equalp changes (get-call-changes call (or stage (stage (first changes)))))
             (assert-eql offset (call-offset call))
             (assert-eq from-end (not (not (call-from-end call))))
             (assert-eql fraction (call-fraction call))
             (assert-eql replace (call-replace call))
             (assert-equalp following (call-following call))))
    (let ((c1 (call "X3.Et" :from-end :true :offset 5 :fraction 2/3))
          (c2 (call "3.123" :following "3")))
      (check-call c1 #12!x3.e 5 t 2/3 3)
      (check-call c2 #5!3.123 2 t nil 2 :following (call "3" :from-end nil))
      ;; (assert-true (ppcre:scan "#<CALL X3.Et 5 T 2/3 3 [^ ]+>" (prin1-to-string c1)))
      ;; (assert-true (ppcre:scan "#<CALL 3.123 2 T NIL 2 3 1 [^ ]+>" (prin1-to-string c2)))
      ;; (assert-equal "Call-X3.Et" (princ-to-string c1))
      ;; (assert-equal "Call-3.123*" (princ-to-string c2))
      ;; (let ((*print-readably* t))
      ;;   (assert-equal "(CALL \"X3.Et\" :OFFSET 5 :FRACTION 2/3 :REPLACE 3)"
      ;;                 (prin1-to-string c1))
      ;;   (assert-equal "(CALL \"3.123\" :OFFSET 2 :REPLACE 2 :FOLLOWING \"3\" :FOLLOWING-REPLACE 1)"
      ;;                 (prin1-to-string c2))))
      )
    (check-call (call "4") #8!4 1 t nil 1)
    (check-call (call nil :from-end nil :replace 2) nil 0 nil nil 2 :stage 6)
    (check-call (call nil :replace 2) nil 2 t nil 2 :stage 9)
    (check-call (call "5" :fraction 1/2) #8!5 1 t 1/2 1)
    (assert-equalp (call "456") (call "456" :offset nil :replace nil))
    (assert-error 'type-error (call 4))
    (assert-error 'type-error (call #8!4))
    (assert-error 'type-error (call (first #8!4)))
    (assert-error 'type-error (call t))
    (assert-error 'type-error (call "4" :offset -1))
    (assert-error 'type-error (call "4" :offset t))
    (assert-error 'type-error (call "1" :fraction 0))
    (assert-error 'type-error (call "3" :fraction 1))
    (assert-error 'type-error (call "5" :fraction 0.5))
    (assert-error 'type-error (call "7" :fraction t))
    (assert-error 'type-error (call "6" :replace -1))
    (assert-error 'type-error (call "6" :replace t))
    (assert-warning 'warning (call nil :replace 0))
    (assert-warning 'warning (call "" :replace 0))
    (assert-warning 'warning (call nil))
    (assert-warning 'warning (call ""))))

(define-test test-call-apply ()
  ;; (labels ((check-call-apply (result method &rest calls)
  ;;            (assert-equalp (list (parse-place-notation result :stage (method-stage method))
  ;;                                 nil)
  ;;                           (multiple-value-list (apply #'call-apply method calls)))))
  ;;   (iter (for s :from 6 :to +maximum-stage+ :by 2)
  ;;         (check-call-apply "x1x4,4"
  ;;                           (method :stage s :place-notation "x1x4,2")
  ;;                           (call "4")))
  ;;   (check-call-apply "3.1.5.3.1.345,145"
  ;;                     (lookup-method "Stedman" 5)
  ;;                     (call "145")
  ;;                     (call "345" :fraction 1/2))
  ;;   (check-call-apply "7.3.1.3.1.3"
  ;;                     (lookup-method "Erin" 9)
  ;;                     (call "7" :from-end nil))
  ;;   (check-call-apply "x4x2x3x4x5x4x3x2x4x3x2"
  ;;                     (lookup-method "Cambridge Surprise" 6)
  ;;                     nil
  ;;                     (call nil :replace 2 :from-end nil))
  ;;   (check-call-apply "x3x4x2x3x4x5x4x3x2x4x3x2.3x3x3"
  ;;                     (lookup-method "Cambridge Surprise" 6)
  ;;                     (call "3x3x3" :replace 0)
  ;;                     nil)
  ;;   (check-call-apply "x3x4x2x3x4x5x4x3x2x4x3x2"
  ;;                     (lookup-method "Cambridge Surprise" 6)
  ;;                     nil)
  ;;   (check-call-apply "x3x4x2x3x4x5x4x3x2x4x3x2"
  ;;                     (lookup-method "Cambridge Surprise" 6))
  ;;   (check-call-apply "x5x5x5x3x4x2x3x4x5x4x3x2x4x3x2"
  ;;                     (lookup-method "Cambridge Surprise" 6)
  ;;                     (call "x5x5x5" :from-end nil :replace 0))
  ;;   (check-call-apply "x3x4x2x3x4x5x4x3x2x4x"
  ;;                     (lookup-method "Cambridge Surprise" 6)
  ;;                     (call nil :replace 3 :offset 3))
  ;;   (check-call-apply "x3x4x2x3x4.234.5x6x4x3x2x4x3x2"
  ;;                     (lookup-method "Cambridge Surprise" 6)
  ;;                     (call "234.5x6" :fraction 1/2 :offset 2 :replace 2))
  ;;   (check-call-apply "x3x4x2x3.345.4x5x4x3x2x4x3x2"
  ;;                     (lookup-method "Cambridge Surprise" 6)
  ;;                     (call "3456" :from-end nil :fraction 1/3))
  ;;   (check-call-apply "x3x4x2x3x4x5x4x5x2x4x3x2"
  ;;                     (lookup-method "Cambridge Surprise" 6)
  ;;                     (call "5" :fraction 2/3))
  ;;   (check-call-apply "x5x5x5x3x4x2x3.345.4.234.5x6x4x5x2x4x"
  ;;                     (lookup-method "Cambridge Surprise" 6)
  ;;                     nil
  ;;                     (call nil :replace 3 :offset 3)
  ;;                     nil nil nil
  ;;                     (call "x5x5x5" :from-end nil :replace 0)
  ;;                     nil
  ;;                     (call "5" :fraction 2/3)
  ;;                     nil
  ;;                     (call "234.5x6" :fraction 1/2 :offset 2 :replace 2)
  ;;                     (call "3456" :from-end nil :fraction 1/3)
  ;;                     nil))
  ;; (let* ((m (lookup-method "Plain Bob" 5)))
  ;;   (multiple-value-bind (lead1 following)
  ;;       (call-apply m (call "23" :following "3"))
  ;;     (let ((lead2 (call-apply m following)))
  ;;       (assert-equalp (values (parse-place-notation "5.1.5.1.5.1.5.1.5.123" :stage 5))
  ;;                      lead1)
  ;;       (assert-equalp (values (parse-place-notation "3.1.5.1.5.1.5.1.5.125" :stage 5))
  ;;                      lead2))))
  ;; (let* ((m (lookup-method "Bastow Little Bob" 6)))
  ;;   (multiple-value-bind (lead1 following)
  ;;       (call-apply m (call "5.4" :replace 3 :following "5.6.3" :following-replace 2))
  ;;     (let ((lead2 (call-apply m following)))
  ;;       (assert-equalp (values (parse-place-notation "x5.4" :stage 6))
  ;;                      lead1)
  ;;       (assert-equalp (values (parse-place-notation "5.6.3x6" :stage 6))
  ;;                      lead2))))
  (assert-error 'type-error (call-apply nil))
  (assert-error 'type-error (call-apply "Vermont Delight" 8))
  ;; (assert-error 'type-error (call-apply (lookup-method "Cambridge Surprise" 8) "4"))
  ;; (assert-error 'type-error (call-apply (lookup-methods-by-name "Cambridge *" :stage 8)))
  ;; (assert-error 'type-error (call-apply (lookup-method "Cambridge Surprise" 8)
  ;;                                       (call "4") "234"))
  ;; (assert-error 'type-error (call-apply (lookup-method "Cambridge Surprise" 8)
  ;;                                       nil "234" (call "4") nil))
  (assert-error 'parse-error (call-apply (method-from-title "Messed Major" "x1x4,0")
                                         (call "4")))
  ;; (assert-error 'call-application-error (call-apply (lookup-method "Advent Surprise" 8)
  ;;                                                   (call "4" :fraction 63/64)))
  ;; (assert-error 'call-application-error (call-apply (lookup-method "Bastow Little Bob" 8)
  ;;                                                   (call "x1x4,8")))
  ;; (assert-error 'call-application-error (call-apply (lookup-method "Bastow Little Bob" 8)
  ;;                                                   (call "x1x4,8" :from-end nil)))
  ;; (assert-error 'call-application-error (call-apply (lookup-method "Pudsey Surprise" 12)
  ;;                                                   (call "36.58.3" :offset 2)))
  ;; (assert-error 'call-application-error (call-apply (lookup-method "Plain Bob" 5)
  ;;                                                   (call "145" :offset 2 :following "123"))))
)
