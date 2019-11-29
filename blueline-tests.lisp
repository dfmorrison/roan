;;;; Copyright (c) 2019 Donald F Morrison
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

(in-readtable :roan+interpol)

(define-test test-rotate-cycle ()
  ;; Note that freshly consed lists, not constants, must be used below 'cause
  ;; rotate-cycle does surgery on the list structure.
  (assert-equal '(3 4 1 2) (rotate-cycle (list 1 2 3 4) '(3 6)))
  (assert-equal '(1 2 3 4) (rotate-cycle (list 1 2 3 4) '(1)))
  (assert-equal '(1 2 3 4) (rotate-cycle (list 3 4 1 2) :smallest))
  (assert-equal '(4 1 2 3) (rotate-cycle (list 3 4 1 2) :largest))
  (assert-equal '(1 2 3 4) (rotate-cycle (list 2 3 4 1) :smallest))
  (assert-equal '(7) (rotate-cycle (list 7) :largest)))

(define-test test-partition-hunt-bells ()
  (labels ((test-it (result title target)
             (let ((*blueline-method* (lookup-method-by-title title)))
               (assert-equal result
                             (mapcar (rcurry #'sort #'<)
                                     (multiple-value-list
                                      (partition-hunt-bells target)))))))
    (test-it '((0) nil) "Advent Surprise Major" :first)
    (test-it '((0) nil) "Advent Surprise Major" :all)
    (test-it '(nil (0)) "Advent Surprise Major" :working)
    (test-it '(nil nil) "Advent Surprise Major" nil)
    (test-it '((0) nil) "Advent Surprise Major" '(0))
    (test-it '((0) nil) "Advent Surprise Major" '(0 2))
    (test-it '(nil (0)) "Advent Surprise Major" '(2))
    (test-it '((4 5) nil) "Seven Stars Major" :all)
    (test-it '((4) (5)) "Seven Stars Major" :first)
    (test-it '(nil (4 5)) "Seven Stars Major" :working)
    (test-it '(nil nil) "Seven Stars Major" nil)
    (test-it '((4) (5)) "Seven Stars Major" '(4 6))
    (test-it '((4 5) nil) "Seven Stars Major" '(3 4 5 6))))

(define-test test-blueline-destinations ()
  (labels ((run (d) (blueline d (lookup-method-by-title "Advent Surprise Major")))
           (compare (s) (assert-equal
              "<svg xmlns='http://www.w3.org/2000/svg' preserveaspectratio='xMidYMid meet' height='704' width='576'>
<style>.blueline{fill:none;stroke:hsl(240,100%,60%);stroke-width:1.4;stroke-linecap:round;stroke-linejoin:miter;}
.hunt{stroke:hsl(0,60%,60%);stroke-width:0.8}
.dot{fill:black;stroke:none;}
.figure{font-family:sans-serif;font-size:14px;}
.notation{font-family:sans-serif;font-size:11px;font-weight:lighter;font-style:italic;fill:dimgray}
.label{font-family:sans-serif;font-size:15px;font-weight:bolder;}</style>
<polyline class='blueline hunt' points='14,16 30,23 14,30 30,37 46,44 62,51 46,58 62,65 78,72 94,79 78,86 94,93 110,100 126,107 110,114 126,121 126,128 110,135 126,142 110,149 94,156 78,163 94,170 78,177 62,184 46,191 62,198 46,205 30,212 14,219 30,226 14,233 14,240 30,247 14,254 30,261 46,268 62,275 46,282 62,289 78,296 94,303 78,310 94,317 110,324 126,331 110,338 126,345 126,352 110,359 126,366 110,373 94,380 78,387 94,394 78,401 62,408 46,415 62,422 46,429 30,436 14,443 30,450 14,457 14,464 30,471 14,478 30,485 46,492 62,499 46,506 62,513 78,520 94,527 78,534 94,541 110,548 126,555 110,562 126,569 126,576 110,583 126,590 110,597 94,604 78,611 94,618 78,625 62,632 46,639 62,646 46,653 30,660 14,667 30,674 14,681 14,688'></polyline>
<polyline class='blueline' points='126,16 110,23 126,30 110,37 126,44 126,51 110,58 94,65 94,72 78,79 94,86 78,93 78,100 94,107 78,114 94,121 78,128 94,135 78,142 94,149 110,156 126,163 110,170 126,177 110,184 94,191 78,198 78,205 94,212 94,219 78,226 62,233 78,240 62,247 46,254 62,261 62,268 46,275 62,282 46,289 30,296 14,303 14,310 30,317 14,324 30,331 46,338 62,345 46,352 62,359 62,366 46,373 62,380 46,387 30,394 14,401 14,408 30,415 14,422 30,429 46,436 62,443 46,450 46,457 30,464 14,471 30,478 14,485 14,492 30,499 14,506 30,513 46,520 62,527 62,534 46,541 62,548 46,555 30,562 14,569 30,576 14,583 14,590 30,597 14,604 30,611 46,618 62,625 78,632 78,639 94,646 110,653 126,660 110,667 126,674 110,681 94,688'></polyline>
<text class='label' x='150' y='21'>8</text><circle cx='155' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='126' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='150' y='245'>5</text><circle cx='155' cy='240' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='78' cy='240' r='2.8' class='dot'></circle>
<text class='label' x='150' y='469'>2</text><circle cx='155' cy='464' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='30' cy='464' r='2.8' class='dot'></circle>
<text class='label' x='150' y='693'>6</text><circle cx='155' cy='688' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='94' cy='688' r='2.8' class='dot'></circle>
<polyline class='blueline hunt' points='200,16 216,23 200,30 216,37 232,44 248,51 232,58 248,65 264,72 280,79 264,86 280,93 296,100 312,107 296,114 312,121 312,128 296,135 312,142 296,149 280,156 264,163 280,170 264,177 248,184 232,191 248,198 232,205 216,212 200,219 216,226 200,233 200,240 216,247 200,254 216,261 232,268 248,275 232,282 248,289 264,296 280,303 264,310 280,317 296,324 312,331 296,338 312,345 312,352 296,359 312,366 296,373 280,380 264,387 280,394 264,401 248,408 232,415 248,422 232,429 216,436 200,443 216,450 200,457 200,464 216,471 200,478 216,485 232,492 248,499 232,506 248,513 264,520 280,527 264,534 280,541 296,548 312,555 296,562 312,569 312,576 296,583 312,590 296,597 280,604 264,611 280,618 264,625 248,632 232,639 248,646 232,653 216,660 200,667 216,674 200,681 200,688'></polyline>
<polyline class='blueline' points='280,16 280,23 264,30 264,37 280,44 296,51 312,58 312,65 296,72 312,79 296,86 312,93 312,100 296,107 312,114 296,121 296,128 312,135 296,142 312,149 312,156 296,163 312,170 296,177 312,184 312,191 296,198 280,205 264,212 264,219 280,226 280,233 296,240 312,247 296,254 312,261 296,268 280,275 264,282 264,289 248,296 232,303 216,310 200,317 216,324 200,331 200,338 216,345 200,352 216,359 232,366 248,373 232,380 248,387 248,394 232,401 216,408 200,415 216,422 200,429 200,436 216,443 200,450 216,457 232,464 232,471 248,478 232,485 216,492 200,499 216,506 200,513 200,520 216,527 232,534 248,541 232,548 248,555 248,562 232,569 248,576 232,583 216,590 200,597 216,604 200,611 200,618 216,625 232,632 248,639 232,646 248,653 248,660 232,667 248,674 264,681 248,688'></polyline>
<text class='label' x='336' y='21'>6</text><circle cx='341' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='280' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='336' y='245'>7</text><circle cx='341' cy='240' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='296' cy='240' r='2.8' class='dot'></circle>
<text class='label' x='336' y='469'>3</text><circle cx='341' cy='464' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='232' cy='464' r='2.8' class='dot'></circle>
<text class='label' x='336' y='693'>4</text><circle cx='341' cy='688' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='248' cy='688' r='2.8' class='dot'></circle>
<polyline class='blueline hunt' points='386,16 402,23 386,30 402,37 418,44 434,51 418,58 434,65 450,72 466,79 450,86 466,93 482,100 498,107 482,114 498,121 498,128 482,135 498,142 482,149 466,156 450,163 466,170 450,177 434,184 418,191 434,198 418,205 402,212 386,219 402,226 386,233 386,240'></polyline>
<polyline class='blueline' points='434,16 450,23 466,30 466,37 450,44 450,51 466,58 482,65 498,72 482,79 498,86 482,93 466,100 450,107 466,114 450,121 466,128 450,135 466,142 450,149 450,156 466,163 450,170 466,177 466,184 482,191 498,198 498,205 482,212 498,219 482,226 498,233 498,240'></polyline>
<text class='label' x='522' y='21'>4</text><circle cx='527' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='434' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='522' y='245'>8</text><circle cx='527' cy='240' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='498' cy='240' r='2.8' class='dot'></circle>
</svg>
"
              s)))
    (compare (run nil))
    (compare (run (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (compare (with-output-to-string (s) (run s)))
    (compare (with-output-to-string (*standard-output*) (run t)))
    (compare (let ((temp nil))
               (unwind-protect
                    (progn
                      (fad:with-open-temporary-file (s :keep t)
                        (setf temp (pathname s))
                        (format s "Junk~%"))
                      (run temp)
                      (read-file-into-string temp))
                 (when (probe-file temp)
                   (delete-file temp)))))))

(defmacro define-blueline-test (method-title (&rest args) expected)
  (let ((test-name (format-symbol t "BLUELINE-TEST-~@:(~A~)" (ppcre:scan-to-strings #?/\S+/ method-title))))
    `(define-test ,test-name ()
       (let ((#0=#:method (lookup-method-by-title ',method-title)))
         (assert-equal ',expected (blueline nil #0# ,@args))
         (let ((*blueline-default-parameters* ',args))
           (assert-equal ',expected (blueline nil #0#)))))))

(define-blueline-test "Advent Surprise Major" (:layout :grid)
  "<svg xmlns='http://www.w3.org/2000/svg' preserveaspectratio='xMidYMid meet' height='256' width='164'>
<style>.blueline{fill:none;stroke:hsl(240,100%,60%);stroke-width:1.4;stroke-linecap:round;stroke-linejoin:miter;}
.hunt{stroke:hsl(0,100%,60%);stroke-width:1.4}
.dot{fill:black;stroke:none;}
.figure{font-family:sans-serif;font-size:14px;}
.notation{font-family:sans-serif;font-size:11px;font-weight:lighter;font-style:italic;fill:dimgray}
.label{font-family:sans-serif;font-size:15px;font-weight:bolder;}</style>
<polyline class='blueline hunt' points='14,16 30,23 14,30 30,37 46,44 62,51 46,58 62,65 78,72 94,79 78,86 94,93 110,100 126,107 110,114 126,121 126,128 110,135 126,142 110,149 94,156 78,163 94,170 78,177 62,184 46,191 62,198 46,205 30,212 14,219 30,226 14,233 14,240'></polyline>
<polyline class='blueline' points='126,16 110,23 126,30 110,37 126,44 126,51 110,58 94,65 94,72 78,79 94,86 78,93 78,100 94,107 78,114 94,121 78,128 94,135 78,142 94,149 110,156 126,163 110,170 126,177 110,184 94,191 78,198 78,205 94,212 94,219 78,226 62,233 78,240'></polyline>
<polyline class='blueline' points='78,16 62,23 46,30 62,37 62,44 46,51 62,58 46,65 30,72 14,79 14,86 30,93 14,100 30,107 46,114 62,121 46,128 62,135 62,142 46,149 62,156 46,163 30,170 14,177 14,184 30,191 14,198 30,205 46,212 62,219 46,226 46,233 30,240'></polyline>
<polyline class='blueline' points='30,16 14,23 30,30 14,37 14,44 30,51 14,58 30,65 46,72 62,79 62,86 46,93 62,100 46,107 30,114 14,121 30,128 14,135 14,142 30,149 14,156 30,163 46,170 62,177 78,184 78,191 94,198 110,205 126,212 110,219 126,226 110,233 94,240'></polyline>
<polyline class='blueline' points='94,16 94,23 78,30 78,37 94,44 110,51 126,58 126,65 110,72 126,79 110,86 126,93 126,100 110,107 126,114 110,121 110,128 126,135 110,142 126,149 126,156 110,163 126,170 110,177 126,184 126,191 110,198 94,205 78,212 78,219 94,226 94,233 110,240'></polyline>
<polyline class='blueline' points='110,16 126,23 110,30 126,37 110,44 94,51 78,58 78,65 62,72 46,79 30,86 14,93 30,100 14,107 14,114 30,121 14,128 30,135 46,142 62,149 46,156 62,163 62,170 46,177 30,184 14,191 30,198 14,205 14,212 30,219 14,226 30,233 46,240'></polyline>
<polyline class='blueline' points='46,16 46,23 62,30 46,37 30,44 14,51 30,58 14,65 14,72 30,79 46,86 62,93 46,100 62,107 62,114 46,121 62,128 46,135 30,142 14,149 30,156 14,163 14,170 30,177 46,184 62,191 46,198 62,205 62,212 46,219 62,226 78,233 62,240'></polyline>
<polyline class='blueline' points='62,16 78,23 94,30 94,37 78,44 78,51 94,58 110,65 126,72 110,79 126,86 110,93 94,100 78,107 94,114 78,121 94,128 78,135 94,142 78,149 78,156 94,163 78,170 94,177 94,184 110,191 126,198 126,205 110,212 126,219 110,226 126,233 126,240'></polyline>
</svg>
")

(define-blueline-test "Seven Stars Major" (:layout :grid :figures :head :place-notation :half)
"<svg xmlns='http://www.w3.org/2000/svg' preserveaspectratio='xMidYMid meet' height='424' width='236'>
<style>.blueline{fill:none;stroke:hsl(240,100%,60%);stroke-width:1.4;stroke-linecap:round;stroke-linejoin:miter;}
.hunt{stroke:hsl(0,100%,60%);stroke-width:1.4}
.dot{fill:black;stroke:none;}
.figure{font-family:sans-serif;font-size:14px;}
.notation{font-family:sans-serif;font-size:11px;font-weight:lighter;font-style:italic;fill:dimgray}
.label{font-family:sans-serif;font-size:15px;font-weight:bolder;}</style>
<text class='notation' x='26' y='26' text-anchor='end'>x</text>
<text class='notation' x='26' y='40' text-anchor='end'>5</text>
<text class='notation' x='26' y='54' text-anchor='end'>x</text>
<text class='notation' x='26' y='68' text-anchor='end'>4</text>
<text class='notation' x='26' y='82' text-anchor='end'>5</text>
<text class='notation' x='26' y='96' text-anchor='end'>x</text>
<text class='notation' x='26' y='110' text-anchor='end'>5</text>
<text class='notation' x='26' y='124' text-anchor='end'>x</text>
<text class='notation' x='26' y='138' text-anchor='end'>4</text>
<text class='notation' x='26' y='152' text-anchor='end'>x</text>
<text class='notation' x='26' y='166' text-anchor='end'>5</text>
<text class='notation' x='26' y='180' text-anchor='end'>x</text>
<text class='notation' x='26' y='194' text-anchor='end'>36</text>
<text class='notation' x='26' y='208' text-anchor='end'>x</text>
<text class='notation' x='26' y='404' text-anchor='end'>2</text>
<text class='figure' x='40' y='20'>1</text><text class='figure' x='56' y='20'>2</text><text class='figure' x='72' y='20'>3</text><text class='figure' x='88' y='20'>4</text><text class='figure' x='104' y='20'>5</text><text class='figure' x='120' y='20'>6</text><text class='figure' x='136' y='20'>7</text><text class='figure' x='152' y='20'>8</text>
<polyline class='blueline hunt' points='108,16 124,30 140,44 156,58 140,72 124,86 108,100 108,114 124,128 108,142 124,156 140,170 156,184 140,198 156,212 140,226 156,240 156,254 140,268 156,282 140,296 124,310 108,324 108,338 124,352 108,366 108,380 124,394 108,408'></polyline>
<polyline class='blueline' points='156,16 140,30 124,44 108,58 124,72 140,86 156,100 156,114 140,128 156,142 140,156 124,170 108,184 92,198 76,212 76,226 92,240 76,254 92,268 92,282 76,296 92,310 76,324 92,338 92,352 76,366 92,380 76,394 92,408'></polyline>
<polyline class='blueline' points='92,16 76,30 92,44 76,58 60,72 44,86 60,100 44,114 60,128 76,142 92,156 76,170 92,184 108,198 124,212 124,226 108,240 108,254 124,268 108,282 124,296 140,310 156,324 156,338 140,352 156,366 156,380 140,394 156,408'></polyline>
<polyline class='blueline' points='140,16 156,30 156,44 140,58 156,72 156,86 140,100 124,114 108,128 124,142 108,156 108,170 124,184 124,198 108,212 92,226 76,240 92,254 76,268 60,282 44,296 60,310 44,324 60,338 76,352 92,366 76,380 92,394 76,408'></polyline>
<polyline class='blueline' points='76,16 92,30 76,44 92,58 92,72 76,86 92,100 76,114 92,128 92,142 76,156 92,170 76,184 76,198 92,212 108,226 124,240 140,254 156,268 140,282 156,296 156,310 140,324 124,338 108,352 124,366 140,380 156,394 140,408'></polyline>
<polyline class='blueline' points='124,16 108,30 108,44 124,58 108,72 108,86 124,100 140,114 156,128 140,142 156,156 156,170 140,184 156,198 140,212 156,226 140,240 124,254 108,268 124,282 108,296 108,310 124,324 140,338 156,352 140,366 124,380 108,394 124,408'></polyline>
<polyline class='blueline' points='60,16 44,30 60,44 44,58 44,72 60,86 44,100 60,114 44,128 44,142 60,156 44,170 60,184 44,198 60,212 44,226 60,240 44,254 60,268 76,282 92,296 76,310 92,324 76,338 60,352 44,366 60,380 44,394 44,408'></polyline>
<polyline class='blueline' points='44,16 60,30 44,44 60,58 76,72 92,86 76,100 92,114 76,128 60,142 44,156 60,170 44,184 60,198 44,212 60,226 44,240 60,254 44,268 44,282 60,296 44,310 60,324 44,338 44,352 60,366 44,380 60,394 60,408'></polyline>
</svg>
")

(define-blueline-test "Bristol Surprise Maximus" (:layout 40 :hunt-bell nil)
"<svg xmlns='http://www.w3.org/2000/svg' preserveaspectratio='xMidYMid meet' height='368' width='2768'>
<style>.blueline{fill:none;stroke:hsl(240,100%,60%);stroke-width:1.4;stroke-linecap:round;stroke-linejoin:miter;}
.hunt{stroke:hsl(0,60%,60%);stroke-width:0.8}
.dot{fill:black;stroke:none;}
.figure{font-family:sans-serif;font-size:14px;}
.notation{font-family:sans-serif;font-size:11px;font-weight:lighter;font-style:italic;fill:dimgray}
.label{font-family:sans-serif;font-size:15px;font-weight:bolder;}</style>
<polyline class='blueline' points='190,16 174,23 158,30 142,37 158,44 174,51 190,58 190,65 174,72 190,79 174,86 158,93 142,100 158,107 142,114 142,121 126,128 126,135 110,142 94,149 78,156 94,163 110,170 126,177 142,184 158,191 142,198 158,205 174,212 190,219 174,226 190,233 174,240 158,247 142,254 158,261 142,268 126,275 110,282 126,289 110,296 94,303 78,310 78,317 94,324 78,331 78,338 94,345 110,352'></polyline>
<text class='label' x='214' y='21'>T</text><circle cx='219' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='190' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='214' y='357'>7</text><circle cx='219' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='110' cy='352' r='2.8' class='dot'></circle>
<polyline class='blueline' points='360,16 376,23 392,30 408,37 392,44 376,51 360,58 344,65 344,72 328,79 344,86 328,93 328,100 312,107 296,114 312,121 296,128 280,135 264,142 264,149 280,156 264,163 264,170 280,177 296,184 312,191 328,198 344,205 328,212 312,219 296,226 280,233 264,240 280,247 264,254 264,261 280,268 264,275 280,282 296,289 296,296 312,303 296,310 312,317 312,324 296,331 312,338 296,345 280,352'></polyline>
<text class='label' x='464' y='21'>7</text><circle cx='469' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='360' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='464' y='357'>2</text><circle cx='469' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='280' cy='352' r='2.8' class='dot'></circle>
<polyline class='blueline' points='530,16 514,23 530,30 514,37 514,44 530,51 514,58 530,65 514,72 514,79 530,86 514,93 530,100 546,107 562,114 546,121 562,128 578,135 594,142 610,149 626,156 610,163 594,170 578,177 562,184 546,191 530,198 514,205 530,212 546,219 562,226 578,233 594,240 578,247 594,254 594,261 610,268 610,275 626,282 610,289 626,296 642,303 658,310 674,317 690,324 674,331 658,338 642,345 626,352'></polyline>
<text class='label' x='714' y='21'>2</text><circle cx='719' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='530' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='714' y='357'>8</text><circle cx='719' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='626' cy='352' r='2.8' class='dot'></circle>
<polyline class='blueline' points='876,16 860,23 844,30 828,37 844,44 860,51 876,58 892,65 908,72 892,79 908,86 924,93 940,100 924,107 940,114 940,121 924,128 940,135 924,142 940,149 940,156 924,163 940,170 924,177 908,184 892,191 908,198 892,205 892,212 908,219 892,226 908,233 908,240 924,247 940,254 924,261 940,268 940,275 924,282 940,289 924,296 908,303 892,310 876,317 860,324 876,331 892,338 908,345 924,352'></polyline>
<text class='label' x='964' y='21'>8</text><circle cx='969' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='876' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='964' y='357'>E</text><circle cx='969' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='924' cy='352' r='2.8' class='dot'></circle>
<polyline class='blueline' points='1174,16 1190,23 1190,30 1174,37 1190,44 1190,51 1174,58 1158,65 1142,72 1158,79 1142,86 1126,93 1126,100 1110,107 1126,114 1110,121 1110,128 1094,135 1078,142 1062,149 1046,156 1062,163 1078,170 1094,177 1110,184 1126,191 1126,198 1110,205 1126,212 1126,219 1110,226 1094,233 1078,240 1094,247 1078,254 1062,261 1046,268 1062,275 1046,282 1030,289 1014,296 1030,303 1014,310 1030,317 1046,324 1062,331 1046,338 1062,345 1078,352'></polyline>
<text class='label' x='1214' y='21'>E</text><circle cx='1219' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='1174' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='1214' y='357'>5</text><circle cx='1219' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='1078' cy='352' r='2.8' class='dot'></circle>
<polyline class='blueline' points='1328,16 1344,23 1360,30 1376,37 1360,44 1344,51 1328,58 1328,65 1312,72 1312,79 1296,86 1312,93 1296,100 1280,107 1264,114 1280,121 1264,128 1264,135 1280,142 1296,149 1312,156 1296,163 1280,170 1264,177 1264,184 1280,191 1296,198 1312,205 1296,212 1280,219 1264,226 1264,233 1280,240 1264,247 1280,254 1296,261 1312,268 1296,275 1312,282 1312,289 1328,296 1328,303 1344,310 1360,317 1376,324 1360,331 1344,338 1328,345 1312,352'></polyline>
<text class='label' x='1464' y='21'>5</text><circle cx='1469' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='1328' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='1464' y='357'>4</text><circle cx='1469' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='1312' cy='352' r='2.8' class='dot'></circle>
<polyline class='blueline' points='1562,16 1546,23 1562,30 1546,37 1530,44 1514,51 1530,58 1514,65 1530,72 1546,79 1562,86 1546,93 1562,100 1578,107 1594,114 1578,121 1594,128 1610,135 1626,142 1626,149 1610,156 1626,163 1626,170 1610,177 1594,184 1578,191 1562,198 1546,205 1562,212 1578,219 1594,226 1610,233 1610,240 1626,247 1610,254 1626,261 1626,268 1642,275 1658,282 1642,289 1658,296 1674,303 1690,310 1690,317 1674,324 1690,331 1690,338 1674,345 1658,352'></polyline>
<text class='label' x='1714' y='21'>4</text><circle cx='1719' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='1562' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='1714' y='357'>0</text><circle cx='1719' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='1658' cy='352' r='2.8' class='dot'></circle>
<polyline class='blueline' points='1908,16 1892,23 1876,30 1860,37 1876,44 1892,51 1908,58 1924,65 1940,72 1924,79 1940,86 1940,93 1924,100 1940,107 1924,114 1908,121 1908,128 1892,135 1908,142 1892,149 1892,156 1908,163 1892,170 1908,177 1924,184 1940,191 1924,198 1940,205 1940,212 1924,219 1940,226 1924,233 1940,240 1940,247 1924,254 1940,261 1924,268 1908,275 1892,282 1908,289 1892,296 1876,303 1860,310 1844,317 1828,324 1844,331 1860,338 1876,345 1892,352'></polyline>
<text class='label' x='1964' y='21'>0</text><circle cx='1969' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='1908' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='1964' y='357'>9</text><circle cx='1969' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='1892' cy='352' r='2.8' class='dot'></circle>
<polyline class='blueline' points='2142,16 2158,23 2174,30 2190,37 2174,44 2158,51 2142,58 2126,65 2110,72 2126,79 2110,86 2110,93 2094,100 2094,107 2078,114 2094,121 2078,128 2062,135 2046,142 2030,149 2014,156 2030,163 2046,170 2062,177 2078,184 2094,191 2110,198 2126,205 2110,212 2094,219 2078,226 2062,233 2046,240 2062,247 2046,254 2030,261 2014,268 2030,275 2014,282 2014,289 2030,296 2014,303 2030,310 2014,317 2014,324 2030,331 2014,338 2030,345 2046,352'></polyline>
<text class='label' x='2214' y='21'>9</text><circle cx='2219' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='2142' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='2214' y='357'>3</text><circle cx='2219' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='2046' cy='352' r='2.8' class='dot'></circle>
<polyline class='blueline' points='2296,16 2312,23 2296,30 2312,37 2312,44 2296,51 2312,58 2296,65 2296,72 2280,79 2264,86 2280,93 2264,100 2264,107 2280,114 2264,121 2280,128 2296,135 2312,142 2328,149 2344,156 2328,163 2312,170 2296,177 2280,184 2264,191 2264,198 2280,205 2264,212 2264,219 2280,226 2296,233 2312,240 2296,247 2312,254 2328,261 2328,268 2344,275 2328,282 2344,289 2344,296 2360,303 2376,310 2392,317 2408,324 2392,331 2376,338 2360,345 2344,352'></polyline>
<text class='label' x='2464' y='21'>3</text><circle cx='2469' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='2296' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='2464' y='357'>6</text><circle cx='2469' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='2344' cy='352' r='2.8' class='dot'></circle>
<polyline class='blueline' points='2594,16 2578,23 2578,30 2594,37 2578,44 2578,51 2594,58 2610,65 2626,72 2610,79 2626,86 2642,93 2658,100 2642,107 2658,114 2674,121 2690,128 2674,135 2690,142 2674,149 2658,156 2642,163 2658,170 2642,177 2626,184 2610,191 2594,198 2578,205 2594,212 2610,219 2626,226 2626,233 2642,240 2642,247 2658,254 2642,261 2658,268 2674,275 2690,282 2674,289 2690,296 2690,303 2674,310 2658,317 2642,324 2658,331 2674,338 2690,345 2690,352'></polyline>
<text class='label' x='2714' y='21'>6</text><circle cx='2719' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='2594' cy='16' r='2.8' class='dot'></circle>
<text class='label' x='2714' y='357'>T</text><circle cx='2719' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='2690' cy='352' r='2.8' class='dot'></circle>
</svg>
")

(define-blueline-test "Grandsire Caters" (:hunt-bell 1 :working-bell 5 :layout nil :figures :lead :place-notation t :place-bells :dot)
  "<svg xmlns='http://www.w3.org/2000/svg' preserveaspectratio='xMidYMid meet' height='1796' width='362'>
<style>.blueline{fill:none;stroke:hsl(240,100%,60%);stroke-width:1.4;stroke-linecap:round;stroke-linejoin:miter;}
.hunt{stroke:hsl(0,60%,60%);stroke-width:0.8}
.dot{fill:black;stroke:none;}
.figure{font-family:sans-serif;font-size:14px;}
.notation{font-family:sans-serif;font-size:11px;font-weight:lighter;font-style:italic;fill:dimgray}
.label{font-family:sans-serif;font-size:15px;font-weight:bolder;}</style>
<text class='notation' x='20' y='26' text-anchor='end'>3</text>
<text class='notation' x='20' y='40' text-anchor='end'>1</text>
<text class='notation' x='20' y='54' text-anchor='end'>9</text>
<text class='notation' x='20' y='68' text-anchor='end'>1</text>
<text class='notation' x='20' y='82' text-anchor='end'>9</text>
<text class='notation' x='20' y='96' text-anchor='end'>1</text>
<text class='notation' x='20' y='110' text-anchor='end'>9</text>
<text class='notation' x='20' y='124' text-anchor='end'>1</text>
<text class='notation' x='20' y='138' text-anchor='end'>9</text>
<text class='notation' x='20' y='152' text-anchor='end'>1</text>
<text class='notation' x='20' y='166' text-anchor='end'>9</text>
<text class='notation' x='20' y='180' text-anchor='end'>1</text>
<text class='notation' x='20' y='194' text-anchor='end'>9</text>
<text class='notation' x='20' y='208' text-anchor='end'>1</text>
<text class='notation' x='20' y='222' text-anchor='end'>9</text>
<text class='notation' x='20' y='236' text-anchor='end'>1</text>
<text class='notation' x='20' y='250' text-anchor='end'>9</text>
<text class='notation' x='20' y='264' text-anchor='end'>1</text>
<text class='figure' x='34' y='20'>1</text><text class='figure' x='50' y='20'>2</text><text class='figure' x='66' y='20'>3</text><text class='figure' x='82' y='20'>4</text><text class='figure' x='98' y='20'>5</text><text class='figure' x='114' y='20'>6</text><text class='figure' x='130' y='20'>7</text><text class='figure' x='146' y='20'>8</text><text class='figure' x='162' y='20'>9</text>
<text class='figure' x='66' y='34'>3</text><text class='figure' x='82' y='34'>5</text><text class='figure' x='98' y='34'>4</text><text class='figure' x='114' y='34'>7</text><text class='figure' x='130' y='34'>6</text><text class='figure' x='146' y='34'>9</text><text class='figure' x='162' y='34'>8</text>
<text class='figure' x='50' y='48'>3</text><text class='figure' x='82' y='48'>4</text><text class='figure' x='98' y='48'>5</text><text class='figure' x='114' y='48'>6</text><text class='figure' x='130' y='48'>7</text><text class='figure' x='146' y='48'>8</text><text class='figure' x='162' y='48'>9</text>
<text class='figure' x='34' y='62'>3</text><text class='figure' x='66' y='62'>4</text><text class='figure' x='98' y='62'>6</text><text class='figure' x='114' y='62'>5</text><text class='figure' x='130' y='62'>8</text><text class='figure' x='146' y='62'>7</text><text class='figure' x='162' y='62'>9</text>
<text class='figure' x='34' y='76'>3</text><text class='figure' x='50' y='76'>4</text><text class='figure' x='82' y='76'>6</text><text class='figure' x='114' y='76'>8</text><text class='figure' x='130' y='76'>5</text><text class='figure' x='146' y='76'>9</text><text class='figure' x='162' y='76'>7</text>
<text class='figure' x='34' y='90'>4</text><text class='figure' x='50' y='90'>3</text><text class='figure' x='66' y='90'>6</text><text class='figure' x='98' y='90'>8</text><text class='figure' x='130' y='90'>9</text><text class='figure' x='146' y='90'>5</text><text class='figure' x='162' y='90'>7</text>
<text class='figure' x='34' y='104'>4</text><text class='figure' x='50' y='104'>6</text><text class='figure' x='66' y='104'>3</text><text class='figure' x='82' y='104'>8</text><text class='figure' x='114' y='104'>9</text><text class='figure' x='146' y='104'>7</text><text class='figure' x='162' y='104'>5</text>
<text class='figure' x='34' y='118'>6</text><text class='figure' x='50' y='118'>4</text><text class='figure' x='66' y='118'>8</text><text class='figure' x='82' y='118'>3</text><text class='figure' x='98' y='118'>9</text><text class='figure' x='130' y='118'>7</text><text class='figure' x='162' y='118'>5</text>
<text class='figure' x='34' y='132'>6</text><text class='figure' x='50' y='132'>8</text><text class='figure' x='66' y='132'>4</text><text class='figure' x='82' y='132'>9</text><text class='figure' x='98' y='132'>3</text><text class='figure' x='114' y='132'>7</text><text class='figure' x='146' y='132'>5</text>
<text class='figure' x='34' y='146'>8</text><text class='figure' x='50' y='146'>6</text><text class='figure' x='66' y='146'>9</text><text class='figure' x='82' y='146'>4</text><text class='figure' x='98' y='146'>7</text><text class='figure' x='114' y='146'>3</text><text class='figure' x='130' y='146'>5</text>
<text class='figure' x='34' y='160'>8</text><text class='figure' x='50' y='160'>9</text><text class='figure' x='66' y='160'>6</text><text class='figure' x='82' y='160'>7</text><text class='figure' x='98' y='160'>4</text><text class='figure' x='114' y='160'>5</text><text class='figure' x='130' y='160'>3</text>
<text class='figure' x='34' y='174'>9</text><text class='figure' x='50' y='174'>8</text><text class='figure' x='66' y='174'>7</text><text class='figure' x='82' y='174'>6</text><text class='figure' x='98' y='174'>5</text><text class='figure' x='114' y='174'>4</text><text class='figure' x='146' y='174'>3</text>
<text class='figure' x='34' y='188'>9</text><text class='figure' x='50' y='188'>7</text><text class='figure' x='66' y='188'>8</text><text class='figure' x='82' y='188'>5</text><text class='figure' x='98' y='188'>6</text><text class='figure' x='130' y='188'>4</text><text class='figure' x='162' y='188'>3</text>
<text class='figure' x='34' y='202'>7</text><text class='figure' x='50' y='202'>9</text><text class='figure' x='66' y='202'>5</text><text class='figure' x='82' y='202'>8</text><text class='figure' x='114' y='202'>6</text><text class='figure' x='146' y='202'>4</text><text class='figure' x='162' y='202'>3</text>
<text class='figure' x='34' y='216'>7</text><text class='figure' x='50' y='216'>5</text><text class='figure' x='66' y='216'>9</text><text class='figure' x='98' y='216'>8</text><text class='figure' x='130' y='216'>6</text><text class='figure' x='146' y='216'>3</text><text class='figure' x='162' y='216'>4</text>
<text class='figure' x='34' y='230'>5</text><text class='figure' x='50' y='230'>7</text><text class='figure' x='82' y='230'>9</text><text class='figure' x='114' y='230'>8</text><text class='figure' x='130' y='230'>3</text><text class='figure' x='146' y='230'>6</text><text class='figure' x='162' y='230'>4</text>
<text class='figure' x='34' y='244'>5</text><text class='figure' x='66' y='244'>7</text><text class='figure' x='98' y='244'>9</text><text class='figure' x='114' y='244'>3</text><text class='figure' x='130' y='244'>8</text><text class='figure' x='146' y='244'>4</text><text class='figure' x='162' y='244'>6</text>
<text class='figure' x='50' y='258'>5</text><text class='figure' x='82' y='258'>7</text><text class='figure' x='98' y='258'>3</text><text class='figure' x='114' y='258'>9</text><text class='figure' x='130' y='258'>4</text><text class='figure' x='146' y='258'>8</text><text class='figure' x='162' y='258'>6</text>
<text class='figure' x='66' y='272'>5</text><text class='figure' x='82' y='272'>3</text><text class='figure' x='98' y='272'>7</text><text class='figure' x='114' y='272'>4</text><text class='figure' x='130' y='272'>9</text><text class='figure' x='146' y='272'>6</text><text class='figure' x='162' y='272'>8</text>
<polyline class='blueline hunt' points='54,16 38,30 38,44 54,58 70,72 86,86 102,100 118,114 134,128 150,142 166,156 166,170 150,184 134,198 118,212 102,226 86,240 70,254 54,268'></polyline>
<polyline class='blueline' points='38,16 54,30 70,44 86,58 102,72 118,86 134,100 150,114 166,128 166,142 150,156 134,170 118,184 102,198 86,212 70,226 54,240 38,254 38,268'></polyline>
<circle cx='38' cy='268' r='2.8' class='dot'></circle>
<text class='notation' x='187' y='26' text-anchor='end'>3</text>
<text class='notation' x='187' y='40' text-anchor='end'>1</text>
<text class='notation' x='187' y='54' text-anchor='end'>9</text>
<text class='notation' x='187' y='68' text-anchor='end'>1</text>
<text class='notation' x='187' y='82' text-anchor='end'>9</text>
<text class='notation' x='187' y='96' text-anchor='end'>1</text>
<text class='notation' x='187' y='110' text-anchor='end'>9</text>
<text class='notation' x='187' y='124' text-anchor='end'>1</text>
<text class='notation' x='187' y='138' text-anchor='end'>9</text>
<text class='notation' x='187' y='152' text-anchor='end'>1</text>
<text class='notation' x='187' y='166' text-anchor='end'>9</text>
<text class='notation' x='187' y='180' text-anchor='end'>1</text>
<text class='notation' x='187' y='194' text-anchor='end'>9</text>
<text class='notation' x='187' y='208' text-anchor='end'>1</text>
<text class='notation' x='187' y='222' text-anchor='end'>9</text>
<text class='notation' x='187' y='236' text-anchor='end'>1</text>
<text class='notation' x='187' y='250' text-anchor='end'>9</text>
<text class='notation' x='187' y='264' text-anchor='end'>1</text>
<polyline class='blueline hunt' points='221,16 205,30 205,44 221,58 237,72 253,86 269,100 285,114 301,128 317,142 333,156 333,170 317,184 301,198 285,212 269,226 253,240 237,254 221,268 205,282 205,296 221,310 237,324 253,338 269,352 285,366 301,380 317,394 333,408 333,422 317,436 301,450 285,464 269,478 253,492 237,506 221,520 205,534 205,548 221,562 237,576 253,590 269,604 285,618 301,632 317,646 333,660 333,674 317,688 301,702 285,716 269,730 253,744 237,758 221,772 205,786 205,800 221,814 237,828 253,842 269,856 285,870 301,884 317,898 333,912 333,926 317,940 301,954 285,968 269,982 253,996 237,1010 221,1024 205,1038 205,1052 221,1066 237,1080 253,1094 269,1108 285,1122 301,1136 317,1150 333,1164 333,1178 317,1192 301,1206 285,1220 269,1234 253,1248 237,1262 221,1276 205,1290 205,1304 221,1318 237,1332 253,1346 269,1360 285,1374 301,1388 317,1402 333,1416 333,1430 317,1444 301,1458 285,1472 269,1486 253,1500 237,1514 221,1528 205,1542 205,1556 221,1570 237,1584 253,1598 269,1612 285,1626 301,1640 317,1654 333,1668 333,1682 317,1696 301,1710 285,1724 269,1738 253,1752 237,1766 221,1780'></polyline>
<polyline class='blueline' points='285,16 301,30 285,44 269,58 253,72 237,86 221,100 205,114 205,128 221,142 237,156 253,170 269,184 285,198 301,212 317,226 333,240 333,254 317,268 333,282 317,296 301,310 285,324 269,338 253,352 237,366 221,380 205,394 205,408 221,422 237,436 253,450 269,464 285,478 301,492 317,506 333,520 317,534 333,548 333,562 317,576 301,590 285,604 269,618 253,632 237,646 221,660 205,674 205,688 221,702 237,716 253,730 269,744 285,758 301,772 285,786 301,800 317,814 333,828 333,842 317,856 301,870 285,884 269,898 253,912 237,926 221,940 205,954 205,968 221,982 237,996 253,1010 269,1024 253,1038 269,1052 285,1066 301,1080 317,1094 333,1108 333,1122 317,1136 301,1150 285,1164 269,1178 253,1192 237,1206 221,1220 205,1234 205,1248 221,1262 237,1276 237,1290 221,1304 205,1318 205,1332 221,1346 237,1360 253,1374 269,1388 285,1402 301,1416 317,1430 333,1444 333,1458 317,1472 301,1486 285,1500 269,1514 253,1528 269,1542 253,1556 237,1570 221,1584 205,1598 205,1612 221,1626 237,1640 253,1654 269,1668 285,1682 301,1696 317,1710 333,1724 333,1738 317,1752 301,1766 285,1780'></polyline>
<circle cx='285' cy='16' r='2.8' class='dot'></circle>
<circle cx='317' cy='268' r='2.8' class='dot'></circle>
<circle cx='333' cy='520' r='2.8' class='dot'></circle>
<circle cx='301' cy='772' r='2.8' class='dot'></circle>
<circle cx='269' cy='1024' r='2.8' class='dot'></circle>
<circle cx='237' cy='1276' r='2.8' class='dot'></circle>
<circle cx='253' cy='1528' r='2.8' class='dot'></circle>
<circle cx='285' cy='1780' r='2.8' class='dot'></circle>
</svg>
")

(define-blueline-test "Down Maximus" (:hunt-bell :all :working-bell :smallest :place-bells nil)
  "<svg xmlns='http://www.w3.org/2000/svg' preserveaspectratio='xMidYMid meet' height='312' width='1028'>
<style>.blueline{fill:none;stroke:hsl(240,100%,60%);stroke-width:1.4;stroke-linecap:round;stroke-linejoin:miter;}
.hunt{stroke:hsl(0,60%,60%);stroke-width:0.8}
.dot{fill:black;stroke:none;}
.figure{font-family:sans-serif;font-size:14px;}
.notation{font-family:sans-serif;font-size:11px;font-weight:lighter;font-style:italic;fill:dimgray}
.label{font-family:sans-serif;font-size:15px;font-weight:bolder;}</style>
<polyline class='blueline hunt' points='142,16 126,23 110,30 126,37 142,44 158,51 174,58 190,65 174,72 190,79 174,86 190,93 174,100 190,107 190,114 174,121 158,128 142,135 158,142 158,149 142,156 126,163 110,170 126,177 142,184 158,191 174,198 190,205 174,212 190,219 174,226 190,233 174,240 190,247 190,254 174,261 158,268 142,275 158,282 158,289 142,296'></polyline>
<polyline class='blueline hunt' points='158,16 158,23 142,30 158,37 174,44 190,51 190,58 174,65 190,72 174,79 190,86 174,93 190,100 174,107 158,114 142,121 126,128 110,135 126,142 142,149 158,156 158,163 142,170 158,177 174,184 190,191 190,198 174,205 190,212 174,219 190,226 174,233 190,240 174,247 158,254 142,261 126,268 110,275 126,282 142,289 158,296'></polyline>
<polyline class='blueline' points='14,16 14,23 30,30 14,37 14,44 30,51 14,58 14,65 30,72 46,79 62,86 78,93 94,100 110,107 126,114 110,121 94,128 78,135 94,142 110,149 126,156 142,163 158,170 142,177 126,184 110,191 126,198 126,205 110,212 94,219 78,226 62,233 46,240 30,247 14,254 30,261 46,268 62,275 46,282 30,289 14,296'></polyline>
<polyline class='blueline hunt' points='342,16 326,23 310,30 326,37 342,44 358,51 374,58 390,65 374,72 390,79 374,86 390,93 374,100 390,107 390,114 374,121 358,128 342,135 358,142 358,149 342,156 326,163 310,170 326,177 342,184 358,191 374,198 390,205 374,212 390,219 374,226 390,233 374,240 390,247 390,254 374,261 358,268 342,275 358,282 358,289 342,296'></polyline>
<polyline class='blueline hunt' points='358,16 358,23 342,30 358,37 374,44 390,51 390,58 374,65 390,72 374,79 390,86 374,93 390,100 374,107 358,114 342,121 326,128 310,135 326,142 342,149 358,156 358,163 342,170 358,177 374,184 390,191 390,198 374,205 390,212 374,219 390,226 374,233 390,240 374,247 358,254 342,261 326,268 310,275 326,282 342,289 358,296'></polyline>
<polyline class='blueline' points='230,16 246,23 262,30 246,37 230,44 214,51 230,58 246,65 262,72 278,79 294,86 310,93 326,100 326,107 310,114 326,121 342,128 358,135 342,142 326,149 310,156 294,163 278,170 294,177 310,184 326,191 310,198 294,205 278,212 262,219 246,226 230,233 214,240 214,247 230,254 214,261 214,268 230,275 214,282 214,289 230,296'></polyline>
<polyline class='blueline hunt' points='542,16 526,23 510,30 526,37 542,44 558,51 574,58 590,65 574,72 590,79 574,86 590,93 574,100 590,107 590,114 574,121 558,128 542,135 558,142 558,149 542,156 526,163 510,170 526,177 542,184 558,191 574,198 590,205 574,212 590,219 574,226 590,233 574,240 590,247 590,254 574,261 558,268 542,275 558,282 558,289 542,296'></polyline>
<polyline class='blueline hunt' points='558,16 558,23 542,30 558,37 574,44 590,51 590,58 574,65 590,72 574,79 590,86 574,93 590,100 574,107 558,114 542,121 526,128 510,135 526,142 542,149 558,156 558,163 542,170 558,177 574,184 590,191 590,198 574,205 590,212 574,219 590,226 574,233 590,240 574,247 558,254 542,261 526,268 510,275 526,282 542,289 558,296'></polyline>
<polyline class='blueline' points='446,16 430,23 414,30 430,37 446,44 462,51 446,58 430,65 414,72 414,79 430,86 446,93 462,100 478,107 494,114 478,121 462,128 446,135 462,142 478,149 494,156 510,163 526,170 510,177 494,184 478,191 494,198 510,205 526,212 526,219 510,226 494,233 478,240 462,247 446,254 462,261 478,268 494,275 478,282 462,289 446,296'></polyline>
<polyline class='blueline hunt' points='742,16 726,23 710,30 726,37 742,44 758,51 774,58 790,65 774,72 790,79 774,86 790,93 774,100 790,107 790,114 774,121 758,128 742,135 758,142 758,149 742,156 726,163 710,170 726,177 742,184 758,191 774,198 790,205 774,212 790,219 774,226 790,233 774,240 790,247 790,254 774,261 758,268 742,275 758,282 758,289 742,296'></polyline>
<polyline class='blueline hunt' points='758,16 758,23 742,30 758,37 774,44 790,51 790,58 774,65 790,72 774,79 790,86 774,93 790,100 774,107 758,114 742,121 726,128 710,135 726,142 742,149 758,156 758,163 742,170 758,177 774,184 790,191 790,198 774,205 790,212 774,219 790,226 774,233 790,240 774,247 758,254 742,261 726,268 710,275 726,282 742,289 758,296'></polyline>
<polyline class='blueline' points='662,16 678,23 694,30 678,37 662,44 646,51 662,58 678,65 694,72 710,79 726,86 726,93 710,100 694,107 678,114 694,121 710,128 726,135 710,142 694,149 678,156 662,163 646,170 662,177 678,184 694,191 678,198 662,205 646,212 630,219 614,226 614,233 630,240 646,247 662,254 646,261 630,268 614,275 630,282 646,289 662,296'></polyline>
<polyline class='blueline hunt' points='942,16 926,23 910,30 926,37 942,44 958,51 974,58 990,65 974,72 990,79 974,86 990,93 974,100 990,107 990,114 974,121 958,128 942,135 958,142 958,149 942,156 926,163 910,170 926,177 942,184 958,191 974,198 990,205 974,212 990,219 974,226 990,233 974,240 990,247 990,254 974,261 958,268 942,275 958,282 958,289 942,296'></polyline>
<polyline class='blueline hunt' points='958,16 958,23 942,30 958,37 974,44 990,51 990,58 974,65 990,72 974,79 990,86 974,93 990,100 974,107 958,114 942,121 926,128 910,135 926,142 942,149 958,156 958,163 942,170 958,177 974,184 990,191 990,198 974,205 990,212 974,219 990,226 974,233 990,240 974,247 958,254 942,261 926,268 910,275 926,282 942,289 958,296'></polyline>
<polyline class='blueline' points='974,16 990,23 990,30 974,37 958,44 942,51 942,58 958,65 942,72 958,79 958,86 942,93 958,100 942,107 942,114 958,121 974,128 990,135 990,142 974,149 990,156 974,163 974,170 990,177 990,184 974,191 958,198 942,205 958,212 942,219 942,226 958,233 942,240 958,247 974,254 990,261 990,268 974,275 974,282 990,289 974,296'></polyline>
</svg>
")

(define-blueline-test "Huntly Castle Surprise Minor" (:hunt-bell :working :figures t :place-notation :lead)
  "<svg xmlns='http://www.w3.org/2000/svg' preserveaspectratio='xMidYMid meet' height='704' width='625'>
<style>.blueline{fill:none;stroke:hsl(240,100%,60%);stroke-width:1.4;stroke-linecap:round;stroke-linejoin:miter;}
.hunt{stroke:hsl(0,60%,60%);stroke-width:0.8}
.dot{fill:black;stroke:none;}
.figure{font-family:sans-serif;font-size:14px;}
.notation{font-family:sans-serif;font-size:11px;font-weight:lighter;font-style:italic;fill:dimgray}
.label{font-family:sans-serif;font-size:15px;font-weight:bolder;}</style>
<text class='notation' x='26' y='26' text-anchor='end'>x</text>
<text class='notation' x='26' y='40' text-anchor='end'>34</text>
<text class='notation' x='26' y='54' text-anchor='end'>x</text>
<text class='notation' x='26' y='68' text-anchor='end'>4</text>
<text class='notation' x='26' y='82' text-anchor='end'>x</text>
<text class='notation' x='26' y='96' text-anchor='end'>25</text>
<text class='notation' x='26' y='110' text-anchor='end'>x</text>
<text class='notation' x='26' y='124' text-anchor='end'>3</text>
<text class='notation' x='26' y='138' text-anchor='end'>4</text>
<text class='notation' x='26' y='152' text-anchor='end'>x</text>
<text class='notation' x='26' y='166' text-anchor='end'>34</text>
<text class='notation' x='26' y='180' text-anchor='end'>5</text>
<text class='notation' x='26' y='194' text-anchor='end'>34</text>
<text class='notation' x='26' y='208' text-anchor='end'>x</text>
<text class='notation' x='26' y='222' text-anchor='end'>4</text>
<text class='notation' x='26' y='236' text-anchor='end'>3</text>
<text class='notation' x='26' y='250' text-anchor='end'>x</text>
<text class='notation' x='26' y='264' text-anchor='end'>25</text>
<text class='notation' x='26' y='278' text-anchor='end'>x</text>
<text class='notation' x='26' y='292' text-anchor='end'>4</text>
<text class='notation' x='26' y='306' text-anchor='end'>x</text>
<text class='notation' x='26' y='320' text-anchor='end'>34</text>
<text class='notation' x='26' y='334' text-anchor='end'>x</text>
<text class='notation' x='26' y='348' text-anchor='end'>6</text>
<text class='figure' x='40' y='20'>1</text><text class='figure' x='56' y='20'>2</text><text class='figure' x='72' y='20'>3</text><text class='figure' x='88' y='20'>4</text><text class='figure' x='104' y='20'>5</text><text class='figure' x='120' y='20'>6</text>
<text class='figure' x='40' y='34'>2</text><text class='figure' x='56' y='34'>1</text><text class='figure' x='72' y='34'>4</text><text class='figure' x='88' y='34'>3</text><text class='figure' x='120' y='34'>5</text>
<text class='figure' x='40' y='48'>1</text><text class='figure' x='56' y='48'>2</text><text class='figure' x='72' y='48'>4</text><text class='figure' x='88' y='48'>3</text><text class='figure' x='104' y='48'>5</text>
<text class='figure' x='40' y='62'>2</text><text class='figure' x='56' y='62'>1</text><text class='figure' x='72' y='62'>3</text><text class='figure' x='88' y='62'>4</text><text class='figure' x='120' y='62'>5</text>
<text class='figure' x='40' y='76'>2</text><text class='figure' x='56' y='76'>3</text><text class='figure' x='72' y='76'>1</text><text class='figure' x='88' y='76'>4</text><text class='figure' x='104' y='76'>5</text>
<text class='figure' x='40' y='90'>3</text><text class='figure' x='56' y='90'>2</text><text class='figure' x='72' y='90'>4</text><text class='figure' x='88' y='90'>1</text><text class='figure' x='120' y='90'>5</text>
<text class='figure' x='40' y='104'>3</text><text class='figure' x='56' y='104'>2</text><text class='figure' x='72' y='104'>1</text><text class='figure' x='88' y='104'>4</text><text class='figure' x='120' y='104'>5</text>
<text class='figure' x='40' y='118'>2</text><text class='figure' x='56' y='118'>3</text><text class='figure' x='72' y='118'>4</text><text class='figure' x='88' y='118'>1</text><text class='figure' x='104' y='118'>5</text>
<text class='figure' x='40' y='132'>3</text><text class='figure' x='56' y='132'>2</text><text class='figure' x='72' y='132'>4</text><text class='figure' x='88' y='132'>5</text><text class='figure' x='104' y='132'>1</text>
<text class='figure' x='40' y='146'>3</text><text class='figure' x='56' y='146'>4</text><text class='figure' x='72' y='146'>2</text><text class='figure' x='88' y='146'>5</text><text class='figure' x='120' y='146'>1</text>
<text class='figure' x='40' y='160'>4</text><text class='figure' x='56' y='160'>3</text><text class='figure' x='72' y='160'>5</text><text class='figure' x='88' y='160'>2</text><text class='figure' x='104' y='160'>1</text>
<text class='figure' x='40' y='174'>3</text><text class='figure' x='56' y='174'>4</text><text class='figure' x='72' y='174'>5</text><text class='figure' x='88' y='174'>2</text><text class='figure' x='120' y='174'>1</text>
<text class='figure' x='40' y='188'>4</text><text class='figure' x='56' y='188'>3</text><text class='figure' x='72' y='188'>2</text><text class='figure' x='88' y='188'>5</text><text class='figure' x='120' y='188'>1</text>
<text class='figure' x='40' y='202'>3</text><text class='figure' x='56' y='202'>4</text><text class='figure' x='72' y='202'>2</text><text class='figure' x='88' y='202'>5</text><text class='figure' x='104' y='202'>1</text>
<text class='figure' x='40' y='216'>4</text><text class='figure' x='56' y='216'>3</text><text class='figure' x='72' y='216'>5</text><text class='figure' x='88' y='216'>2</text><text class='figure' x='120' y='216'>1</text>
<text class='figure' x='40' y='230'>4</text><text class='figure' x='56' y='230'>5</text><text class='figure' x='72' y='230'>3</text><text class='figure' x='88' y='230'>2</text><text class='figure' x='104' y='230'>1</text>
<text class='figure' x='40' y='244'>5</text><text class='figure' x='56' y='244'>4</text><text class='figure' x='72' y='244'>3</text><text class='figure' x='88' y='244'>1</text><text class='figure' x='104' y='244'>2</text>
<text class='figure' x='40' y='258'>4</text><text class='figure' x='56' y='258'>5</text><text class='figure' x='72' y='258'>1</text><text class='figure' x='88' y='258'>3</text><text class='figure' x='120' y='258'>2</text>
<text class='figure' x='40' y='272'>4</text><text class='figure' x='56' y='272'>5</text><text class='figure' x='72' y='272'>3</text><text class='figure' x='88' y='272'>1</text><text class='figure' x='120' y='272'>2</text>
<text class='figure' x='40' y='286'>5</text><text class='figure' x='56' y='286'>4</text><text class='figure' x='72' y='286'>1</text><text class='figure' x='88' y='286'>3</text><text class='figure' x='104' y='286'>2</text>
<text class='figure' x='40' y='300'>5</text><text class='figure' x='56' y='300'>1</text><text class='figure' x='72' y='300'>4</text><text class='figure' x='88' y='300'>3</text><text class='figure' x='120' y='300'>2</text>
<text class='figure' x='40' y='314'>1</text><text class='figure' x='56' y='314'>5</text><text class='figure' x='72' y='314'>3</text><text class='figure' x='88' y='314'>4</text><text class='figure' x='104' y='314'>2</text>
<text class='figure' x='40' y='328'>5</text><text class='figure' x='56' y='328'>1</text><text class='figure' x='72' y='328'>3</text><text class='figure' x='88' y='328'>4</text><text class='figure' x='120' y='328'>2</text>
<text class='figure' x='40' y='342'>1</text><text class='figure' x='56' y='342'>5</text><text class='figure' x='72' y='342'>4</text><text class='figure' x='88' y='342'>3</text><text class='figure' x='104' y='342'>2</text>
<text class='figure' x='40' y='356'>1</text><text class='figure' x='56' y='356'>4</text><text class='figure' x='72' y='356'>5</text><text class='figure' x='88' y='356'>2</text><text class='figure' x='104' y='356'>3</text>
<polyline class='blueline' points='124,16 108,30 124,44 108,58 124,72 108,86 108,100 124,114 124,128 108,142 124,156 108,170 108,184 124,198 108,212 124,226 124,240 108,254 108,268 124,282 108,296 124,310 108,324 124,338 124,352'></polyline>
<text class='label' x='139' y='21'>6</text><circle cx='144' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<text class='label' x='139' y='357'>6</text><circle cx='144' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='124' cy='352' r='2.8' class='dot'></circle>
<text class='figure' x='175' y='20'>1</text><text class='figure' x='191' y='20'>4</text><text class='figure' x='207' y='20'>5</text><text class='figure' x='223' y='20'>2</text><text class='figure' x='239' y='20'>3</text><text class='figure' x='255' y='20'>6</text>
<text class='figure' x='175' y='34'>4</text><text class='figure' x='191' y='34'>1</text><text class='figure' x='207' y='34'>2</text><text class='figure' x='239' y='34'>6</text><text class='figure' x='255' y='34'>3</text>
<text class='figure' x='175' y='48'>1</text><text class='figure' x='191' y='48'>4</text><text class='figure' x='207' y='48'>2</text><text class='figure' x='239' y='48'>3</text><text class='figure' x='255' y='48'>6</text>
<text class='figure' x='175' y='62'>4</text><text class='figure' x='191' y='62'>1</text><text class='figure' x='223' y='62'>2</text><text class='figure' x='239' y='62'>6</text><text class='figure' x='255' y='62'>3</text>
<text class='figure' x='175' y='76'>4</text><text class='figure' x='207' y='76'>1</text><text class='figure' x='223' y='76'>2</text><text class='figure' x='239' y='76'>3</text><text class='figure' x='255' y='76'>6</text>
<text class='figure' x='191' y='90'>4</text><text class='figure' x='207' y='90'>2</text><text class='figure' x='223' y='90'>1</text><text class='figure' x='239' y='90'>6</text><text class='figure' x='255' y='90'>3</text>
<text class='figure' x='191' y='104'>4</text><text class='figure' x='207' y='104'>1</text><text class='figure' x='223' y='104'>2</text><text class='figure' x='239' y='104'>6</text><text class='figure' x='255' y='104'>3</text>
<text class='figure' x='175' y='118'>4</text><text class='figure' x='207' y='118'>2</text><text class='figure' x='223' y='118'>1</text><text class='figure' x='239' y='118'>3</text><text class='figure' x='255' y='118'>6</text>
<text class='figure' x='191' y='132'>4</text><text class='figure' x='207' y='132'>2</text><text class='figure' x='223' y='132'>3</text><text class='figure' x='239' y='132'>1</text><text class='figure' x='255' y='132'>6</text>
<text class='figure' x='191' y='146'>2</text><text class='figure' x='207' y='146'>4</text><text class='figure' x='223' y='146'>3</text><text class='figure' x='239' y='146'>6</text><text class='figure' x='255' y='146'>1</text>
<text class='figure' x='175' y='160'>2</text><text class='figure' x='207' y='160'>3</text><text class='figure' x='223' y='160'>4</text><text class='figure' x='239' y='160'>1</text><text class='figure' x='255' y='160'>6</text>
<text class='figure' x='191' y='174'>2</text><text class='figure' x='207' y='174'>3</text><text class='figure' x='223' y='174'>4</text><text class='figure' x='239' y='174'>6</text><text class='figure' x='255' y='174'>1</text>
<text class='figure' x='175' y='188'>2</text><text class='figure' x='207' y='188'>4</text><text class='figure' x='223' y='188'>3</text><text class='figure' x='239' y='188'>6</text><text class='figure' x='255' y='188'>1</text>
<text class='figure' x='191' y='202'>2</text><text class='figure' x='207' y='202'>4</text><text class='figure' x='223' y='202'>3</text><text class='figure' x='239' y='202'>1</text><text class='figure' x='255' y='202'>6</text>
<text class='figure' x='175' y='216'>2</text><text class='figure' x='207' y='216'>3</text><text class='figure' x='223' y='216'>4</text><text class='figure' x='239' y='216'>6</text><text class='figure' x='255' y='216'>1</text>
<text class='figure' x='175' y='230'>2</text><text class='figure' x='191' y='230'>3</text><text class='figure' x='223' y='230'>4</text><text class='figure' x='239' y='230'>1</text><text class='figure' x='255' y='230'>6</text>
<text class='figure' x='175' y='244'>3</text><text class='figure' x='191' y='244'>2</text><text class='figure' x='223' y='244'>1</text><text class='figure' x='239' y='244'>4</text><text class='figure' x='255' y='244'>6</text>
<text class='figure' x='175' y='258'>2</text><text class='figure' x='191' y='258'>3</text><text class='figure' x='207' y='258'>1</text><text class='figure' x='239' y='258'>6</text><text class='figure' x='255' y='258'>4</text>
<text class='figure' x='175' y='272'>2</text><text class='figure' x='191' y='272'>3</text><text class='figure' x='223' y='272'>1</text><text class='figure' x='239' y='272'>6</text><text class='figure' x='255' y='272'>4</text>
<text class='figure' x='175' y='286'>3</text><text class='figure' x='191' y='286'>2</text><text class='figure' x='207' y='286'>1</text><text class='figure' x='239' y='286'>4</text><text class='figure' x='255' y='286'>6</text>
<text class='figure' x='175' y='300'>3</text><text class='figure' x='191' y='300'>1</text><text class='figure' x='207' y='300'>2</text><text class='figure' x='239' y='300'>6</text><text class='figure' x='255' y='300'>4</text>
<text class='figure' x='175' y='314'>1</text><text class='figure' x='191' y='314'>3</text><text class='figure' x='223' y='314'>2</text><text class='figure' x='239' y='314'>4</text><text class='figure' x='255' y='314'>6</text>
<text class='figure' x='175' y='328'>3</text><text class='figure' x='191' y='328'>1</text><text class='figure' x='223' y='328'>2</text><text class='figure' x='239' y='328'>6</text><text class='figure' x='255' y='328'>4</text>
<text class='figure' x='175' y='342'>1</text><text class='figure' x='191' y='342'>3</text><text class='figure' x='207' y='342'>2</text><text class='figure' x='239' y='342'>4</text><text class='figure' x='255' y='342'>6</text>
<text class='figure' x='175' y='356'>1</text><text class='figure' x='191' y='356'>2</text><text class='figure' x='207' y='356'>3</text><text class='figure' x='223' y='356'>4</text><text class='figure' x='255' y='356'>6</text>
<text class='figure' x='175' y='370'>2</text><text class='figure' x='191' y='370'>1</text><text class='figure' x='207' y='370'>4</text><text class='figure' x='223' y='370'>3</text><text class='figure' x='239' y='370'>6</text>
<text class='figure' x='175' y='384'>1</text><text class='figure' x='191' y='384'>2</text><text class='figure' x='207' y='384'>4</text><text class='figure' x='223' y='384'>3</text><text class='figure' x='255' y='384'>6</text>
<text class='figure' x='175' y='398'>2</text><text class='figure' x='191' y='398'>1</text><text class='figure' x='207' y='398'>3</text><text class='figure' x='223' y='398'>4</text><text class='figure' x='239' y='398'>6</text>
<text class='figure' x='175' y='412'>2</text><text class='figure' x='191' y='412'>3</text><text class='figure' x='207' y='412'>1</text><text class='figure' x='223' y='412'>4</text><text class='figure' x='255' y='412'>6</text>
<text class='figure' x='175' y='426'>3</text><text class='figure' x='191' y='426'>2</text><text class='figure' x='207' y='426'>4</text><text class='figure' x='223' y='426'>1</text><text class='figure' x='239' y='426'>6</text>
<text class='figure' x='175' y='440'>3</text><text class='figure' x='191' y='440'>2</text><text class='figure' x='207' y='440'>1</text><text class='figure' x='223' y='440'>4</text><text class='figure' x='239' y='440'>6</text>
<text class='figure' x='175' y='454'>2</text><text class='figure' x='191' y='454'>3</text><text class='figure' x='207' y='454'>4</text><text class='figure' x='223' y='454'>1</text><text class='figure' x='255' y='454'>6</text>
<text class='figure' x='175' y='468'>3</text><text class='figure' x='191' y='468'>2</text><text class='figure' x='207' y='468'>4</text><text class='figure' x='239' y='468'>1</text><text class='figure' x='255' y='468'>6</text>
<text class='figure' x='175' y='482'>3</text><text class='figure' x='191' y='482'>4</text><text class='figure' x='207' y='482'>2</text><text class='figure' x='239' y='482'>6</text><text class='figure' x='255' y='482'>1</text>
<text class='figure' x='175' y='496'>4</text><text class='figure' x='191' y='496'>3</text><text class='figure' x='223' y='496'>2</text><text class='figure' x='239' y='496'>1</text><text class='figure' x='255' y='496'>6</text>
<text class='figure' x='175' y='510'>3</text><text class='figure' x='191' y='510'>4</text><text class='figure' x='223' y='510'>2</text><text class='figure' x='239' y='510'>6</text><text class='figure' x='255' y='510'>1</text>
<text class='figure' x='175' y='524'>4</text><text class='figure' x='191' y='524'>3</text><text class='figure' x='207' y='524'>2</text><text class='figure' x='239' y='524'>6</text><text class='figure' x='255' y='524'>1</text>
<text class='figure' x='175' y='538'>3</text><text class='figure' x='191' y='538'>4</text><text class='figure' x='207' y='538'>2</text><text class='figure' x='239' y='538'>1</text><text class='figure' x='255' y='538'>6</text>
<text class='figure' x='175' y='552'>4</text><text class='figure' x='191' y='552'>3</text><text class='figure' x='223' y='552'>2</text><text class='figure' x='239' y='552'>6</text><text class='figure' x='255' y='552'>1</text>
<text class='figure' x='175' y='566'>4</text><text class='figure' x='207' y='566'>3</text><text class='figure' x='223' y='566'>2</text><text class='figure' x='239' y='566'>1</text><text class='figure' x='255' y='566'>6</text>
<text class='figure' x='191' y='580'>4</text><text class='figure' x='207' y='580'>3</text><text class='figure' x='223' y='580'>1</text><text class='figure' x='239' y='580'>2</text><text class='figure' x='255' y='580'>6</text>
<text class='figure' x='175' y='594'>4</text><text class='figure' x='207' y='594'>1</text><text class='figure' x='223' y='594'>3</text><text class='figure' x='239' y='594'>6</text><text class='figure' x='255' y='594'>2</text>
<text class='figure' x='175' y='608'>4</text><text class='figure' x='207' y='608'>3</text><text class='figure' x='223' y='608'>1</text><text class='figure' x='239' y='608'>6</text><text class='figure' x='255' y='608'>2</text>
<text class='figure' x='191' y='622'>4</text><text class='figure' x='207' y='622'>1</text><text class='figure' x='223' y='622'>3</text><text class='figure' x='239' y='622'>2</text><text class='figure' x='255' y='622'>6</text>
<text class='figure' x='191' y='636'>1</text><text class='figure' x='207' y='636'>4</text><text class='figure' x='223' y='636'>3</text><text class='figure' x='239' y='636'>6</text><text class='figure' x='255' y='636'>2</text>
<text class='figure' x='175' y='650'>1</text><text class='figure' x='207' y='650'>3</text><text class='figure' x='223' y='650'>4</text><text class='figure' x='239' y='650'>2</text><text class='figure' x='255' y='650'>6</text>
<text class='figure' x='191' y='664'>1</text><text class='figure' x='207' y='664'>3</text><text class='figure' x='223' y='664'>4</text><text class='figure' x='239' y='664'>6</text><text class='figure' x='255' y='664'>2</text>
<text class='figure' x='175' y='678'>1</text><text class='figure' x='207' y='678'>4</text><text class='figure' x='223' y='678'>3</text><text class='figure' x='239' y='678'>2</text><text class='figure' x='255' y='678'>6</text>
<text class='figure' x='175' y='692'>1</text><text class='figure' x='191' y='692'>4</text><text class='figure' x='223' y='692'>2</text><text class='figure' x='239' y='692'>3</text><text class='figure' x='255' y='692'>6</text>
<polyline class='blueline' points='243,16 259,30 243,44 259,58 243,72 259,86 259,100 243,114 227,128 227,142 211,156 211,170 227,184 227,198 211,212 195,226 179,240 195,254 195,268 179,282 179,296 195,310 179,324 195,338 211,352 227,366 227,380 211,394 195,408 179,422 179,436 195,450 179,464 179,478 195,492 179,506 195,520 179,534 195,548 211,562 211,576 227,590 211,604 227,618 227,632 211,646 211,660 227,674 243,688'></polyline>
<text class='label' x='283' y='21'>5</text><circle cx='288' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<text class='label' x='283' y='357'>3</text><circle cx='288' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='211' cy='352' r='2.8' class='dot'></circle>
<text class='label' x='283' y='693'>5</text><circle cx='288' cy='688' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='243' cy='688' r='2.8' class='dot'></circle>
<text class='figure' x='319' y='20'>1</text><text class='figure' x='335' y='20'>4</text><text class='figure' x='351' y='20'>5</text><text class='figure' x='367' y='20'>2</text><text class='figure' x='383' y='20'>3</text><text class='figure' x='399' y='20'>6</text>
<text class='figure' x='335' y='34'>1</text><text class='figure' x='351' y='34'>2</text><text class='figure' x='367' y='34'>5</text><text class='figure' x='383' y='34'>6</text><text class='figure' x='399' y='34'>3</text>
<text class='figure' x='319' y='48'>1</text><text class='figure' x='351' y='48'>2</text><text class='figure' x='367' y='48'>5</text><text class='figure' x='383' y='48'>3</text><text class='figure' x='399' y='48'>6</text>
<text class='figure' x='335' y='62'>1</text><text class='figure' x='351' y='62'>5</text><text class='figure' x='367' y='62'>2</text><text class='figure' x='383' y='62'>6</text><text class='figure' x='399' y='62'>3</text>
<text class='figure' x='335' y='76'>5</text><text class='figure' x='351' y='76'>1</text><text class='figure' x='367' y='76'>2</text><text class='figure' x='383' y='76'>3</text><text class='figure' x='399' y='76'>6</text>
<text class='figure' x='319' y='90'>5</text><text class='figure' x='351' y='90'>2</text><text class='figure' x='367' y='90'>1</text><text class='figure' x='383' y='90'>6</text><text class='figure' x='399' y='90'>3</text>
<text class='figure' x='319' y='104'>5</text><text class='figure' x='351' y='104'>1</text><text class='figure' x='367' y='104'>2</text><text class='figure' x='383' y='104'>6</text><text class='figure' x='399' y='104'>3</text>
<text class='figure' x='335' y='118'>5</text><text class='figure' x='351' y='118'>2</text><text class='figure' x='367' y='118'>1</text><text class='figure' x='383' y='118'>3</text><text class='figure' x='399' y='118'>6</text>
<text class='figure' x='319' y='132'>5</text><text class='figure' x='351' y='132'>2</text><text class='figure' x='367' y='132'>3</text><text class='figure' x='383' y='132'>1</text><text class='figure' x='399' y='132'>6</text>
<text class='figure' x='319' y='146'>5</text><text class='figure' x='335' y='146'>2</text><text class='figure' x='367' y='146'>3</text><text class='figure' x='383' y='146'>6</text><text class='figure' x='399' y='146'>1</text>
<text class='figure' x='319' y='160'>2</text><text class='figure' x='335' y='160'>5</text><text class='figure' x='351' y='160'>3</text><text class='figure' x='383' y='160'>1</text><text class='figure' x='399' y='160'>6</text>
<text class='figure' x='319' y='174'>5</text><text class='figure' x='335' y='174'>2</text><text class='figure' x='351' y='174'>3</text><text class='figure' x='383' y='174'>6</text><text class='figure' x='399' y='174'>1</text>
<text class='figure' x='319' y='188'>2</text><text class='figure' x='335' y='188'>5</text><text class='figure' x='367' y='188'>3</text><text class='figure' x='383' y='188'>6</text><text class='figure' x='399' y='188'>1</text>
<text class='figure' x='319' y='202'>5</text><text class='figure' x='335' y='202'>2</text><text class='figure' x='367' y='202'>3</text><text class='figure' x='383' y='202'>1</text><text class='figure' x='399' y='202'>6</text>
<text class='figure' x='319' y='216'>2</text><text class='figure' x='335' y='216'>5</text><text class='figure' x='351' y='216'>3</text><text class='figure' x='383' y='216'>6</text><text class='figure' x='399' y='216'>1</text>
<text class='figure' x='319' y='230'>2</text><text class='figure' x='335' y='230'>3</text><text class='figure' x='351' y='230'>5</text><text class='figure' x='383' y='230'>1</text><text class='figure' x='399' y='230'>6</text>
<text class='figure' x='319' y='244'>3</text><text class='figure' x='335' y='244'>2</text><text class='figure' x='351' y='244'>5</text><text class='figure' x='367' y='244'>1</text><text class='figure' x='399' y='244'>6</text>
<text class='figure' x='319' y='258'>2</text><text class='figure' x='335' y='258'>3</text><text class='figure' x='351' y='258'>1</text><text class='figure' x='367' y='258'>5</text><text class='figure' x='383' y='258'>6</text>
<text class='figure' x='319' y='272'>2</text><text class='figure' x='335' y='272'>3</text><text class='figure' x='351' y='272'>5</text><text class='figure' x='367' y='272'>1</text><text class='figure' x='383' y='272'>6</text>
<text class='figure' x='319' y='286'>3</text><text class='figure' x='335' y='286'>2</text><text class='figure' x='351' y='286'>1</text><text class='figure' x='367' y='286'>5</text><text class='figure' x='399' y='286'>6</text>
<text class='figure' x='319' y='300'>3</text><text class='figure' x='335' y='300'>1</text><text class='figure' x='351' y='300'>2</text><text class='figure' x='367' y='300'>5</text><text class='figure' x='383' y='300'>6</text>
<text class='figure' x='319' y='314'>1</text><text class='figure' x='335' y='314'>3</text><text class='figure' x='351' y='314'>5</text><text class='figure' x='367' y='314'>2</text><text class='figure' x='399' y='314'>6</text>
<text class='figure' x='319' y='328'>3</text><text class='figure' x='335' y='328'>1</text><text class='figure' x='351' y='328'>5</text><text class='figure' x='367' y='328'>2</text><text class='figure' x='383' y='328'>6</text>
<text class='figure' x='319' y='342'>1</text><text class='figure' x='335' y='342'>3</text><text class='figure' x='351' y='342'>2</text><text class='figure' x='367' y='342'>5</text><text class='figure' x='399' y='342'>6</text>
<text class='figure' x='319' y='356'>1</text><text class='figure' x='335' y='356'>2</text><text class='figure' x='351' y='356'>3</text><text class='figure' x='383' y='356'>5</text><text class='figure' x='399' y='356'>6</text>
<text class='figure' x='319' y='370'>2</text><text class='figure' x='335' y='370'>1</text><text class='figure' x='367' y='370'>3</text><text class='figure' x='383' y='370'>6</text><text class='figure' x='399' y='370'>5</text>
<text class='figure' x='319' y='384'>1</text><text class='figure' x='335' y='384'>2</text><text class='figure' x='367' y='384'>3</text><text class='figure' x='383' y='384'>5</text><text class='figure' x='399' y='384'>6</text>
<text class='figure' x='319' y='398'>2</text><text class='figure' x='335' y='398'>1</text><text class='figure' x='351' y='398'>3</text><text class='figure' x='383' y='398'>6</text><text class='figure' x='399' y='398'>5</text>
<text class='figure' x='319' y='412'>2</text><text class='figure' x='335' y='412'>3</text><text class='figure' x='351' y='412'>1</text><text class='figure' x='383' y='412'>5</text><text class='figure' x='399' y='412'>6</text>
<text class='figure' x='319' y='426'>3</text><text class='figure' x='335' y='426'>2</text><text class='figure' x='367' y='426'>1</text><text class='figure' x='383' y='426'>6</text><text class='figure' x='399' y='426'>5</text>
<text class='figure' x='319' y='440'>3</text><text class='figure' x='335' y='440'>2</text><text class='figure' x='351' y='440'>1</text><text class='figure' x='383' y='440'>6</text><text class='figure' x='399' y='440'>5</text>
<text class='figure' x='319' y='454'>2</text><text class='figure' x='335' y='454'>3</text><text class='figure' x='367' y='454'>1</text><text class='figure' x='383' y='454'>5</text><text class='figure' x='399' y='454'>6</text>
<text class='figure' x='319' y='468'>3</text><text class='figure' x='335' y='468'>2</text><text class='figure' x='367' y='468'>5</text><text class='figure' x='383' y='468'>1</text><text class='figure' x='399' y='468'>6</text>
<text class='figure' x='319' y='482'>3</text><text class='figure' x='351' y='482'>2</text><text class='figure' x='367' y='482'>5</text><text class='figure' x='383' y='482'>6</text><text class='figure' x='399' y='482'>1</text>
<text class='figure' x='335' y='496'>3</text><text class='figure' x='351' y='496'>5</text><text class='figure' x='367' y='496'>2</text><text class='figure' x='383' y='496'>1</text><text class='figure' x='399' y='496'>6</text>
<text class='figure' x='319' y='510'>3</text><text class='figure' x='351' y='510'>5</text><text class='figure' x='367' y='510'>2</text><text class='figure' x='383' y='510'>6</text><text class='figure' x='399' y='510'>1</text>
<text class='figure' x='335' y='524'>3</text><text class='figure' x='351' y='524'>2</text><text class='figure' x='367' y='524'>5</text><text class='figure' x='383' y='524'>6</text><text class='figure' x='399' y='524'>1</text>
<text class='figure' x='319' y='538'>3</text><text class='figure' x='351' y='538'>2</text><text class='figure' x='367' y='538'>5</text><text class='figure' x='383' y='538'>1</text><text class='figure' x='399' y='538'>6</text>
<text class='figure' x='335' y='552'>3</text><text class='figure' x='351' y='552'>5</text><text class='figure' x='367' y='552'>2</text><text class='figure' x='383' y='552'>6</text><text class='figure' x='399' y='552'>1</text>
<text class='figure' x='335' y='566'>5</text><text class='figure' x='351' y='566'>3</text><text class='figure' x='367' y='566'>2</text><text class='figure' x='383' y='566'>1</text><text class='figure' x='399' y='566'>6</text>
<text class='figure' x='319' y='580'>5</text><text class='figure' x='351' y='580'>3</text><text class='figure' x='367' y='580'>1</text><text class='figure' x='383' y='580'>2</text><text class='figure' x='399' y='580'>6</text>
<text class='figure' x='335' y='594'>5</text><text class='figure' x='351' y='594'>1</text><text class='figure' x='367' y='594'>3</text><text class='figure' x='383' y='594'>6</text><text class='figure' x='399' y='594'>2</text>
<text class='figure' x='335' y='608'>5</text><text class='figure' x='351' y='608'>3</text><text class='figure' x='367' y='608'>1</text><text class='figure' x='383' y='608'>6</text><text class='figure' x='399' y='608'>2</text>
<text class='figure' x='319' y='622'>5</text><text class='figure' x='351' y='622'>1</text><text class='figure' x='367' y='622'>3</text><text class='figure' x='383' y='622'>2</text><text class='figure' x='399' y='622'>6</text>
<text class='figure' x='319' y='636'>5</text><text class='figure' x='335' y='636'>1</text><text class='figure' x='367' y='636'>3</text><text class='figure' x='383' y='636'>6</text><text class='figure' x='399' y='636'>2</text>
<text class='figure' x='319' y='650'>1</text><text class='figure' x='335' y='650'>5</text><text class='figure' x='351' y='650'>3</text><text class='figure' x='383' y='650'>2</text><text class='figure' x='399' y='650'>6</text>
<text class='figure' x='319' y='664'>5</text><text class='figure' x='335' y='664'>1</text><text class='figure' x='351' y='664'>3</text><text class='figure' x='383' y='664'>6</text><text class='figure' x='399' y='664'>2</text>
<text class='figure' x='319' y='678'>1</text><text class='figure' x='335' y='678'>5</text><text class='figure' x='367' y='678'>3</text><text class='figure' x='383' y='678'>2</text><text class='figure' x='399' y='678'>6</text>
<text class='figure' x='319' y='692'>1</text><text class='figure' x='351' y='692'>5</text><text class='figure' x='367' y='692'>2</text><text class='figure' x='383' y='692'>3</text><text class='figure' x='399' y='692'>6</text>
<polyline class='blueline' points='371,16 355,30 355,44 371,58 371,72 355,86 371,100 355,114 355,128 339,142 323,156 339,170 323,184 339,198 323,212 323,226 339,240 323,254 323,268 339,282 355,296 371,310 371,324 355,338 339,352 323,366 339,380 323,394 323,408 339,422 339,436 323,450 339,464 355,478 371,492 371,506 355,520 355,534 371,548 371,562 387,576 403,590 403,604 387,618 403,632 387,646 403,660 387,674 371,688'></polyline>
<text class='label' x='427' y='21'>4</text><circle cx='432' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<text class='label' x='427' y='357'>2</text><circle cx='432' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='339' cy='352' r='2.8' class='dot'></circle>
<text class='label' x='427' y='693'>4</text><circle cx='432' cy='688' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='371' cy='688' r='2.8' class='dot'></circle>
<text class='figure' x='463' y='20'>1</text><text class='figure' x='479' y='20'>4</text><text class='figure' x='495' y='20'>5</text><text class='figure' x='511' y='20'>2</text><text class='figure' x='527' y='20'>3</text><text class='figure' x='543' y='20'>6</text>
<text class='figure' x='463' y='34'>4</text><text class='figure' x='495' y='34'>2</text><text class='figure' x='511' y='34'>5</text><text class='figure' x='527' y='34'>6</text><text class='figure' x='543' y='34'>3</text>
<text class='figure' x='479' y='48'>4</text><text class='figure' x='495' y='48'>2</text><text class='figure' x='511' y='48'>5</text><text class='figure' x='527' y='48'>3</text><text class='figure' x='543' y='48'>6</text>
<text class='figure' x='463' y='62'>4</text><text class='figure' x='495' y='62'>5</text><text class='figure' x='511' y='62'>2</text><text class='figure' x='527' y='62'>6</text><text class='figure' x='543' y='62'>3</text>
<text class='figure' x='463' y='76'>4</text><text class='figure' x='479' y='76'>5</text><text class='figure' x='511' y='76'>2</text><text class='figure' x='527' y='76'>3</text><text class='figure' x='543' y='76'>6</text>
<text class='figure' x='463' y='90'>5</text><text class='figure' x='479' y='90'>4</text><text class='figure' x='495' y='90'>2</text><text class='figure' x='527' y='90'>6</text><text class='figure' x='543' y='90'>3</text>
<text class='figure' x='463' y='104'>5</text><text class='figure' x='479' y='104'>4</text><text class='figure' x='511' y='104'>2</text><text class='figure' x='527' y='104'>6</text><text class='figure' x='543' y='104'>3</text>
<text class='figure' x='463' y='118'>4</text><text class='figure' x='479' y='118'>5</text><text class='figure' x='495' y='118'>2</text><text class='figure' x='527' y='118'>3</text><text class='figure' x='543' y='118'>6</text>
<text class='figure' x='463' y='132'>5</text><text class='figure' x='479' y='132'>4</text><text class='figure' x='495' y='132'>2</text><text class='figure' x='511' y='132'>3</text><text class='figure' x='543' y='132'>6</text>
<text class='figure' x='463' y='146'>5</text><text class='figure' x='479' y='146'>2</text><text class='figure' x='495' y='146'>4</text><text class='figure' x='511' y='146'>3</text><text class='figure' x='527' y='146'>6</text>
<text class='figure' x='463' y='160'>2</text><text class='figure' x='479' y='160'>5</text><text class='figure' x='495' y='160'>3</text><text class='figure' x='511' y='160'>4</text><text class='figure' x='543' y='160'>6</text>
<text class='figure' x='463' y='174'>5</text><text class='figure' x='479' y='174'>2</text><text class='figure' x='495' y='174'>3</text><text class='figure' x='511' y='174'>4</text><text class='figure' x='527' y='174'>6</text>
<text class='figure' x='463' y='188'>2</text><text class='figure' x='479' y='188'>5</text><text class='figure' x='495' y='188'>4</text><text class='figure' x='511' y='188'>3</text><text class='figure' x='527' y='188'>6</text>
<text class='figure' x='463' y='202'>5</text><text class='figure' x='479' y='202'>2</text><text class='figure' x='495' y='202'>4</text><text class='figure' x='511' y='202'>3</text><text class='figure' x='543' y='202'>6</text>
<text class='figure' x='463' y='216'>2</text><text class='figure' x='479' y='216'>5</text><text class='figure' x='495' y='216'>3</text><text class='figure' x='511' y='216'>4</text><text class='figure' x='527' y='216'>6</text>
<text class='figure' x='463' y='230'>2</text><text class='figure' x='479' y='230'>3</text><text class='figure' x='495' y='230'>5</text><text class='figure' x='511' y='230'>4</text><text class='figure' x='543' y='230'>6</text>
<text class='figure' x='463' y='244'>3</text><text class='figure' x='479' y='244'>2</text><text class='figure' x='495' y='244'>5</text><text class='figure' x='527' y='244'>4</text><text class='figure' x='543' y='244'>6</text>
<text class='figure' x='463' y='258'>2</text><text class='figure' x='479' y='258'>3</text><text class='figure' x='511' y='258'>5</text><text class='figure' x='527' y='258'>6</text><text class='figure' x='543' y='258'>4</text>
<text class='figure' x='463' y='272'>2</text><text class='figure' x='479' y='272'>3</text><text class='figure' x='495' y='272'>5</text><text class='figure' x='527' y='272'>6</text><text class='figure' x='543' y='272'>4</text>
<text class='figure' x='463' y='286'>3</text><text class='figure' x='479' y='286'>2</text><text class='figure' x='511' y='286'>5</text><text class='figure' x='527' y='286'>4</text><text class='figure' x='543' y='286'>6</text>
<text class='figure' x='463' y='300'>3</text><text class='figure' x='495' y='300'>2</text><text class='figure' x='511' y='300'>5</text><text class='figure' x='527' y='300'>6</text><text class='figure' x='543' y='300'>4</text>
<text class='figure' x='479' y='314'>3</text><text class='figure' x='495' y='314'>5</text><text class='figure' x='511' y='314'>2</text><text class='figure' x='527' y='314'>4</text><text class='figure' x='543' y='314'>6</text>
<text class='figure' x='463' y='328'>3</text><text class='figure' x='495' y='328'>5</text><text class='figure' x='511' y='328'>2</text><text class='figure' x='527' y='328'>6</text><text class='figure' x='543' y='328'>4</text>
<text class='figure' x='479' y='342'>3</text><text class='figure' x='495' y='342'>2</text><text class='figure' x='511' y='342'>5</text><text class='figure' x='527' y='342'>4</text><text class='figure' x='543' y='342'>6</text>
<text class='figure' x='479' y='356'>2</text><text class='figure' x='495' y='356'>3</text><text class='figure' x='511' y='356'>4</text><text class='figure' x='527' y='356'>5</text><text class='figure' x='543' y='356'>6</text>
<polyline class='blueline' points='467,16 483,30 467,44 483,58 499,72 515,86 499,100 515,114 531,128 547,142 531,156 547,170 547,184 531,198 547,212 531,226 515,240 499,254 515,268 499,282 483,296 467,310 483,324 467,338 467,352'></polyline>
<text class='label' x='571' y='21'>1</text><circle cx='576' cy='16' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<text class='label' x='571' y='357'>1</text><circle cx='576' cy='352' r='11' stroke='black' stroke-width='1.4' fill='none'></circle>
<circle cx='467' cy='352' r='2.8' class='dot'></circle>
</svg>
")

(define-test test-blueline-errors ()
  (let ((m (lookup-method-by-title "Advent Surprise Major")))
    (assert-error 'type-error (blueline :stream m))
    (assert-error 'error (blueline "" m))
    (assert-error 'type-error (blueline nil "Advent Surprise Major"))
    (assert-error 'type-error (blueline nil m :layout -1))
    (assert-error 'type-error (blueline nil m :layout :foo))
    (assert-error 'type-error (blueline nil m :hunt-bell -1))
    (assert-error 'type-error (blueline nil m :hunt-bell :nope))
    (assert-error 'type-error (blueline nil m :working-bell -1))
    (assert-error 'type-error (blueline nil m :working-bell :no-way))
    (assert-error 'type-error (blueline nil m :figures 0))
    (assert-error 'type-error (blueline nil m :figures 'foo))
    (assert-error 'type-error (blueline nil m :place-notation 0))
    (assert-error 'type-error (blueline nil m :place-notation "x3x4"))
    (assert-error 'type-error (blueline nil m :place-bells t))
    (assert-error 'type-error (blueline nil m :place-bells 7)))
  (assert-error 'no-place-notation-error (blueline nil (method :name "Foo" :stage 8)))
  (assert-error 'no-place-notation-error (blueline nil (method :name "Foo" :stage nil :place-notation "x3x4x5x6"))))
