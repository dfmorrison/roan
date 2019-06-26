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

(in-package :roan)

(define-constant +methods-database+
    (merge-pathnames (make-pathname :name "methods" :type "db")
                     (asdf:system-source-directory :roan))
  :test #'equal)

(defun %execute-with-methods-database (thunk database busy-timeout)
  (check-type* database (or null pathname string stream))
  (check-type* busy-timeout (or null (integer 0 *)))
  (setf database (or database +methods-database+))
  (unless (probe-file database)
    ;; We need to check the file exists, because if it doesn't SQLite will happily
    ;; create a useless, empty one for us.
    (error "Can't find method database (~A)" database))
  (sqlite:with-open-database (connection database :busy-timeout busy-timeout)
    (funcall thunk connection)))

(defun sql-wildcardify (string)
  ;; backslash escapes any %, _ or \ in the string, converts unescaped ? or * to _ or %,
  ;; removes the backslash from escaped ? or *; any other appearence of backslash
  ;; signals an error
  (check-type* string string)
  (let ((result (make-array (+ (length string) 1) :element-type 'character
                            :adjustable t :fill-pointer 0))
        (pending-backslash nil))
    (labels ((add (c &optional another)
               (setf pending-backslash nil)
               (vector-push-extend c result)
               (when another
                 (vector-push-extend another result))))
      (macrolet ((test-backslash (false &optional true)
                   `(if pending-backslash
                        ,(or true '(error "Backslash can only precede ?, * or \\."))
                        ,false)))
        (iter (for c :in-string string)
              (case c
                (#\\ (test-backslash (setf pending-backslash t) (add c c)))
                ((#\% #\_) (test-backslash (add #\\ c)))
                (#\? (test-backslash (add #\_) (add c)))
                (#\* (test-backslash (add #\%) (add c)))
                (otherwise (test-backslash (add c))))
              (finally (test-backslash nil)))))
    result))

(defun %lookup-methods-by-name (name stage limit update url database busy-timeout)
  (check-type* stage stage)
  (check-type* name string)
  (%lookup-methods stage "name like ? escape '\\'" (sql-wildcardify name)
                   limit update url database busy-timeout))

(defun %lookup-method (name stage)
  (check-type* stage (or null stage))
  (check-type* name string)
  (unless stage
    (multiple-value-setq (name stage) (parse-method-title name)))
  (unless stage
    (setf stage *default-stage*))
  (let ((result (%lookup-methods stage "name = ? collate nocase" name nil nil nil nil nil)))
    (assert (null (rest result)))
    (first result)))

(defun %lookup-methods-by-notation (place-notation stage rotations limit update url
                                    database busy-timeout)
  (check-type* stage stage)
  (check-type* place-notation string)
  (%%lookup-methods-from-changes (parse-place-notation place-notation :stage stage)
                                 rotations limit update url database busy-timeout))

(defun %lookup-methods-from-changes (changes rotations limit update url database
                                     busy-timeout)
  (check-type* changes cons)
  (iter (with stage)
        (for c :in changes)
        (check-type* c row)
        (if-first-time (setf stage (stage c))
                       (unless (eql (stage c) stage)
                         (error "Not all changes in ~S are of the same stage." changes))))
  (%%lookup-methods-from-changes changes rotations limit update url database busy-timeout))


(defun %%lookup-methods-from-changes (changes rotations limit update url database
                                      busy-timeout)
  (labels ((fmt (clist)
             (with-initial-format-characters (place-notation-string clist :comma t))))
    (let ((stage (stage (first changes))))
      (if rotations
          (%lookup-methods stage "canonicalRotation = ?" (fmt (canonical-rotation changes))
                           limit update url database busy-timeout)
          (%lookup-methods stage "notation = ?" (fmt changes)
                           limit update url database busy-timeout)))))

(defun %lookup-methods (stage where param limit update url database busy-timeout)
  (check-type* limit (or null (integer 1 *)))
  (check-type* url (or null string))
  (check-type* update (or null (member :query t :force) (integer 0 *)))
  (when update
    (update-methods-database :since (and (integerp update) update)
                             :force (eq update :force)
                             :database database
                             :url url))
  (with-methods-database (connection :database database :busy-timeout busy-timeout)
    (iter (for (stage name notation tower hand)
               :in-sqlite-query (format nil "select stage, name, notation, firstTower, firstHand ~
                                             from methods where ~A and stage = ~D ~@[ limit ~D~]"
                                        where stage limit)
               :on-database connection
               :with-parameters (param))
          (let ((m (method :stage stage :name name :place-notation notation)))
            (when tower
              (setf (method-property m :first-tower) tower))
            (when hand
              (setf (method-property m :first-hand) hand))
            (collect m)))))

(define-constant +timestamp-scanner+
    (ppcre:create-scanner "(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2}):(\\d{2})")
  :test #'same-type-p)

#-(or clisp lispworks6)
(define-constant +methods-database-url+ "http://www.ringing.org/static/roan-methods.zip"
  :test #'equal)

#-(or clisp lispworks6)
(defconstant +status-ok+ 200)

(define-constant +database-entry-name+ "methods.db" :test #'equal)

(defun parse-timestamp (timestamp)
  (ppcre:register-groups-bind (year month day hour minute second)
      (+timestamp-scanner+ timestamp)
    (and (stringp second) (stringp minute) (stringp hour)
         (stringp day) (stringp month) (stringp year)
         (encode-universal-time (parse-integer second)
                                (parse-integer minute)
                                (parse-integer hour)
                                (parse-integer day)
                                (parse-integer month)
                                (parse-integer year)
                                0))))

#-(or clisp lispworks6)
(defun header-etag (headers)
  (string-trim " \"" (drakma:header-value :etag headers)))

#-(or clisp lispworks6)
(defun get-etag (url)
  (multiple-value-bind (reply status headers uri socket-stream must-close reason)
      (drakma:http-request url :method :head)
    (declare (ignore reply uri socket-stream must-close))
    (unless (eql status +status-ok+)
      (error "Unexpected status of HEAD request on ~A: ~A (~D)." url reason status))
    (header-etag headers)))

#-(or clisp lispworks6)
(defun download-zipped-database (url)
  (multiple-value-bind (stream status headers uri socket-stream must-close reason)
      (drakma:http-request url :method :get :want-stream t)
    (declare (ignore uri socket-stream must-close))
    (unless (eql status +status-ok+)
      (error "Unexpected status of GET request on ~A: ~A (~D)." url reason status))
    (values (fad:with-output-to-temporary-file
                (out :element-type '(unsigned-byte 8)
                     :template (namestring (make-pathname :device "TEMPORARY-FILES"
                                                          :name "roan-methods-temp-%"
                                                          :type "zip")))
              (let ((in (flex:flexi-stream-stream stream)))
                (iter (for b := (read-byte in nil))
                      (while b)
                      (write-byte b out))))
            (header-etag headers))))

#-(or clisp lispworks6)
(defun unzip-database (zipfile)
  (fad:with-output-to-temporary-file
      (out :element-type '(unsigned-byte 8)
           :template (namestring (make-pathname :device "TEMPORARY-FILES"
                                                :name "roan-db-temp-%"
                                                :type "db")))
    (zip:with-zipfile (z zipfile)
      (zip:zipfile-entry-contents
       (zip:get-zipfile-entry +database-entry-name+ z)
       out))))

(defun prepare-database (db etag)
  (sqlite:with-open-database (conn db)
    (sqlite:with-transaction conn
      (sqlite:execute-non-query conn "create unique index key on methods (stage, name)")
      (sqlite:execute-non-query conn "create index notationIndex on methods (notation)")
      (iter (for (rowid stage notation)
                 :in-sqlite-query "select rowid, stage, notation from methods"
                 :on-database conn)
            (when (not (equal notation "")) ; TODO excise this test when no longer needed
              (sqlite:execute-non-query conn
                                        "update methods set canonicalRotation = ? where rowid = ?"
                                        (with-initial-format-characters
                                          (place-notation-string
                                           (canonical-rotation
                                            (parse-place-notation notation :stage stage))
                                           :comma t))
                                        rowid)))
      (sqlite:execute-non-query conn "create index rotationIndex on methods (canonicalRotation)")
      (sqlite:execute-non-query conn "update version set etag = ?" etag))))

(defun get-db-info (connection)
  (sqlite:execute-one-row-m-v connection "select updated, etag from version"))

#-(or clisp lispworks6)
(defun %update-methods-database (since force url database)
  (check-type* since (or null (integer 0 *)))
  (check-type* url (or null string))
  (check-type* database (or null pathname string stream))
  (setf database (pathname (or database +methods-database+)))
  (setf url (or url +methods-database-url+))
  (unless (probe-file database)
    (setf since nil)
    (setf force t))
  (unless (and (null since) force)
    (multiple-value-bind (updated etag)
        (with-methods-database (connection :database database)
          (get-db-info connection))
     (when (and since (< (- (get-universal-time) (parse-timestamp updated)) since))
        (return-from %update-methods-database nil))
      (when (and (not force) (equal (get-etag url) etag))
        (return-from %update-methods-database nil))))
  (let (zipfile temp-db etag)
    (unwind-protect
         (progn
           (multiple-value-setq (zipfile etag) (download-zipped-database url))
           (setf temp-db (unzip-database zipfile))
           (delete-file zipfile)
           (setf zipfile nil)
           (prepare-database temp-db etag)
           (fad:copy-file temp-db database :overwrite t)
           (truename database))
      (delete-file temp-db)
      (when zipfile
        (delete-file zipfile)))))

#+(or clisp lispworks6)
(defun %update-methods-database (since force url database)
  (error "Database updating is not supported in this Lisp implementation."))

(defun %methods-database-attributes (database)
  (check-type* database (or null pathname string stream))
  (setf database (pathname (or database +methods-database+)))
  (when-let ((truename (probe-file database)))
    (with-methods-database (connection :database database)
      (multiple-value-bind (created etag) (get-db-info connection)
        (let ((counts (make-array (+ +maximum-stage+ 1) :initial-element 0)))
          (iter (for (stage count)
                     :in-sqlite-query "select stage, count(*) from methods group by stage"
                     :on-database connection)
                (setf (aref counts stage) count))
          (values truename (parse-timestamp created) etag (reduce #'+ counts) counts))))))
