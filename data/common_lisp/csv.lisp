;;; read/write comma-separated values
;;;
;;; Copyright (C) 2003-2010 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2+)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: csv.lisp,v 2.40 2010/12/31 02:32:44 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/csv.lisp,v $

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  (require :port-mop (translate-logical-pathname "clocc:src;port;mop"))
  ;; `with-collect'
  (require :cllib-simple (translate-logical-pathname "cllib:simple"))
  ;; `symbol-prepend'
  (require :cllib-symb (translate-logical-pathname "cllib:symb"))
  ;; `with-timing', `log'
  (require :cllib-log (translate-logical-pathname "cllib:log")))

(in-package :cllib)

(export '(csv-print-vector csv-parse-string csv-read-file with-csv csv-names
          class-csv-header class-csv-print *csv-first-line-names* *csv-junk*
          new-csv defcsv csv-read csv-write float% csv-i/o csv-i/o-name
          csv-i/o-header csv-i/o-reader csv-i/o-writer csv-i/o-package
          *csv-separator* *csv-whitespace* *csv-progress* *csv-progress-1*))

(defcustom *csv-separator* character #\,
  "The separator in the CSV file, normally the comma.")

(defcustom *csv-first-line-names* (or t nil :default) :default
  "How to treat the first line in WITH-CSV et el.
If this is T (or :DEFAULT and the first line starts with a +COMMENTS+
 character), treat the first line as the vector of column names.
Otherwise, the first line is nothing special.")

(defun csv-print-vector (vec &optional (out *standard-output*))
  "Print a vector as a comma-separated line."
  (declare (type vector vec) (stream out))
  (loop :with len = (length vec) :for val :across vec :and ii :from 1
        :when val :do (write val :stream out :escape nil)
        :unless (= ii len) :do (write-char *csv-separator* out))
  (terpri out))

(defcustom *csv-whitespace* (or null string) +whitespace+
  "The string of characters to trim from the values.")
(defcustom *csv-progress* integer 1000
  "*How often the progress report should be made")
(defcustom *csv-progress-1* integer 10
  "*How often the secondary progress report should be made")

(defcustom *csv-junk* (or symbol integer) :ERROR
  "How to treat lines of wrong length.
When the :JUNK argument is :ERROR, signal an error.
When it is :WARNING, issue a warning and drop the line.
When it is a number, issue at most this many warnings.
When it is :KEEP, keep the line as is.")

(defun csv-trim (whitespace string)
  "Trim the string argument from the whitespace."
  (let ((clean (string-trim whitespace string)))
    (if (zerop (length clean)) nil clean)))

(defun csv-parse-string (string &key
                         ((:separator *csv-separator*) *csv-separator*)
                         ((:whitespace *csv-whitespace*) *csv-whitespace*))
  "Parse a string, returning a vector of strings."
  (loop :with res :and current :and len = (length string) :and end
    :for beg = 0 :then (1+ end) :while (< beg len) :do
    (if (char= #\" (char string beg))
        (multiple-value-bind (w e)
            (read-from-string string t nil :start beg)
          (setq current (and (plusp (length w)) w)
                end e))
        (let ((e (or (position *csv-separator* string :test #'char= :start beg)
                     len)))
          (setq current (and (> e beg) ; otherwise NIL = missing
                             (subseq string beg e))
                end e)))
    (push (and current (csv-trim *csv-whitespace* current)) res)
    :finally (return (coerce (nreverse res) 'vector))))

(defconst +comments+ string "#;" "Characters that start comments.")
(defun uncomment-line (line)
  "Remove the comment prefix from the string."
  (if (find (char line 0) +comments+)
      (string-left-trim +whitespace+ (string-left-trim +comments+ line))
      line))

;;;###autoload
(defun csv-names (file)
  "Read and parse as names the first line in the file."
  (csv-parse-string (uncomment-line (with-open-file (s file) (read-line s)))))

(defun csv-check-vec-len (vec cols fn pos)
  (unless (= cols (length vec))
    (error "~S:~:D: Wrong column count: ~:D instead of ~:D: ~S"
           fn pos (length vec) cols vec)))

(defmacro with-csv ((vec file &key (progress '*csv-progress*)
                         (first-line-names '*csv-first-line-names*)
                         (junk '*csv-junk*)
                         (progress-1 '*csv-progress-1*) limit
                         (out '*standard-output*) columns)
                    &body body)
  "Open FILE and set VEC to successive vectors in it.
Return 3 values:
  number of records (lines) read,
  number of bytes in the file,
  fraction of bytes read
  vector of column names if FIRST-LINE-NAMES is non-NIL
    or if it is :DEFAULT and the first line starts with a +COMMENTS+ character."
  (with-gensyms ("WITH-CSV-" in fn fsize ln len cols lim l1 fln drop ja)
    `(with-timing (:out ,out :count ,len :units "records" :progress ,progress
                   :progress-1 ,progress-1)
       (let* ((,fn ,file) ,fsize ,l1
              (,fln ,first-line-names) (,cols ,columns)
              (,ja ,junk) (,drop 0)
              ,@(when limit `((,lim ,limit))))
         (with-open-file (,in ,fn :direction :input)
           (format ,out "~&Reading `~a' [~:d bytes]..."
                   ,fn (setq ,fsize (file-length ,in)))
           (force-output ,out)
           (when (eq ,fln :default)
             (setq ,fln (find (peek-char nil ,in) +comments+)))
           (when ,fln
             (let ((line1 (read-line ,in)))
               (cond ((zerop (length line1))
                      (cerror "ignore, return NIL for names"
                              "empty first line, names expected"))
                     (t (setq ,l1 (csv-parse-string (uncomment-line line1)))
                        (if ,cols (csv-check-vec-len ,l1 ,cols ,fn 0)
                            (setq ,cols (length ,l1)))))))
           (loop :with ,vec :for ,ln = (read-line ,in nil nil) :while ,ln
             ,@(when limit
                 `(:when (and ,lim (= ,len ,lim))
                   :do (warn "reached the limit of ~:D record~:P ~
                              at ~:D byte~:P (~4F%), aborted~%"
                             ,len (file-position ,in)
                             (/ (file-position ,in) ,fsize 1d-2))
                       (loop-finish)
                   :end))
             :do (setq ,ln (string-trim *csv-whitespace* ,ln))
             :if (or (zerop (length ,ln)) ; empty line
                     (find (char ,ln 0) +comments+) ; comment line
                     (progn (setq ,vec (csv-parse-string ,ln)) (incf ,len)
                            (if ,cols
                                (case ,ja
                                  (:keep nil)
                                  (:error
                                   (csv-check-vec-len ,vec ,cols ,fn ,len))
                                  (t
                                   (handler-case (csv-check-vec-len
                                                  ,vec ,cols ,fn ,len)
                                     (error (c)
                                       (unless (eql ,ja 0)
                                         (warn (princ-to-string c)))
                                       (when (and (integerp ,ja) (plusp ,ja))
                                         (decf ,ja)
                                         (when (zerop ,ja)
                                           (warn "Further warnings omitted")))
                                       t))))
                                (and (setq ,cols (length ,vec)) nil))))
             :do (incf ,drop)
             :else :do ,@body
             (progress (/ (file-position ,in) ,fsize)
                       ;; print <*...*> when we expect to reach limit
                       (if ,(when limit `(and ,lim (> ,len (* ,lim pos))))
                           "*" ""))
             :end
             :finally (format ,out "done [~:d record~:p~@[, ~:d column~:p~]~
                                    ~[~:;,~:* ~:d line~:p dropped~]]"
                              ,len ,cols ,drop)
             :finally (return
                        (values ,len (file-length ,in)
                                (if (zerop ,fsize) 1
                                    (/ (file-position ,in) ,fsize))
                                ,l1))))))))

;;;###autoload
(defun csv-read-file (inf &key ((:junk *csv-junk*) *csv-junk*)
                      ((:first-line-names *csv-first-line-names*)
                       *csv-first-line-names*)
                      ((:separator *csv-separator*) *csv-separator*))
  "Read comma-separated values into a list of vectors."
  (let (len file-size complete names)
    (declare (ignorable complete))
    (values (with-collect (coll)
              (setf (values len file-size complete names)
                    (with-csv (vec inf) (coll vec))))
            len file-size names)))

;;; defstruct i/o
(defun class-csv-header (class &key (out *standard-output*))
  "Print the CSV header for the class to the stream."
  (format out "#~{~A~^,~}~%" (port:class-slot-list class)))

(defun class-csv-print (obj &key (out *standard-output*))
  (format out "~{~A~^,~}~%" (mapcar (lambda (slot) (slot-value obj slot))
                                    (port:class-slot-list obj))))


;;; generic CSV i/o
(defstruct csv-i/o
  (name (port:required-argument) :type symbol)
  (header (port:required-argument) :type vector)
  (reader (port:required-argument) :type function)
  (writer (port:required-argument) :type function)
  (package *package* :type 'package))
(defvar *csv-i/o* (make-hash-table :test 'eq) "type -> csv-i/o")
(defun new-csv (&rest args &key name &allow-other-keys)
  (let ((old (gethash name *csv-i/o*)))
    (when old
      (warn "Redefining CSV i/o for type ~S" name))
    (setf (gethash name *csv-i/o*) (apply #'make-csv-i/o args))))
(defun csv-i/o (type)
  (or (gethash type *csv-i/o*)
      (error "unknown CSV i/o ~S" type)))
(defun csv-read (type file)
  "Read a csv FILE with records of the given TYPE (defined with DEFCSV)."
  (let ((csv-i/o (csv-i/o type)))
    (multiple-value-bind (data len file-size names)
        (csv-read-file file :first-line-names t :junk :keep)
      (declare (ignore len file-size))
      (unless (equalp names (csv-i/o-header csv-i/o))
        (cerror "ignore and procede" "~S(~S): bad header ~S (expected ~S)"
                'csv-read type names (csv-i/o-header csv-i/o)))
      (mapcar (csv-i/o-reader csv-i/o) data))))
(defun csv-write (type file data)
  "Write a csv FILE records DATA of the given TYPE (defined with DEFCSV)."
  (let ((csv-i/o (csv-i/o type)))
    (write-list-to-file
     data file (csv-i/o-writer csv-i/o)
     (lambda (out)
       (write-char (char +comments+ 0) out)
       (loop :for name :across (csv-i/o-header csv-i/o) :and i :upfrom 0
         :do (unless (zerop i) (write-char *csv-separator* out))
         (write-string name out))
       (terpri out)))))

;;; struct definition for CSV i/o
(defun type-parser (slot-type)
  (ecase slot-type
    (symbol (lambda (s) (intern (nstring-upcase (nsubstitute #\- #\Space s)))))
    (string #'identity)
    (integer #'parse-integer)
    (float #'read-from-string)
    (float% (lambda (s) (/ (read-from-string (nsubstitute #\Space #\% s))
                           100)))))
(defun type-default (slot-type)
  (ecase slot-type
    (symbol nil)
    (string "")
    (integer 0)
    ((float float%) 0s0)))
(defun type-type (slot-type)
  (case slot-type
    (float% 'float)
    (t slot-type)))
(defun set-slots-documentation (slots type)
  (mapc (lambda (slotd dslot)
          (setf (documentation (car (port:slot-definition-readers dslot)) t)
                (car slotd)))
        slots (port:class-direct-slots (find-class type))))
;; MAKE-READER & MAKE-WRITER are separate macros to avoid calling MOP functions
;; at read time when DEFSTRUCT has not been called yet.
(defmacro make-reader (vec slots type package)
  `(let ((len (length ,vec)) (*package* ,(find-package package)))
     (,(port:structure-keyword-constructor `,type)
      ,@(loop :for pos :upfrom 0
          :for dslot :in (port:class-direct-slots (find-class `,type))
          :for slotd :in slots
          :for slot-type = (third slotd)
          :for parser = (or (fourth slotd) (type-parser slot-type))
          :nconc `(,(car (port:slot-definition-initargs dslot))
                    (let ((s (and (< ,pos len) (aref vec ,pos))))
                      (if s (funcall ,parser s)
                          ,(type-default slot-type))))))))
(defmacro make-writer (obj out type)
  `(progn
     ,@(loop :for pos :upfrom 0
         :for slot :in (port:class-direct-slots (find-class type))
         :nconc `(,@(when (plusp pos) `((write-char *csv-separator* ,out)))
                    (write (,(car (port:slot-definition-readers slot)) ,obj)
                           :stream ,out :escape nil)))))
(defmacro defcsv (type (&key (package (symbol-package type))) slots
                  &aux (reader (symbol-prepend type '#:csv-reader-))
                  (writer (symbol-prepend type '#:csv-writer-)))
  "Define a structure type and the concomitant CSV i/o.
TYPE if the structure type to be defined.
PACKAGE is the package where the symbols are placed.
SLOTS is a list of
  (CSV-name-string slot-name-symbol slot-type &optional parser)
SLOT-TYPE can be STRING, SYMBOL, INTEGER, FLOAT, FLOAT%.
PARSER is a function to be used instead of the type-appropriate default."
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defstruct ,type
         ,@(mapcar (lambda (slot)
                     (destructuring-bind (string symbol slot-type &rest opts)
                         slot
                       (declare (ignore string opts))
                       `(,symbol (port:required-argument)
                                 :type ,(type-type slot-type))))
                   slots)))
     (set-slots-documentation ',slots ',type)
     (defun ,reader (vec) (make-reader vec ,slots ,type ,package))
     (defun ,writer (obj out) (make-writer obj out ,type))
     (new-csv
      :name ',type
      :header ,(coerce (mapcar #'car slots) 'vector)
      :reader #',reader :writer #',writer
      :package ,package)))

(provide :cllib-csv)
;;; file csv.lisp ends here
