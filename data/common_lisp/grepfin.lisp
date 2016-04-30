;;; Grep Financial Data
;;;
;;; Copyright (C) 2010-2011 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2+)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: grepfin.lisp,v 2.9 2011/02/13 05:00:05 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/grepfin.lisp,v $

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `open-socket-server'
  (require :port-net (translate-logical-pathname "port:net"))
  ;; `quit'
  (require :port-ext (translate-logical-pathname "port:ext"))
  ;; `string-beg-with'
  (require :cllib-string (translate-logical-pathname "cllib:string"))
  ;; `print-counts'
  (require :cllib-miscprint (translate-logical-pathname "cllib:miscprint"))
  ;; `flush-http'
  (require :cllib-url (translate-logical-pathname "cllib:url"))
  ;; `with-html-tag'
  (require :cllib-htmlgen (translate-logical-pathname "cllib:htmlgen"))
  ;; `csv-i/o'
  (require :cllib-csv (translate-logical-pathname "cllib:csv")))

(defpackage #:grepfin
  (:nicknames #:gf)
  (:use #:cl #:port #:cllib)
  (:export #:*data-dir* #:*fund-dir* #:*stock-file* #:*funds-db*
           #:ensure-data #:query-funds #:query-stocks #:init #:server))

(in-package #:grepfin)

;;; * data

(defcustom *data-dir* pathname (merge-pathnames "data/" (user-homedir-pathname))
  "The location of all financial data.")
(defcustom *fund-dir* pathname (merge-pathnames "funds/" *data-dir*)
  "The location of the tree of funds data from MFFAIS.")

(defpackage "FIN" (:use))

;; export from http://www.mffais.com/
(defcsv holding (:package "FIN")
  (("Fullname" fullname string)
   ("Symbol" symbol symbol)
   ("Country" country symbol)
   ("Industry" industry symbol)
   ("Shares" shares integer)
   ("Activity" activity symbol)
   ("Shares Change" shares-change integer)
   ("Shares Pct Change" shares-change-% float)
   ("Previous Shares" previous-shares integer)
   ("Today Pct. Portfolio" today-portfolio-% float)
   ("As-Of/On Price" as-of/on-price float)
   ("As-Of/On Value" as-of/on-value float)
   ("Today Price" today-price float)
   ("Today Value" today-value float)
   ("Amount Of Chg." amount-of-chg float)
   ("Return" return float)
   ("Result Of Changed Shares Only" result-of-changed-shares-only float)
   ("Lifetime Buy Total" lifetime-buy-total float)
   ("Lifetime Buy Avg. Price" lifetime-buy-avg-price float)
   ("Lifetime Sell Total" lifetime-sell-total float)
   ("Lifetime Sell Avg. Price" lifetime-sell-avg-price float)
   ("Lifetime Avg. Return Pct" lifetime-avg-return-% float)))

;; extra accessors (don't forget to show them in query-funds@:help!)
(defun holdings-total-value (holdings)
  "The total value of holdings (sum of TODAY-VALUE's.)"
  (reduce #'+ holdings :key #'holding-today-value))
(defvar *holding-total-value*)  ; the total value of the current holding
(defun holding-stock (holding)
  "The STOCK object corresponding to the HOLDING or NIL if not known."
  (let ((symbol (holding-symbol holding)))
    (and (boundp symbol) (symbol-value symbol))))
(defun holding-%-of-fund (holding)
  "The HOLDING's today value as percentage of the total fund holdings."
  (/ (holding-today-value holding) 1d-2 *holding-total-value*))
(defun holding-%-of-stock (holding)
  "The HOLDING's today value as percentage of the stock's market cap."
  (let ((stock (holding-stock holding)))
    (and stock                ; today-value is in $; market-cap is in M$
         (/ (holding-today-value holding) 1d4 (stock-market-cap stock)))))
(defun holding-market-cap (holding)
  "The HOLDING's STOCK's market cap or NIL for unknown stocks."
  (let ((stock (holding-stock holding)))
    (and stock (stock-market-cap stock))))

;; for comparing (possibly NIL) return values of HOLDING-%-OF-STOCK et al
(defmacro defstar (f)
  `(defun ,(symbol-concat f '*) (&rest args)
     (and (every #'realp args)
          (apply #',f args))))
(defstar <)
(defstar <=)
(defstar >)
(defstar >=)

(defvar *funds* (make-hash-table :test 'equalp))

(defun read-fund (file)
  (setf (gethash (pathname-name file) *funds*) (csv-read 'holding file)))
(defun read-funds (&key (dir *fund-dir*))
  (mapc #'read-fund (directory (merge-pathnames "**/*.csv" dir))))
(defvar *funds-db* (merge-pathnames "funds.sexp" *data-dir*))
(defun save-funds (&key (file *funds-db*))
  (write-to-file *funds* file))
(defun load-funds (&key (file *funds-db*))
  (setq *funds* (read-from-file file)))
(defun ensure-funds (&key (dir *fund-dir*))
  ;; make sure *funds-db* is up to date
  (let* ((fund-files (directory (merge-pathnames "**/*.csv" dir)
                                #+clisp :full #+clisp t))
         (latest (loop :for ff :in fund-files :maximize
                   #+clisp (apply #'encode-universal-time (third ff))
                   #-clisp (file-write-date ff)))
         (db-fwd (or (ignore-errors (file-write-date *funds-db*)) 0)))
    (cond ((> latest db-fwd)
           (format t "~&Latest fund file(~A) is newer than the fund DB file(~A), rebuilding...~%"
                   (dttm->string latest :format :short)
                   (dttm->string db-fwd :format :short))
           (clrhash *funds*)
           (dolist (ff fund-files)
             (read-fund #+clisp (car ff) #-clisp ff))
           (save-funds))
          (t (load-funds)))
    (values)))                  ; avoid lengthy output of the return value

;; export from http://finviz.com/
(defcsv stock (:package "FIN")
  (("No." no integer)
   ("Ticker" ticker symbol)
   ("Company" company string)
   ("Sector" sector symbol)
   ("Industry" industry symbol)
   ("Country" country symbol)
   ("Market Cap" market-cap float)
   ("P/E" p/e float)
   ("Price" price float)
   ("Change" change float%)
   ("Volume" volume integer)))

(defcustom *stock-file* pathname
  (merge-pathnames "stocksunder300m.csv" *data-dir*)
  "The stock universe from finviz.")
(defvar *stocks* ())
(defun read-stocks (&key (file *stock-file*))
  (dolist (stock (setq *stocks* (csv-read 'stock file)))
    (setf (symbol-value (stock-ticker stock)) stock)))

(defun ensure-data (&optional force)
  (when (or force (null *stocks*))
    (read-stocks))
  (when (or force (zerop (hash-table-count *funds*)))
    (ensure-funds))
  ;; make sure that we know all stocks mentioned in funds and they match
  (let ((unknown (make-hash-table :test 'eq)))
    (maphash (lambda (file holdings)
               (dolist (holding holdings)
                 (unless (boundp (holding-symbol holding))
                   (push file (gethash (holding-symbol holding) unknown)))))
             *funds*)
    (format t "~&~:D unknown stock~:P and ~:D known stock~:P in ~:D fund~:P~%"
            (hash-table-count unknown) (length *stocks*)
            (hash-table-count *funds*)))
  ;; report counts of some symbols
  (format t "~& ## stock counts:~%")
  (dolist (slot '(sector industry country))
    (format t "~& == ~A" slot)
    (let ((ht (make-hash-table :test 'eq)))
      (dolist (stock *stocks*)
        (incf (gethash (slot-value stock slot) ht 0)))
      (print-counts ht)))
  (format t "~& ## fund counts:~%")
  (dolist (slot '(activity industry country))
    (format t "~& == ~A" slot)
    (let ((ht (make-hash-table :test 'eq)))
      (maphash (lambda (file holdings)
                 (declare (ignore file))
                 (dolist (holding holdings)
                   (incf (gethash (slot-value holding slot) ht 0))))
               *funds*)
      (print-counts ht))))

;;; * queries

(defun show-readers (type)
  "Print the list of slot accessors for a TYPE defined with DEFCSV."
  (dolist (dslot (port:class-direct-slots (find-class type)))
    (let ((f (car (port:slot-definition-readers dslot))))
      (format t "~& ~20A   ~A~%" f (documentation f t)))))
(defun show-extras (extras)
  "Print the list of extra accessor functions."
  (dolist (f extras)
    (format t "~& ~A~%  ~A~%" f (documentation f 'function))))

(defgeneric query-stocks (query &key csv)
  (:method ((query (eql :help)) &key &allow-other-keys)
    (format t "~&~S prints stocks which satisfy a certain condition
on the variable ~S of type ~S.
The atomic queries are:~%" 'query-stocks 'STOCK 'STOCK)
    (show-readers 'stock)
    (format t "~&The atomic queries can be combined, e.g.:~%~S
to list all ~A stocks with p/e>10.~%"
            '(AND (< 10 (STOCK-P/E STOCK))
              (EQ (STOCK-COUNTRY STOCK) 'FIN::USA)
              (STOCK-P/E STOCK))
            'FIN::USA))
  (:method ((query function) &key csv)
    (let ((ret ()))
      (dolist (stock *stocks*)
        (let ((res (funcall query stock)))
          (when res
            (push stock ret)
            (format t "~7A: ~A~%" (stock-ticker stock) res))))
      (when (and csv ret) (csv-write 'stock csv ret))))
  (:method ((query cons) &key csv)
    (query-stocks (compile nil `(lambda (stock) ,query)) :csv csv))
  (:method ((query pathname) &key csv)
    (query-stocks (read-from-file query) :csv csv))
  (:method ((query string) &key csv)
    (query-stocks (read-from-file query) :csv csv)))

(let* ((holding (csv-i/o 'holding)) (holding-h (csv-i/o-header holding))
       (stock (csv-i/o 'stock)) (stock-h (csv-i/o-header stock))
       (h-len (1+ (length holding-h)))
       (h-reader (csv-i/o-reader holding)) (s-reader (csv-i/o-reader stock))
       (h-writer (csv-i/o-writer holding)) (s-writer (csv-i/o-writer stock)))
  (new-csv
   :name 'file+holding+stock
   :header (concatenate 'vector #("fund") holding-h stock-h)
   :reader (lambda (vec)
             (let ((len (length vec)))
               (cond ((= 0 len) nil)
                     ((= 1 len) (list (aref vec 0) nil nil))
                     ((>= h-len len)
                      (list (aref vec 0)
                            (funcall h-reader (subseq vec 1))
                            nil))
                     (t (list (aref vec 0)
                              (funcall h-reader (subseq vec 1 h-len))
                              (funcall s-reader (subseq vec h-len)))))))
   :writer (lambda (list out)
             (destructuring-bind (file holding stock) list
               (write-string file out)
               (write-char *csv-separator* out)
               (funcall h-writer holding out)
               (when stock
                 (write-char *csv-separator* out)
                 (funcall s-writer stock out))))
   :package "FIN"))

(defgeneric query-funds (query &key csv)
  (:method ((query (eql :help)) &key &allow-other-keys)
    (format t "~&~S prints funds which satisfy a certain condition
on the variable ~S of type ~S.
The atomic queries are:~%" 'query-funds 'HOLDINGS '(LIST HOLDING))
    (show-readers 'holding)
    (format t "~&Extra functions:~%")
    (show-extras '(holdings-total-value holding-stock holding-%-of-fund
                   holding-%-of-stock))
    (format t "~&The atomic queries can be combined, e.g.:~%~S
to list all the funds who invest more that 5%
in a stock with market cap less than 100M;
or~%~S~%to list all the funds who hold more than 10% of a known stock;
or~%~S~%to combine the above queries.~%"
            '(LET ((*HOLDING-TOTAL-VALUE* (HOLDINGS-TOTAL-VALUE HOLDINGS)))
              (REMOVE-IF-NOT
               (LAMBDA (HOLDING)
                 (AND (> (HOLDING-%-OF-FUND HOLDING) 5)
                      (<* (HOLDING-MARKET-CAP HOLDING) 100)))
               HOLDINGS))
            '(REMOVE-IF-NOT
              (LAMBDA (HOLDING) (>* (HOLDING-%-OF-STOCK HOLDING) 10))
              HOLDINGS)
            '(LET ((*HOLDING-TOTAL-VALUE* (HOLDINGS-TOTAL-VALUE HOLDINGS)))
              (REMOVE-IF-NOT
               (LAMBDA (HOLDING)
                  (AND (> (HOLDING-%-OF-FUND HOLDING) 3)
                       (>* (HOLDING-%-OF-STOCK HOLDING) 5)
                       (<* (HOLDING-MARKET-CAP HOLDING) 100)
                       (< 30 (HOLDING-SHARES-CHANGE-% HOLDING))))
               holdings))))
  (:method ((query function) &key csv)
    (let ((ret ()))
      (maphash (lambda (file holdings)
                 (let ((res (funcall query holdings)))
                   (when res
                     (dolist (h res)
                       (push (list file h (holding-stock h)) ret))
                     (format t "~A: ~A~%" file
                             (mapcar #'holding-symbol res)))))
               *funds*)
      (when (and csv ret)
        (csv-write 'file+holding+stock csv ret))))
  (:method ((query cons) &key csv)
    (query-funds (compile nil `(lambda (holdings) ,query)) :csv csv))
  (:method ((query pathname) &key csv)
    (query-funds (read-from-file query) :csv csv))
  (:method ((query string) &key csv)
    (query-funds (read-from-file query) :csv csv)))

(defun init ()
  (ensure-data)
  (format t "~%~40~~%")
  (query-stocks :help)
  (format t "~%~40~~%")
  (query-funds :help))

(defun parse-request (request)
  "Find GET and split it into a list.
Errors here are our bugs."
  (let ((get (find-if (lambda (line) (string-beg-with #1="GET /" line))
                      request)))
    (if get
        (split-string get "&" :start #2=#.(length #1#)
                      :end (position #\Space get :start #2#))
        (values nil request))))

(defun report-error (sock text &aux
                     (port (nth-value 4 (port:socket-host/port sock))))
  "Report usage on an http socket in html."
  (with-html-output (html sock)
    (with-tag (:h1) (format html "Error"))
    (with-tag (:h2) (format html "Bad request"))
    (etypecase text
      (string (with-tag (:p) (write-string text html)))
      (cons (dolist (s text) (with-tag (:p) (write-string s html)))))
    (with-tag (:h2) (format html "Usage"))
    (with-tag (:p)
      (format html "A request consists of conditions separated with ampersands.
E.g.,")
      (with-tag (:pre)
        (format html "http://localhost:~D/&csv=/tmp/stocks.csv&stock-country=usa&stock-p/e<10" port))
      (format html " or ")
      (with-tag (:pre)
        (format html "http://localhost:~D/&csv=/tmp/funds.csv&holding-%-of-fund>1&holding-%-of-stock>2&holding-market-cap<300" port)))))

(defmacro bad (sock fmt &rest args)
  "Report user error to http output as html."
  `(progn
     (report-error ,sock (format nil ,fmt ,@args))
     (return)))

(defun make-query (realm queries)
  (let ((body
         `(AND ,@(mapcar
                  (lambda (query)
                    (destructuring-bind (cmp func arg) query
                      (typecase arg
                        (real `(,cmp (,func ,realm) ,arg))
                        (cons `(let ((v (,func ,realm)))
                                 (or ,@(mapcar (lambda (a) `(eql ',a v)) arg))))
                        (t (return-from make-query arg)))))
                  queries))))
    (ecase realm
      (HOLDING
       `(LAMBDA (HOLDINGS)
          (LET ((*HOLDING-TOTAL-VALUE* (HOLDINGS-TOTAL-VALUE HOLDINGS)))
            (REMOVE-IF-NOT
             (LAMBDA (HOLDING)
               ,body)
             HOLDINGS))))
      (STOCK
       `(LAMBDA (STOCK) ,body)))))

(defun run-request (list sock)
  "Run the given parsed HTTP request.
Errors here mean user errors; they are printed to SOCK
which is assumed to go to the http client."
  (let (csv queries realm)
    (dolist (cmd list
             (let ((*standard-output* sock))
               (funcall (case realm
                          (holding #'query-funds)
                          (stock #'query-stocks)
                          (t (bad sock "bad realm ~S" realm)))
                        (let ((queryf (make-query realm queries)))
                          (when (atom queryf)
                            (bad sock "bad RHS ~S" queryf))
                          (compile nil queryf))
                        :csv csv)))
      (let ((pos (or (position-if (lambda (c) (or (char= c #\=) (char= c #\<)
                                                  (char= c #\>)))
                                  cmd)
                     (bad sock "bad query ~S" cmd)))
            (len (length cmd)))
        #1=(when (= pos (1- len))
             (bad sock "nothing to compare with in ~S" cmd))
        (let* ((1st (subseq cmd 0 pos)) (sep (char cmd pos))
               (sep= (char= #\= (char cmd (1+ pos)))) arg
               (cmp (ecase sep
                      (#\=
                       '=)
                      (#\> (cond (sep=
                                  (incf pos)
                                  #1#
                                  '>=*)
                                 (t '>*)))
                      (#\< (cond (sep=
                                  (incf pos)
                                  #1#
                                  '<=*)
                                 (t '<*))))))
          (cond ((string-equal 1st "csv")
                 (unless (eq cmp '=)
                   (bad sock "csv file can only be assigned with [=], not with [~S]" cmp))
                 (setq csv (subseq cmd (1+ pos))))
                (t
                 (ecase cmp
                   (=
                    (setq arg
                          (port:string-tokens
                           (nsubstitute #\Space #\, cmd :start pos)
                           :start (1+ pos) :package #.(find-package '#:fin))))
                   ((>* >=* <* <=*)
                    (setq arg (read-from-string cmd t t :start (1+ pos)))))
                 (multiple-value-bind (func status)
                     (find-symbol (nstring-upcase 1st)
                                  #2=#.(find-package '#:grepfin))
                   (unless (eq status :internal)
                     (bad sock "invalid function ~S" 1st))
                   (unless (string-equal 1st "csv")
                     (let* ((r1 (subseq 1st 0 (or (position #\- 1st)
                                                  (bad sock "no dash in ~S" 1st))))
                            (r (or (find-symbol r1 #2#)
                                   (bad sock
                                        "bad realm ~S (should be HOLDING or STOCK)"
                                        r1))))
                       (cond ((null realm) (setq realm r))
                             ((not (eq realm r))
                              (bad sock "inconsistent realms: ~S & ~S" r realm)))))
                   (push (list cmp func arg) queries)))))))))

(defun server (&optional port)
  "Listen on the given port and execute queries."
  (let ((serv (port:open-socket-server port)))
    (format t "~&Listening for connections on port ~S~%"
            (nth-value 1 (port:socket-server-host/port serv)))
    (unwind-protect
         (loop (with-open-stream (sock (port:socket-accept serv))
                 (format t "~&Connection: ~S~%"
                         (multiple-value-list (port:socket-host/port sock)))
                 (multiple-value-bind (parsed raw)
                     (parse-request (flush-http sock))
                   (format t "~&Raw: ~S~%Parsed: ~S~%" raw parsed)
                   (cond ((equalp raw '("quit")) (return))
                         ((equalp raw '("status"))
                          (format sock "~:D stock~:P, ~:D fund~:P~%"
                                  (length *stocks*) (hash-table-count *funds*)))
                         (parsed
                          (format t "~%Result: ~S~%" (run-request parsed sock)))
                         (t (report-error sock raw))))))
      (port:socket-server-close serv))))

(provide :grepfin)
;;; file grepfin.lisp ends here
