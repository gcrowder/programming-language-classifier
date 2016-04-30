;;; Naive Bayesian classifier
;;;
;;; C. Elkan "Boosting and Naive Bayesian Learning",
;;;	UCSD, Technical Report CS97-557, 1997
;;; http://www-cse.ucsd.edu/users/elkan/papers/bnb.ps
;;;
;;; Copyright (C) 2010 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: bayes.lisp,v 2.21 2010/12/29 01:44:39 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/bayes.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `mesg'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; `hash-table->alist', `print-counts', `copy-hash-table'
  (require :cllib-miscprint (translate-logical-pathname "cllib:miscprint"))
  ;; `top-bottom-ui'
  (require :cllib-sorted (translate-logical-pathname "cllib:sorted"))
  ;; `array-lin-comb'
  (require :cllib-matrix (translate-logical-pathname "cllib:matrix"))
  ;; `mutual-information-N', `to-percent', `sample', `entropy-distribution'
  (require :cllib-math (translate-logical-pathname "cllib:math")))

(in-package :cllib)

(export '(nb-model nb-model-make nb-add-observation nb-model-prune
          *nb-describe-feature-count* *nb-describe-feature-grouper*
          *prune-methods* feature-power feature-weight
          nb-predict-classes logodds-to-prob best-class nb-evaluate
          nb-boost train-test))

(defstruct nb-model
  (name (port:required-argument)) ; any ID
  (count 0 :type real)       ; sample count; real to accommodate weights
  (class-names (port:required-argument) :type (vector symbol))
  (class-counts (port:required-argument) :type (vector real))
  (features (port:required-argument) :type hash-table)) ; feature -> counts

(defun nb-model-check (model)
  (let ((cc (nb-model-class-counts model)))
    (assert (= (length (nb-model-class-names model)) (length cc)))
    (assert (= (nb-model-count model) (reduce #'+ cc)))))

(defun nb-model-copy (model)
  "Deep copy of fields modifiable during model building."
  (let ((ret (copy-nb-model model)))
    (setf (nb-model-class-counts ret) (copy-seq (nb-model-class-counts model))
          (nb-model-features ret)
          (cllib:copy-hash-table (nb-model-features model)))
    ret))

(defmethod print-object ((model nb-model) (out stream))
  (if *print-readably* (call-next-method)
      (print-unreadable-object (model out :type t)
        (format out "~S observation~:P:~:D classes:~S features:~:D"
                (nb-model-name model) (nb-model-count model)
                (nb-model-class-names model)
                (hash-table-count (nb-model-features model))))))

(defun nb-model-make (name class-names &key (feature-test 'equal))
  (make-nb-model :name name
                 :class-names class-names
                 :class-counts (make-array (length class-names)
                                           :initial-element 0)
                 :features (make-hash-table :test feature-test)))

(defun feature-counts (model feature)
  "Return the vector of counts for the given feature."
  (let ((f (nb-model-features model)))
    (or (gethash feature f)
        (setf (gethash feature f)
              (make-array (length (nb-model-class-names model))
                          :initial-element 0)))))

(defun nb-model-merge (model1 model2 new-name)
  (let ((cnames (nb-model-class-names model1)) ret
        (f1 (nb-model-features model1)) (f2 (nb-model-features model2)))
    ;; make sure the models are compatible (i.e., predict the same classes)
    (assert (equalp cnames (nb-model-class-names model2)))
    (assert (eq (hash-table-test f1) (hash-table-test f2)))
    (setq ret (nb-model-make new-name (copy-seq (nb-model-class-names model1))
                             :feature-test (hash-table-test f1)))
    (setf (nb-model-count ret)
          (+ (nb-model-count model1) (nb-model-count model2)))
    ;; merge class counts
    (cllib:array-lin-comb 1 (nb-model-class-counts model1)
                          1 (nb-model-class-counts model2)
                          (nb-model-class-counts ret))
    ;; merge feature counts
    (dolist (f (list f1 f2) ret)
      (maphash (lambda (feature counts &aux (fc (feature-counts ret feature)))
                 (cllib:array-lin-comb 1 fc 1 counts fc))
               f))))

(defun nb-class-index (model class)
  "Convert symbolic or numeric class id to in index in CLASS-COUNTS et al."
  (etypecase class
    (integer
     (if (< -1 class (length (nb-model-class-names model))) class
         (error "Class ~:D out of range for ~S" class model)))
    (symbol
     (or (position class (nb-model-class-names model))
         (error "Class ~S is not valid for ~S" class model)))))

(defun nb-add-observation (model class features &key (weight 1))
  "Add an observation with the given features of the given class."
  (assert (plusp weight) (weight) "~S: weight ~S must be positive"
          'nb-add-observation weight)
  (let ((ci (nb-class-index model class)))
    (incf (nb-model-count model) weight)
    (incf (aref (nb-model-class-counts model) ci) weight)
    (dolist (feature features)
      (incf (aref (feature-counts model feature) ci) weight))))

(defun feature-weight (counts)
  "The number of times the feature appears in the training sample."
  (reduce #'+ counts))

(defun feature-power (counts)
  "An indicator of the predictive power of the feature."
  (* (feature-weight counts)
     (- (log (length counts) 2) ; max possible entropy
        (cllib:entropy-distribution counts)))) ; actual entropy

(defun feature-groups (model &key (key #'value) (test #'eql))
  "Return the list of feature groups, their counts and powers."
  (let ((fg (make-hash-table :test test)))
    (maphash (lambda (feature counts)
               (let ((g (funcall key feature)))
                 (let ((cp (gethash g fg)))
                   (cond (cp
                          (cllib:array-lin-comb 1 (car cp) 1 counts (car cp))
                          (incf (cadr cp) (feature-power counts)))
                         (t (setf (gethash g fg)
                                  (list counts (feature-power counts))))))))
             (nb-model-features model))
    (sort (cdr (cllib:hash-table->alist fg)) #'> :key #'third)))

(defcustom *nb-describe-feature-count* integer 10
  "*The number of top and bottom features to print in DESCRIBE.")
(defcustom *nb-describe-feature-grouper* (or symbol function) nil
  "*The default :KEY argument for FEATURE-GROUPS in DESCRIBE.")
(defmethod describe-object ((model nb-model) (out stream))
  (format out "a Naive Bayesian model.~%Features and their powers:~%")
  (let* ((fht (nb-model-features model)) (fn (hash-table-count fht))
         (features (cdr (cllib:hash-table->alist fht))))
    (if (> fn (* *nb-describe-feature-count* 2))
        (cllib:top-bottom-ui
         features *nb-describe-feature-count* *nb-describe-feature-count* nil
         :key (lambda (feature.counts) (feature-power (cdr feature.counts)))
         :out out :label #'identity)
        (let ((features
               (sort (mapcar
                      (lambda (feature.counts)
                        (cons feature.counts
                              (feature-power (cdr feature.counts))))
                      features)
                     #'> :key #'cdr)))
          (dolist (l features)
            (format out "feature=~A counts=~A power=~G~%"
                    (caar l) (cdar l) (cdr l))))))
  (when *nb-describe-feature-grouper*
    (format out "Feature groups and their powers:~%")
    (dolist (l (feature-groups model :key *nb-describe-feature-grouper*))
      (format out "group=~A counts=~A power=~G~%"
              (first l) (second l) (third l)))))

(defcustom *prune-methods* list
  `((,#'feature-weight 1 "weight")
    (,#'feature-power 1 "power"))
  "*The methods of pruning models.")

(defun nb-model-prune (model feature-quality threshold
                       &key (out *standard-output*)
                       (name (or (nth-value 2 (function-lambda-expression
                                               feature-quality))
                                 feature-quality)))
  "Remove features observed fewer than THRESHOLD times."
  (let ((features (nb-model-features model)) (removed 0))
    (cllib:mesg :bayes out "~&Pruning ~S by ~A to ~:D~%" model name threshold)
    (maphash (lambda (feature counts)
               (when (> threshold (funcall feature-quality counts))
                 (when out (format out "Removing ~S ~S~%" feature counts))
                 (incf removed)
                 (remhash feature features)))
             features)
    (cllib:mesg :bayes out "Pruned ~S (removed ~:D feature~:P)~%" model removed)
    removed))

(defun logodds (this total)
  ;; should be using ieee infinities and nans...
  (cond ((zerop this) '-infinity)
        ((= total this) '+infinity)
        (t (- (log this) (log (- total this))))))

(defgeneric logodds-to-prob (lo)
  (:method ((lo sequence))
    (map (type-of lo) #'logodds-to-prob lo))
  (:method ((lo (eql '-infinity))) 0)
  (:method ((lo (eql '+infinity))) 1)
  (:method ((lo (eql 'nan))) 'nan)
  (:method ((lo number)) (/ (1+ (exp (- lo))))))

(defun logodds+ (lo0 lo1)
  (case lo0
    ((nan) 'nan)
    ((+infinity) (case lo1 ((nan -infinity) 'nan) (t '+infinity)))
    ((-infinity) (case lo1 ((nan +infinity) 'nan) (t '-infinity)))
    (t (etypecase lo1 (symbol lo1) (number (+ lo0 lo1))))))

(defun nb-predict-classes (model features)
  "Return the vector of logodds for classes.
I.e., P(class) = 1/(1+exp(-logodds))."
  (let ((count (nb-model-count model))
        (nc (length (nb-model-class-names model)))
        (ft (nb-model-features model)))
    (unless (plusp count)
      (error "~S(~S): no observations yet" 'nb-predict-classes model))
    (reduce (lambda (vec feature)
              (let ((fc (gethash feature ft)))
                (when fc
                  (let ((tot (reduce #'+ fc)))
                    ;; tot>0 is guaranteed by the assert in nb-add-observation
                    (loop :for i :below nc :for c :across fc
                      :do (setf (aref vec i)
                                (logodds+ (aref vec i) (logodds c tot))))))
                vec))
            features
            :initial-value (map 'vector ; compute "b = ln b1 - ln b0"
                                (lambda (n) (logodds n count))
                                (nb-model-class-counts model)))))

(defun best-class (logodds)
  "Return the best class index or NIL if all logodds is -INFINITY or NAN
or T if multiple best classes."
  (loop :with ret = nil :with best = nil
    :for lo :across logodds :for index :upfrom 0 :do
    (case lo
      ((-infinity nan))
      ((+infinity) (if (eq best '+infinity) (return T)
                       (setq best '+infinity ret index)))
      (t (unless (eq best '+infinity)
           (when (= best lo) (return T))
           (when (< best lo) (setq best lo ret index)))))
    :finally (return (values ret best))))

(defun nb-evaluate (model observations &key (key #'identity)
                    (out *standard-output*))
  "Return the proficiency of the model on the observations.
KEY should return a cons (CLASS . FEATURES)."
  (let ((failed 0))
    (multiple-value-bind (mi h correct detected)
        (cllib:mutual-information-N
         (map 'vector (lambda (o)
                        (let* ((c-f (funcall key o))
                               (detected (best-class (nb-predict-classes
                                                      model (cdr c-f)))))
                          (when (symbolp detected) (incf failed))
                          (cons (car c-f) detected)))
              observations)
         :key #'identity)
      (when (plusp failed)
        (cllib:mesg :bayes out
                    "~&~S(~S): failed on ~:D observation~:P (~,2F%)~%"
                    'nb-evaluate model failed
                    (cllib:to-percent (/ failed (length observations)))))
      (cllib:mesg :bayes out "~&~S(~S, ~:D observation~:P): I(C,D)=~6F  H(C,D)=~6F  H(C)=~6F  H(D)=~6F  Proficiency=~6F~%"
                  'nb-evaluate model (length observations)
                  mi h correct detected (/ mi correct))
      (/ mi correct))))

(defun nb-boost-once (model observations &key (key #'identity))
  "Add misclassified OBSERVATIONS to MODEL.
KEY should return a cons (CLASS . FEATURES)."
  (let ((misclassified ())
        (classes (nb-model-class-names model)))
    (map nil (lambda (o)
               (let* ((c-f (funcall key o))
                      (bc (best-class (nb-predict-classes model (cdr c-f)))))
                 (unless (and (integerp bc)
                              (eql (aref classes bc) (car c-f)))
                   (push c-f misclassified))))
         observations)
    (dolist (mi misclassified (length misclassified))
      (nb-add-observation model (car mi) (cdr mi)))))

(defun nb-boost (model train test &key (key #'identity) (repeat t)
                 (out *standard-output*))
  "Perform several rounds of boosting by TRAIN and testing on TEST.
Return the best model so far and its proficiency (on TEST).
If REPEAT is T, boost while there is an improvement,
 if an integer, do this many rounds and to pick the best."
  (do* ((best (nb-model-copy model)) (round 1 (1+ round))
        (misclassified -1) (best-round 0)
        (proficiency (nb-evaluate model train :key key :out out))
        (last 1))
       ((or (zerop misclassified)
            (if (realp repeat)
                (< repeat round) ; fixed number of rounds
                (<= last proficiency))) ; while improving
        (values best proficiency best-round))
    (cllib:mesg :bayes out "~&~S(~S) [[[round ~:D proficiency=~G~%"
                'nb-boost model round proficiency)
    (setq misclassified (nb-boost-once model train :key key)
          last (nb-evaluate model test :key key :out out))
    (cllib:mesg :bayes out
                "~&~S(~S) ]]]round ~:D misclassified:~:D proficiency=~G~%"
                'nb-boost model round misclassified last)
    (when (< proficiency last)
      (setq proficiency last
            best-round round
            best (nb-model-copy model)))))

(defun train-test (observations &key (key #'identity) (out *standard-output*)
                   (model-name (port:required-argument)) (train-rate 0.7)
                   (prune *prune-methods*) (boost t)
                   (feature-test 'equal) (class-test 'eql) (min-box-size 5))
  "Build a model and test it.
Split OBSERVATIONS at TRAIN-RATE into TRAIN and TEST sets,
 build a model on TRAIN, evaluate on TEST.
NB: if BOOST is non-NIL, the final model is selected based on TEST,
    so it must be evaluated on a separate data set."
  (let* ((classes
          (cdr (cllib:hash-table->alist
                (let ((ht (make-hash-table :test class-test)))
                  (map nil (lambda (o)
                             (let ((c-f (funcall key o)))
                               (push (cdr c-f) (gethash (car c-f) ht))))
                       observations)
                  ht))))
         (model (nb-model-make model-name (map 'vector #'car classes)
                               :feature-test feature-test))
         (train ()) (test ()))
    (when (and out (cllib:print-log-p :bayes))
      (format out "~&~S(~S): ~:D observation~:P:~%"
              'train-test model-name (length observations))
      (cllib:print-counts
       (mapcar (lambda (c-f) (cons (car c-f) (length (cdr c-f)))) classes)
       :out out))
    ;; sample observations for each class separately,
    ;; so that the `train-rate' is maintained for each class
    (dolist (c-f classes)
      (let* ((features-list (cdr c-f))
             (cons-class (lambda (o) (cons (car c-f) o)))
             (count (length features-list))
             (train-n (round (* train-rate count))))
        (when (< train-n min-box-size)
          (cerror "ignore and proceed"
                  "Too few train samples (~:D < ~:D) for class ~S"
                  train-n min-box-size (car c-f)))
        (when (< (- count train-n) min-box-size)
          (cerror "ignore and proceed"
                  "Too few test samples (~:D < ~:D) for class ~S"
                  (- count train-n) min-box-size (car c-f)))
        (multiple-value-bind (tr te)
            (cllib:sample features-list train-n :complement t)
          (setq train (nconc (mapcar cons-class tr) train)
                test (nconc (mapcar cons-class te) test)))))
    (cllib:mesg :bayes out "~S(~S): ~:D train samples, ~:D test samples~%"
                'train-test model-name (length train) (length test))
    ;; train the model
    (dolist (o train) (nb-add-observation model (car o) (cdr o)))
    ;; prune the mode
    (dolist (p prune)
      (nb-model-prune model (first p) (second p) :out out
                      :name (third p)))
    (cllib:mesg :bayes out "~S: trained ~S~%" 'train-test model)
    ;; boosting
    (if boost
        (nb-boost model train test :key key :out out :repeat boost)
        (values model (nb-evaluate model test :out out)))))

(provide :cllib-bayes)
;;; file bayes.lisp ends here
