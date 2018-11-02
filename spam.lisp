(defpackage :com.julian.spam
  (:use :common-lisp :com.julian.pathnames))

(in-package :com.julian.spam)


(defun classify (text)
  (classification (score (extract-features text))))

(defparameter *max-ham-score* 0.4)
(defparameter *min-spam-score* 0.6)

(defun classification (score)
  (values  (cond ((<= score *max-ham-score*) 'ham)
                 ((>= score *min-spam-score*) 'spam)
                 (t 'unsure))
           score))

(defclass word-feature ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we've seen this feature in")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams we've seen this feature in")))

(defvar *feature-database* (make-hash-table :test #'equal))


(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal))
  (setf *total-spams* 0)
  (setf *total-hams* 0))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*) (make-instance 'word-feature :word word))))

(defun split-spaces (text)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space text :start i)
     collect (subseq text i j)
     while j))

(defun extract-words (text)
  (delete-duplicates (split-spaces text) :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

;; technically you have to implement print-object but
;; lisp gives you this to make it easier
(defmethod print-object ((object word-feature) stream)
  ;; print-unreadable-object (object stream :type t) BODY
  ;; is a macro that binds object to the object to be printed,
  ;; stream to the stream we're using
  ;; :type says whether to print the type of the thing
  ;; :identity says whether to print the ID of the thing
  ;; the body says how to print it
  (cl:print-unreadable-object (object stream :type t :identity t)
    (with-slots (word spam-count ham-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

(extract-features "foo bar baz foo")


(defun increment-count (feature type)
  ;; this is a switch, determined by eql
  (ecase type ; exhaustive case - if key isn't found, signal error
    (ham (incf (ham-count feature))) ; note: keys aren't evaluated
    (spam (incf (spam-count feature)))))

(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defun increment-total-count (type)
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))



(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
          (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))


(defun bayesian-spam-probability (feature &optional
                                            (assumed-probability 1/2)
                                            (weight 1))
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
   (loop with m = (/ value 2)
      for i below (/ degrees-of-freedom 2)
      for prob = (exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0))

(defun fisher (probs number-of-probs)
  (inverse-chi-square
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))


(defun score (features)
  (let ((spam-probs ())
        (ham-probs ())
        (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))

    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))


(train "The viagra enlarge" 'spam)
(train "Please excuse my dear aunt sally the quick brown fox jumps" 'ham)
(classify "Please excuse my enlarge viagra")

(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend (list filename type) corpus))

(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))

(defun add-directory-to-corpus (dir type corpus)
  (dolist (filename (list-directory dir))
    (add-file-to-corpus filename type corpus)))

(defparameter *max-chars* (* 10 1024))

(defun train-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) do
       (destructuring-bind (file type) (aref corpus idx)
         (train (start-of-file file *max-chars*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) collect
        (destructuring-bind (file type) (aref corpus idx)
          (multiple-value-bind (classification score)
              (classify (start-of-file file *max-chars*))
            (list 
             :file file
             :type type
             :classification classification
             :score score)))))





(defun test-classifier (corpus testing-fraction)
  (clear-database)
  (let* ((shuffled (shuffle-vector corpus))
         (size (length corpus))
         (train-on (floor (* size (- 1 testing-fraction)))))
    (train-from-corpus shuffled :start 0 :end train-on)
    (test-from-corpus shuffled :start train-on)))
