(defun function-example (argument
                         &optional optional-argument (op-default 'a) (c 'b c-supplied-p)
                         &rest rest-arguments
                         &key (x 0 x-supplied-p)
                         ((:banana  b) 1)
                         )
  "This is a function and THIS is a docstring"
  (format t "Hello argument: ~a~%"
          argument)

  (format t "Optional arguments ~a ~a ~a ~a~%"
          optional-argument op-default c c-supplied-p)

  (format t "Rest arguments ~a~%" 
          rest-arguments)
  
  (format t "Key a: ~A~%Key b: ~A~% ~A~%" x b x-supplied-p)
  )

;; not a good idea to mix optional and keyword parameters

(defun return-example (x)
  (dotimes (i x)
    (dotimes (j x)
      (when (> (* i j) x)
        (return-from return-example (list i j))))))

(defun square (x) (* x x))

;; get function object of a name
(function square)
#'square ; same thing

;; funcall: apply function object with arguments passed as normal 
(funcall #'square 1)

;; apply: apply function object with arguments passed as list
(apply #'square '(1))

;; lambda: anonymous function, outputs a function object
(apply (lambda (x y) (* x x y y)) '(1 2))
(apply #'(lambda (x y) (* x x y y)) '(1 2)) ; #' doesn't matter, lambda
                                        ; includes it anyway


;; Variables
(let ((x 1)) ; lexically scoped, doesn't exist outside of this block
  (+ x x))

(let* ((x 1)
       (y x))
  (+ y x))

;; closures
(defparameter *inc/dec* (let ((count 0))
                          (list
                           (lambda () (setf count (1+ count)))
                           (lambda () (setf count (1- count))))))

;; why can't we just apply the function normally here?
(funcall (cadr *inc/dec*))


;; defparameter: set always
(defparameter *a* 1) ; 1
(setf *a* (1+ *a*))  ; 2
(defparameter *a* 1) ; 1
;; defvar: set if undefined
(defvar *a* 10) ; 1

;; you can shadow special variables too, they will still be dynamic

(let ((*a* 10))
  (format t "~a~%" *a*) ; 10
  (incf *a*)
  (format t "~a~%" *a*)) ; 11

*a* ; still 1

;; constants: never change
(defconstant +c+ 11)
(defconstant +c+ 12) ; error, will prompt for "really change this?"

;; setf: set a place
(setf *a* 10) ; *a* is now 10
(defun silly (x)
  (setf x 100))

(silly *a*) ; doesn't do anything outside the function

;; you can chain setfs, setf returns the value set
(incf *a*) ; add
(decf *a*) ; sub


(let ((x 'x)
      (y 'y)
      (z 'z))
  (rotatef x y z) ; rotate: front -> back
  (format t "~a ~a ~a~%" x y z)
  (shiftf x y z) ; shift: return front, rest shift left
  (format t "~a ~a ~a~%" x y z))

;; macros

(if (> 2 3) 'x 'y)
(when (> 3 2) ; when condition, do body
  (format t "one~%")
  (format t "two~%")
  (format t "three~%"))

(unless (> 2 3) ; unless condition, do body
  (format t "one~%")
  (format t "two~%")
  (format t "three~%"))

(cond ; if, else if, else if, else if, else
 ((= 1 2) 'x)
 ((= 2 3) 'y)
 ((= 4 5) 'z)
 (t 'else))

(and 'x nil 'z) ; evaluate up to a nil
(or nil nil 'z) ; evaluate up to non-nil

(dolist (x '(a b c))
  (format t "x: ~A~%" x))

(dotimes (i 5)
  (format t "i: ~A~%" i))


;; first form: initialize vars and how to update them second form: terminating
;; condition and result(s), the last of which is returned steps are all
;; evaluated with old values of their variables and only update after all steps
;; are computed
(do ((n 0 (incf n))
     (m 10 (decf m)))
    ((= n 7) (* n m)))

;; loop
(loop
 (format t "Forever and ever~%"))

;; There are a lot of mysterious keywords... to be explained later.
(loop for x from 1 to 10 summing (expt x 2))
