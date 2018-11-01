;; defining your own macros

;; macros are functions that operate on the ast
(defun primep (number)
  (when (> number 1)
    ;; from 2 to sqrt n, return true if no numbers divide the number
    (loop for fac from 2 to (sqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  ;; make numbers ascending until you hit a prime, return it
  (loop for n from number when (primep n) return n))

(defmacro do-primes ((var init final) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,init) (next-prime (1+ ,var)))
          (,ending-value-name ,final)
          )
         ((> ,var ,ending-value-name))
       ,@body)))

(do-primes (x 1 (random 10))
  (format t "~A~%" x))


(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes-2 ((var init final) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,init) (next-prime (1+ ,var)))
          (,ending-value-name ,final))
         ((> ,var ,ending-value-name))
       ,@body)
    ))

(do-primes-2 (x 1 (random 10))
  (format t "~A~%" x))
