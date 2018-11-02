;; Affecting evaluation
(quote (1 2 3)) ; '(1 2 3): do not evaluate
(if t 1 2) ; conditional evaluation
(progn (format t "1") (format t "2") (format t "3~%") t) ; sequence evaluation


;; Affecting lexical environment
(let ((x 1))
  (+ x x))

(let* ((x 1)
       (y (+ x x)))
  (* y y))

(flet ((foo (x) (+ x 1))
       (bar (x) (+ x 2)))
  (foo (bar 10)))

(labels ((fact (x) (if (= x 1) 1  (* x (fact (- x 1))))))
  (fact 10))

;; local macro
(macrolet ((foo (x)
             `'(,x ,x)))
  (foo '(1)))

;; local symbol macro: no arguments, just expands 
(symbol-macrolet ((x '(1 2 3)))
  x)

;; global symbol macro
(define-symbol-macro my-symbol-macro '(1 2 3 4 5))



;; blocks can have any symbol for a name, including nil
;; return is sugar for return-from nil
;; which is why you can use return in do, dotimes, etc.
;; defun, flet, labels use the function name for the block
;; thus you must use return-from on them

;; local flow control
(block foo
  (format t "hello world")
  (format t "foo")
  (return-from foo)
  (format t "bar"))

;; tagbody and go are labels and goto
;; useful when directly translating TAOCP algos
(tagbody
 top
   (print 'hello)
   (when (> 3 (random 5))  (go top)))

(tagbody
 a (print 'a) (if (zerop (random 2)) (go c))
 b (print 'b) (if (zerop (random 2)) (go a))
 c (print 'c) (if (zerop (random 2)) (go b))
   )


(defun foo ()
  (format t "Entering foo~%")
  (block a
    (format t " Entering BLOCK~%")
    (bar #'(lambda () (return-from a))) ; passing this through...
    (format t " Leaving BLOCK~%"))
  (format t "Leaving foo~%"))

(defun bar (fn)
  (format t "  Entering bar~%")
  (block a
    (format t "  Entering BLOCK2~%")
    (baz fn) ; and through...
    (format t "  Leaving BLOCK2~%")
    )
  (format t "  Leaving bar~%"))

(defun baz (fn)
  (format t "   Entering baz~%")
  (funcall fn) ; and calling it here breaks out of the block in foo
  (format t "   Leaving baz~%"))

;; thus the Leaving bar/baz strings are never printed
;; we still return from foo's a because it's the only a-block that exists at the time
;; (block names are lexically scoped)
(foo)

(defun ba (fn)
  (format t "x~%")
  (funcall fn))
(defun fo ()
  (tagbody
   a (if (zerop (random 2)) (ba #'(lambda () (go a))))))
(fo)

;; important caveat: lexical variable bindings can outlive their binding form
;; block and tagbody do NOT (you can't return-from or go unless they're on the stack)


(defun closure (x)
  (let ((y (+ x 1)))
    (lambda () y)))

(funcall  (closure 1))

(defun bad (x)
  (block foo
    (format t "hello world~A~%" x)
    (lambda () (return-from foo))))

(funcall  (bad 1)) ; foo doesn't exist anymore, throws error

;; catch and throw: they are from ancient times, don't use
;; like doing the block/return-from example above, but no need to pass a closure
;; just (catch *obj* ...    ) and somewhere inside a (throw *obj* nil) would
;; unwind the stack

;; unwind-protect
;; do stuff, cleanup whether or not something explodes
unwind-protect (let ((s  (open  "xxx")))
                  (unwind-protect 
                       (format t "opened")
                    (close s)))


;; multiple-value-call
;; first: values/values-list
(values-list '(x y z)) ; return x, y, z as multiple values
(values 'x 'y 'z)      ; return x, y, z as multiple values

(funcall #'+ (values 1 2) (values 3 4)) ; non-primary results discarded
(multiple-value-call #'+ (values 1 2) (values 3 4)) ; catch 'em all


(multiple-value-bind (x y) (values 1 2)
  (list x y))

(multiple-value-list (values 1 2))

;; values is setf-able
(defparameter *x* 0)
(defparameter *y* 0)
(setf (values *x* *y*) (values 1 2))


;; eval-when
;; load: load a file, evaluate all top-level forms
;; compile-file: compile .lisp into .fasl, which can be loaded later
;; during load, some forms might have side effects
;; compile-file might not see these until load time
;; to make certain forms be evaluated at compile time...
(eval-when (:compile-toplevel)
  (format t "compile time"))

;; toplevel forms are
;; top-level forms
;; forms in top-level progns
;; macrolet, symbol-macrolets
;; top-level macros
;;
;; not: things inside defuns (just the defun itself)

;; when :compile-toplevel, stuff will happen when compiled as a toplevel form
;; :load-toplevel: compile the subforms as toplevel forms
;; :execute: just operate as a progn


;; locally, the: CL's declaration system that helps out the compiler

;; load-time-value: create a value determined at load time
(defun when-loaded () (load-time-value (get-universal-time)))


;; progv: create new dynamic bindings for variables whose names are determined
;; at runtime
(progv '(a b c) '(1 2 3)
  (+ a b c))


