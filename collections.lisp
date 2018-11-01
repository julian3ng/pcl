;; vectors

(vector 1 2) ;fixed size

(make-array '(5 2) :initial-element nil) ; fixed size
(make-array 5 :fill-pointer 0) ; fill-pointer is where we next push things

(defparameter *x* (make-array 5 :fill-pointer 0))
(vector-push 'a *x*) ;push 'a to *x*, return location it was place
(vector-pop *x*) ;pop from *x*, return what was popped

(defparameter *x* (make-array 5 :fill-pointer 0 :adjustable t))
(vector-push-extend 'b *x*) ; push and extend if necessary
(vector-pop *x*)


;; making strings that aren't literals
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)
;; bit vectors
(make-array 4 :initial-element 0 :element-type 'bit)


;; sequence functions
(length *x*) 
(elt *x* 3) ;ith element of x
(setf (elt *x* 3) 10)

;; all of these work on lists too
;; use :test to change how equality is used
;; use :key to pass a 1-arg function that generates a value that is compared instead
;; use :start and :end to restrict the function
;; use :from-end t to move in reverse
(count 1 #(1 1 1 2 3))
(remove 1 #(1 2 1 2 1 1))
(substitute 5 1 #(1 2 1 2 1))
(find 1 #(2 2 2 1 2))
(find 3 #(2 2 2 2 2))
(position 1 #(2 2 2 1 2))

;; higher order variants
;; these take the same keywords minus :test, as we provide a test anyways
(count-if #'evenp #(1 2 3 4 5))
(count-if-not #'evenp #(1 2 3 4 5))
(position-if #'digit-char-p "abcd0001")
(remove-if-not (lambda (x) (char= (elt x 0) #\f))
               #("foo" "bar" "far" "baz"))

;; whole sequence functions
(copy-seq #(1 2 3))
(reverse #(1 2 3))
(concatenate 'vector #(1 2 3) #(2 3 4) '(4 5 6))

;; sort, stable sort (destructive!)
(sort #(1 4 2 2 5 6) #'<) ; might reorder equivalent items
(stable-sort #(1 4 2 2 5 6) #'<) ; will not reorder equivalent items

;; merge
(merge 'vector '(1 3 5) #(2 4 6) #'<)

;; subsequencing
(subseq "foobarbaz" 3 6) ; half-open range
(defparameter *x* (copy-seq "foobarbaz"))
(setf (subseq *x* 3 6) "xxx")
(setf (subseq *x* 3 6) "z")

;; search
(search "bar" "foobarbaz")

;; mismatch
(mismatch "foobar" "foobaz")

;; sequence predicates
(every #'evenp #(2 4 6))
(some #'oddp #(2 4 6))
(notany #'evenp #(1 3 5))
(notevery #'evenp #(2 4 3))

(every #'> #(3 4 5) #(2 3 4)) ; zip and check pairs

;; map: n-ary function and n sequences into one sequence
(map 'vector #'* #(1 2 3 4 5) '(2 3 4 5 6))

;; put it into something instead of a new sequence
(let ((a (make-array 3)))
  (map-into a #'+ '(1 2 3) '(2 3 4))
  a)

(reduce #'+ #(1 2 3 4 5) :initial-value 3)

;; hash tables
(make-hash-table) ; keys are eql
(make-hash-table :test 'equal) ; keys are equal, for strings
;; can also use eq and equalp

(defparameter *h* (make-hash-table))
(gethash 'foo *h*)
(setf (gethash 'foo *h*) nil)
(setf (gethash 'bar *h*) 1)
(clrhash *h*)

(multiple-value-bind (value present) (gethash 'foo *h*)
  (if present
      (format t "Value was found and is ~a~%" value)
      (format t "Value was not found")))

;; maphash
(maphash (lambda (k v) (format t "~a ~a~%" k v)) *h*)

;; or loop
(loop for k being the hash-keys in *h* using (hash-value v)
      do (format t "~a => ~a~%" k v))

;; lists
;; they're cons cells yay
;;

(defparameter *a* '(1 2))
(defparameter *b* '(3 4))
(defparameter *c* (append *a* *b*))
(setf (car *b*) 300)
*c* ;; oops, they share structure
;; write in functional style folks

;; functions prefixed with 'n' are nonconsing: they reuse the cons cells
;; don't use old variables after using an nfunction on them

;; mapping

(mapcar  (lambda (x) (+ 2 x)) '(1 2 3 4 5))
(mapcar #'+ '(1 2 3) '(4 5 6))

(maplist (lambda (p) (append (cdr p) (list (car p)))) '(1 2 3 4 5))
(mapcar (lambda (c) (list c c)) '(1 2 3 4 5)) ; cons results together
(mapcan (lambda (c) (list c c)) '(1 2 3 4 5)) ; nconc results together (flatmap)
(mapcon (lambda (p) (list (cdr p) (car p))) '(1 2 3 4 5))

;; just for side effecrs
(mapc (lambda (c) (format t "~a" c)) '(1 2 3 4 5))
(mapl (lambda (l) (format t "~a" l)) '(1 2 3 4 5))


;; cons as trees
(defparameter *x* '((1 2) (3 4) (5 6))) ; list of 3 pairs or a tree
(defparameter *y* (copy-list *x*)) ; (1 2) (3 4) (5 6) are not copied, only the first layer cells
(defparameter *z* (copy-tree *x*)) ; everything is copied

(tree-equal *x* *z*)

;; subst: substitute, but takes a tree and returns a same-shaped tree
;; subst-if, subst-if-not
;; nsubst, nsubst-if, nsubst-if-not: nonconsing

;; cons as sets
(defparameter *s* ())
(setf *s*  (adjoin 1 *s*))
(setf *s*  (adjoin 1 *s*))
(pushnew 2 *s*) ; adjoin and setf

;; set functions: member[-if[-not]], intersection, union, set-difference, set-exclusive-or
;; subsetp (:key, :test)

;; alists:

(assoc 'a '((a . 1) (b . 2) (c . 3)))
(acons 'd 4 '((a . 1) (b . 2) (c . 3))) ;; instead of (cons (cons ... ) alist)
;; assoc[-if[-not]] exist
;; rassoc[-if[-not]] uses the cdr to get the entire cell
(rassoc 2 '((a . 1) (b . 2) (c . 3)))

;; pairlis
(pairlis '(a b c) '(1 2 3)) ; make an alist

;; plists:
;; alternate keys and values
(getf '(a 1 b 2 c 3) 'b) ;; getf is setf-able, use remf to remove a plist key

;; every symbol has a symbol-plist
(getf (symbol-plist 'symbol) 'key) ; same as...
(setf  (get 'symbol 'key) "hello")
(get 'symbol 'key)

(remf (symbol-plist 'symbol) 'key) ; same as...
(remprop 'symbol 'key)

;; destructuring bind
(destructuring-bind (&whole whole x (y &optional yy) z) (list 1 '(2 ) 3)
  (list :x x :y y :yy yy :z z :whole whole))
