;; LOOP

(defparameter *l* '(1 2 3 4 5 6 7 8 9 0))

(loop for item in *l* collecting item)

(loop for i from 1 to 10 collecting i)

;; loop [from|downfrom|upfrom] x [to|upto|below|downto|above] y [by] z
;; defaults to loop for i from 0
(loop for i downfrom 10 above 1 by 2 collecting i)

;; just repeat
(loop repeat 10 do (format t "hello~%"))

;; loop through lists
(loop for v in *l* collect v) ; step car
(loop for v on *l* collect v) ; step cons
(loop for v in *l* by #'cddr collect v) ; by changes how you get the next value

;; use across for vectors
(loop for v across "abcd" collect v)

;; hashtable
(defparameter *h* (make-hash-table))
(setf (gethash 'a *h*) 1)
(setf (gethash 'b *h*) 2)
(loop for k being the hash-keys in *h* collect k)
(loop for v being the hash-values in *h* collect v)

(loop for k being the hash-keys in *h* using (hash-value v) collect (list k k v v))
(loop for v being the hash-values in *h* using (hash-key k) collect (list v k v))


;; package
(loop for s being the present-symbols in *package* collect s)
(loop for s being each external-symbols in *package* collect s)

;; synonyms: the<->each, in<->of, things<->thing

;; equals-then
(loop repeat 5
   for x = 0 then y
   for y = 1 then (+ x y)
   collect y)

(loop repeat 5
   for x = 0 then y
   and y = 1 then (+ x y)
   collect y)


;; with
(loop for i from 0 to 10 with x = 1 do (setf x (+ x i)) collect (list i x))

;; destructuring
(loop for (a . b) in '((1 . 2) (3 . 4) (5 . 6)) collect (+ a b))

;; accumulating
;; verb form [ into var ]
;; verbs: collect append nconc count sum maximize minimize
(loop for i from 0 to 10 collect (+ 3 (- (* i i))))

(loop for i from 0 to 10 append (list i (+ i 1)))

;; do
(loop for i from 0 to 10 do (format t "~A~%" i))


;; return
(block foo
  (loop for i from 0 return 100)
  (format t "this prints"))

(block foo
  (loop for i from 0 do (return-from foo 10))
  (format t "no print"))

;; conditional execution
;; if, when, unless
(loop for i from 0 to 10 if (evenp i) collect i)
(loop for i from 0 to 10 when (evenp i) collect it)


;; setup, teardown

;; naming
(loop named outer for v in *l* do
     (loop for vv in (list v v) do
          (format t "~A" vv)
          (return-from outer)))
