;; Reading files
(open "/tmp/test.txt")


;; using a file
(let ((in (open "/tmp/test.txt" :if-does-not-exist nil)))
  (when in
    (format t "~A~%" (read-line in))
    (close in)))

;; read-* takes optional argument: whether or not to signal error on EOF
;; defaults to t
(let ((in (open "/tmp/test.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do (format t "~A~%" line))
    (close in)))

;; read takes a sexp
(defparameter *s* (open "foo.txt"))
(read *s*)
(read *s*)
(read *s*)
(read *s*)
(close *s*)


;; opening for reading and writing
(let ((s (open "foo.txt" :element-type 'character))
      (o (open "bar.txt" :element-type 'character
               :direction :output :if-exists :supersede)) ; writable, overwrite if exists
      (v (make-array 58 :element-type 'character)))
  (read-sequence v s) ; read into a sequence
  (format t "~A~%" v)
  (write-line "Hello world" o) ; write a string
  (pprint '(1 2 (3 4 (a b))) o) ; write a sexp
  (fresh-line o) ; newline
  (close s)
  (close o))

(with-open-file (f "foo.txt" :element-type '(unsigned-byte 8))
  (read-byte f))
