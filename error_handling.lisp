;; conditions
;; objects that are returned when conditions (usually exceptional) happen
;; subclasses of condition instead of standard-object
;; no slot-values access allowed; you must give :reader or :accessor
;; subclass from error (like subclassing with an empty list for normal objects)
(define-condition my-error (error)
  ((text :initarg :text :reader :text)))

(defun do-something-sketchy (text)
  (if (< (length text) 10)
      (format t "~R ~A" (length text) text)
      (error 'my-error :text text)))

;; if something-sketchy throws a condition
(handler-case (do-something-sketchy "hello world")
  ;; and the condition matches here, it'll bind the condition and run the code
  (my-error (c) (format nil "~A" c)))

(defun invoke-something-sketchy (text)
  ;; instead of handling the condition, define a restart for someone else to
  ;; take care of
  (restart-case (do-something-sketchy text)
    (my-restart () nil)))

(defun invoke-sketchy ()
  ;; handler-bind does NOT unwind the stack - the restart will happen wherever
  ;; it is, (down in invoke-something-sketchy) and continue execution
  ;; handler-case would unwind the stack and bring us up to the current level
  (handler-bind ((my-error
                  #'(lambda (c)
                      (invoke-restart 'my-restart))))
    (invoke-something-sketchy "hello")
    (invoke-something-sketchy "Hello world")))

(defun my-restart (c)
  (invoke-restart 'my-restart))


(defun invoke-sketchy ()
  (handler-bind ((my-error #'my-restart))
    (invoke-something-sketchy "hello")
    (invoke-something-sketchy "hello world")))


(invoke-sketchy)



