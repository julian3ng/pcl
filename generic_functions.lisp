;; making classes

;; standard-object is the default superclass
(defclass A ()
  (a b))

(defclass B (A)
  (a b))

(defgeneric foo (x)
  (:documentation "Foo the x"))

(defmethod foo ((x A))
  (format t "A~%"))



(defmethod foo ((x B))
  (format t "B~%")
  (call-next-method))



(defparameter *c* (make-instance 'B))

(defmethod foo ((x (eql *c*)))
  (format t "*c*~%")
  (call-next-method))

(mapcar #'foo (list (make-instance 'A) (make-instance 'B) *c*))


;; setting slots

(setf (slot-value *c* 'a) 'hello)
(slot-value *c* 'a)

;; initializers
;; :initarg to give make-instance an argument with said keyword
;; :initform to give default value
(defclass point ()
  ((x :initarg :x :initform 0)
   (y :initarg :y :initform 0)))

(slot-value  (make-instance 'point :x 1 :y 2) 'y)

;; defmethod initialize-instance on a class for total control

(defclass point3 ()
  (x y z))

(defmethod initialize-instance ((instance point3) &key)
  (setf (slot-value instance 'x) 0)
  (setf (slot-value instance 'y) 1)
  (setf (slot-value instance 'z) 2)
  (format t "Hello point3~%"))

(make-instance 'point3)

;; initform can be an error if you want to force initialization
(defclass yell-if-unbound ()
  ((x :initarg :x :initform (error ":x not provided"))))

(make-instance 'yell-if-unbound :x 1)


;; initialize-instance does everything with :initarg and :initform, so we can
;; define :after to make derived fields

(defclass verbose-int ()
  ((x :initarg :x :initform 0)))

(defmethod initialize-instance :after ((instance verbose-int) &key)
  (if (> (slot-value instance 'x) 10)
      (format t "BIG~%")
      (format t "small~%"))
  )

(make-instance 'verbose-int :x 100)

;; accessors
(defclass counter ()
  ((x :initform 0)
   (y :initform 10)))

(defgeneric x (c)
  (:documentation "Get x"))

(defmethod x ((c counter))
  (slot-value c 'x))

(defgeneric y (c)
  (:documentation "Get y"))

(defmethod y ((c counter))
  (slot-value c 'y))

;; first is new value, second is object
(defgeneric (setf y) (y c)
  (:documentation "Set y"))

(defmethod (setf y) (y (c counter))
  (setf (slot-value c 'y)  y))

(defparameter *c* (make-instance 'counter))
(x *c*)
(setf (y *c*) 1)
(y *c*)

;; now, without having to write all that boilerplate:

(defclass person ()
  ((name :initarg :name :initform (error "name unspecified") :reader name
         :documentation "Person's name")
   (age :initform 0 :documentation "Person's age")
   (color :initform "blue" :reader color :writer (setf color)
          :documentation "Person's favorite color")
   (food :initform "pho" :accessor food ; same as :reader food :writer (setf food)
         :documentation "Person's favorite food"))) 

(defparameter *j* (make-instance 'person :name "Julian"))

(setf  (color *j*) "green")
(color *j*)


;; standard macros: with-slots, with-accessors
(with-slots (name age color food) *j*
  (setf food "broccoli")
  (format t "~A ~A ~A ~A ~%" name age color food))

;; if name and age had accessors this would work with them too
;; uses accessors instead of slots directly
(with-accessors ((color color) (food food)) *j*
  (setf food "ham")
  (setf color "red")
  (format t "~A ~A~%" color food))

;; :allocation - :class or :instance
(defclass shared-field ()
  ((x :initarg :x :initform 0 :allocation :class)))

(make-instance 'shared-field :x 1)

(slot-value (make-instance 'shared-field) 'x) ; => 1; it's stored in the class!
;; can't access them through the class though, they're not really class variables
;; just shared resources


;; inheritance
;; same-named slots are merged into one specifier
;; slot options are treated differently
;; :initform uses the most specific
;; :initarg uses whatever is provided to make-instance,
;;     (remember they can be differently named)

;; multiple inheritance

(defclass X ()
  ())
(defclass Y () ())
(defclass Z (X Y) ())

(defmethod foo ((x X))
  (format t "x~%"))

(defmethod foo ((x Y))
  (format t "y~%"))

(foo (make-instance 'Z)) ; => x

;; specificity determined by order in superclass list






;; effective method
;; possible methods are listed in order of specificity of arguments
;; a method only matches if all arguments match
;; a specifier only matches if the class matches or the superclass contains it
;; eql specifier only matches if it's actually eql
;; parameters are compared left to right, more specific comes first
;; subclasses are more specific
;;
;; multiple inheritance is decided by an algorithm
;;
;; methods of this type are primary methods

;; we can use :before, :after, and :around to modify the primary method
;; ALL applicable :before methods are invoked before the primary, most to least specific
;; Same for :after occuring after, least to most specific
;; :around happens from most to least specific, and generally needs to call-next-method.


;; method combinations
;; call-next-method bumps you to the next most specific method
;; + and or list append nconc min max progn
;; only support primary and :around methods
;; method combinations define how to combine the results of the primary methods
;; i.e. + will sum all results of the primary methods
;; most-specific first by default
(defgeneric bar (x)
  (:documentation "Bar the x")
  (:method-combination + :most-specific-last))

(defmethod bar + ((x A))
  (format t "Bar A~%"))

;; :most-specific-last doesn't affect :around



