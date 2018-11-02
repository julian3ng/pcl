;; avoiding name conflicts with packages
;;
;; if you have two packages with same names in them, need to represent them
;; with different symbols

;; packages map strings to symbols
;; reader accesses these mapping with find-symbol and intern
;;
;; by default, find-symbol looks in *package* (current package) for symbols
;; intern looks for symbol and creates it if not found

(intern "foo")
(find-symbol "foo")

;; usually we use unqualified names (no colons)
;; when the reader reads such a name, it upcases it and interns it
;; this is how the right defun is called each time


;; single or double colons are package qualified names
;; first part is package, second is symbol
;; single -> external symbol for external use
;; double -> can refer to any symbol in package, generally a bad idea bc
;; encapsulation violation

;; keyword symbols :like-this - interned in the KEYWORD package and exported automatically
;; interned keyword symbols also define a constant variable of the same name
;; which is why you don't have to quote them

(symbol-name :foo) ; name doesn't include colon
(eql ':foo :foo)


;; uninterned symbols: created each time they are read, not interned in any package
(eql '#:foo '#:foo)


;; all symbols that are findable with FIND-SYMBOL are 'accessible' in that package
;; in an unqualified manner

;; symbol is present in a package if it has a name-to-symbol entry
;; home package = package into which a symbol was first interned

;; symbol can be accessible if a package inherits it via using another package
;; a package can only inherit another's external symbols
;; symbols are made external by exporting them
;;
;; only one symbol with the same name can be exported
;;
;; only one symbol with the same name can be accessible
;; i.e. can't have present and inherited or two inherited with same name
;; resolve conflicts by making one a shadowing symbol which makes the other
;; same-named symbols inaccessible. Shadowing symbols are kept in a list.

;; you can import a symbol without the whole package
;; this is useful if you're importing two packages that export the same name
;; and you have to import one symbol to make it shadowing and make the other
;; inaccessible.

;; symbols can be uninterned to remove conflicts as well
;; this removes from the name-to-symbol table and the shadowing table


*package*
common-lisp:*package* ; common-lisp-user inherits common-lisp
cl:*package*
;; you can't intern new symbols into cl, which is why common-lisp-user exists

;; all packages you define inherit common lisp so you don't have to fully
;; qualify basic things:
(cl:defun foo ()
  (cl:+ 1 1))

;; things you define go in common-lisp-user by default
(common-lisp-user::foo)


;; keywords go in keyword
:a
keyword:a
(eql keyword:a :a)

;;;;; making packages
;; names of packages and symbols can be string designators
;; string designators are
;; - strings (designates self)
;; - symbol (designates name)
;; - character (designates string of self)
;; keyword symbols are commonly used because they are converted to upcase for you 
(defpackage :com.julian3ng.package-name
  (:use :common-lisp))

(defpackage "COM.JULIAN3NG.PACKAGE-NAME"
  (:use "COMMON-LISP"))

;; to read code in this package...
(in-package :com.julian3ng.package-name)
*package*

;; if this is used in load or compile-file it'll change the package for the rest of the file
foo ; => error
common-lisp-user::foo ; => 0


(in-package :common-lisp-user)
(defun hello-world () (format nil "Hello world"))
(common-lisp-user::hello-world)
(in-package :com.julian3ng.package-name)
(defun com.julian3ng.package-name::hello-world () (format nil "Julian world 2"))
(common-lisp-user::hello-world)
(com.julian3ng.package-name::hello-world)
;; slime: will look behind for in-package to determine fully qualified name
(in-package :cl-user)
(hello-world)
(in-package :com.julian3ng.package-name)
(hello-world)


(defpackage :com.julian3ng.foo
  (:use :common-lisp)
  (:export :foo-this
           :foo-that
           :foo-all))

(in-package :com.julian3ng.foo)
(defun foo-this (x)
  (format nil "Foo this ~A" x))

(defun foo-that (x)
  (format nil "Foo that ~A" x))

(defun foo-all (x)
  (mapcar (lambda (c) (format nil "Foo ~A" c)) x))

(defpackage :com.julian3ng.bar
  (:use :common-lisp :com.julian3ng.foo))

(in-package :com.julian3ng.bar)
(foo-this 1)
(foo-that 2)
(foo-all '(1 2 3))

;; importing names
(defpackage :com.julian3ng.baz
  (:use :common-lisp)
  (:import-from :com.julian3ng.foo :foo-all))

(in-package :com.julian3ng.baz)
(foo-all '(1 2 3))
(foo-this 'x) ; => error

;; importing all but some names
(defpackage :com.julian3ng.qux
  (:use :common-lisp :com.julian3ng.foo)
  (:shadow :foo-this))

(in-package :com.julian3ng.qux)
(foo-that 2)
(foo-this 1) ; => error


;; resolving conflicts
(defpackage :com.julian3ng.x
  (:use :cl)
  (:export :a))

(in-package :com.julian3ng.x)
(defvar a 1)

(defpackage :com.julian3ng.y
  (:use :cl)
  (:export :a))

(in-package :com.julian3ng.y)
(defvar a 2)


(defpackage :com.julian3ng.z
  (:use :cl :com.julian3ng.x :com.julian3ng.y)
  (:shadowing-import-from :com.julian3ng.x :a))

(in-package :com.julian3ng.z)
a ; => resolves to :com.julian3ng.x:a = 1


;; code organization
;; you must defpackage before doing in-package via load or compile-file
;; one way: defpackage in one file per package
;; then, you have to arrange the defpackage files appropriately for dependencies
;;
;; other way: all defpackages in one file
;; then just load that file and then proceed to loading the in-package files
;; just make a file that loads the defpackages, then loads the in-packages


;; defpackage files need (in-package :common-lisp-user)
;; others need (in-package :package-name)
