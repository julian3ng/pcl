;; Portable Pathname Library
(defpackage :com.julian.pathnames
  (:use :common-lisp)
  (:export :list-directory
           :file-exists-p
           :directory-pathname-p
           :file-pathname-p
           :pathname-as-directory
           :pathname-as-file
           :walk-directory
           :directory-p
           :file-p))

(in-package :com.julian.pathnames)

;; features #+, #-
;; *features*
;; contains a bunch of features
;; 'feature expressions' use these to include/exclude functionality
;; depending on what features are available at read time
;; #+ reads feature expression and includes if available
;; #- reads feature expression and excludes if available
;; Guaranteed to have :sbcl, :allegro, :clisp, etc. on those
;; implementations.

;; is a thing and isn't :unspecific
(defun component-present-p (value)
  (and value (not (equal value :unspecific))))

(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p))) ; no :name
   (not (component-present-p (pathname-type p))) ; no :type
   p)) ; pathname is not nil

(defun pathname-as-directory (name)
  (let ((pathname (pathname name))) ; pathname of passed value
    (when (wild-pathname-p pathname) ; when wild, can't do anything
      (error "Can't reliably convert wild pathname"))
    ;; a directory pathname has no :name or :type
    (if (not (directory-pathname-p name))
        (make-pathname
         ;; pathname-directory gets directory component if present
         ;; otherwise it's a relative path
         :directory (append (or (pathname-directory pathname) (list :relative))
                            ;; gets last part of path
                            (list (file-namestring pathname)))
         :name nil ; dirs have no name
         :type nil ; dirs have no type
         :defaults pathname) ; everything else from the pathname
        pathname))) ; if it's a directory already just return it

;; fix clisp things
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

;; handle different implementations of listing directories
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc (directory wildcard) (directory (clisp-directories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented"))
  )


(defun pathname-as-file (name)
  ;; get pathname
  (let ((pathname (pathname name)))
    ;; if wild, error out
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathname"))
    ;; if it's a directory name (no name or type)
    (if (directory-pathname-p name)
        ;; get the whole directory ancestry
        (let* ((directory (pathname-directory pathname))
               ;; last is the last child as a list
               ;; first is its name
               ;; pathname makes it a path
               (name-and-type (pathname (first (last directory)))))
          (make-pathname
           :directory (butlast directory) ; ancestry minus last child
           :name (pathname-name name-and-type) ; last child name 
           :type (pathname-type name-and-type) ; last child type
           :defaults pathname))
        pathname)))



(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))

  #-(or sbcl allegro cmu clisp lispworks openmcl)
  (error "file-exists-p not implemented"))


;; walk directory tree
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond ((directory-pathname-p name)
                (when (and directories (funcall test name))
                  (funcall fn name))
                (dolist (x (list-directory name)) (walk x)))
               ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))


(walk-directory ".." (lambda (x) (format t "~A~%" x)))
