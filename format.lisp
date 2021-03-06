;; (format stream fmt-string &rest args)

(format t "~$~%" 1.234) ; ~$: print float, default to 2 decimals

;; prefix parameters
(format t "~3$~%" 1.234) ; numeric prefix parameter controls precision
(format t "~v$~%" 3 1.234) ; v prefix parameter uses passed value as precision
(format t "~#$" pi) ; # prefix parameter uses number of remaining format arguments as precision

(format t "~3f~%" 1234.12345)
(format t "~,3f~%" 1234.12345) ; use commas to omit some arguments

;; modifiers : @
;; go AFTER prefix parameters
(format t "~d~%" 1000)
(format t "~:d~%" 1000)
(format t "~@d~%" 1000)
(format t "~@:d~%" 1000)

;; specific directives

;; ~A = aesthetic
(format nil "The value is ~A" "10")

;; ~S = sexp?  ~S will output something that is READable
(format nil "The value is ~S" "10")

;; ~A and ~S take : modifier to make nil print as ()
;; take up to 4 prefix parameters for padding

;; ~a,b,c,dA: pad with at least c copies of d, added b copies at a time until width a achieved?
;; @ modifier puts padding on the left instead
(format nil "~5,,,'x@A" 1)

;; ~% and ~&
(format t "~4%") ; newline: prefix makes n newlines
(format t "~4&") ; freshline: prefix ensures n newlines before cursor
(format t "~~")  ; actual tilde

;; ~C
(format nil "~C" #\) ; just like ~a
(format nil "~:C" #\) ; print by name
(format nil "~@C" #\) ; print literal
(format nil "~:@C" #\ ) ; some versions of lisp print how to type the character

;; integers
;; ~D X O B R
(format nil "~20,'0D" 1000) ; ~a,bD (pad a using char b) : commas, @ sign
(format nil "~10,'x,'-,4:D" 1000000) ; ~a,b,c,d:D (pad a using char b delimiter c in groups of d) must be used with :

;; X O B are just hex, octal, binary
(format nil "~10,'0,'-,4:x" 100000)

;; R takes radix, then the same prefix params
(format nil "~17R" 16)

;; floats
;; ~F E G $
;; F does normal float stuff, truncates decimals
(format nil "~,4F" (*  pi 1000))
;; E does scientific notation
(format nil "~,3E" (* pi 1000))
;; $ is for money
(format nil "~3,4$" 3.157) ; first param is after decimal, second is padding before

;; english
;; ~r converts numbers to english

(format nil "~r" 1234) ; english
(format nil "~:r" 1234) ; ordinal
(format nil "~@r" 3999) ; roman numeral
(format nil "~:@r" 49) ; old-style roman numerals: 4 = IIII, 9 = VIIII

;; ~p properly pluralizes words
(format nil "~r wind~p" 1 1)
(format nil "~r wind~:p" 10) ; use : to reprocess the previous argument
(format nil "~r famil~:@p" 10) ; -y or -ies

(format nil "~(LOWERCASE~)")
(format nil "~@(capitalized first~)")
(format nil "~:(capitalized all~)")
(format nil "~:@(uppercase~)")

;; conditional formatting
(format nil "~[zero~;one~;two~:;default~]" 3) ; numeric selection by default

;; # prefix means number of things left to process
(defparameter *list-etc*
  "~#[NONE~;~A~;~A and ~A~:;~A, ~A~]~#[~; and ~A~;, ~A, etc~].")

(format nil *list-etc* 1 2 3 4)


;; : means nil or non-nil
(format nil "~:[a~;b~]" 1)

;; @ means "if arg nil, continue, else unprocess and do contents"
(format nil "~@[~a~]~@[~a~]" nil 1)

;; iteration

(format nil "~{~A ~}" '(1 2 3))
(format nil "~{~A~^, ~}" '(1 2 3)) ; ~^ kills iteration if list ever becomes nil
(format nil "~@{~A~^, ~}" 1 2 3) ; @ treats remaining arguments as a list

;; list - if num args left is 1, print ", and"
(format nil "~{~A~#[~;, and ~:;, ~]~}" '(1 2 3 4))

(defparameter *english-list*
  ;; 0 -> nothing
  ;; 1 -> ~A
  ;; 2 -> ~A and ~A
  ;; else, treat rest as a list and process by
  ;; Iterate through list, with commas if more than 1 arg left, else with ', and'
  "~{~#[~;~A~;~A and ~A~:;~@{~A~#[~;, and ~:;, ~]~}~]~}")

(format nil *english-list* '(1 2))

;; jumping through the arglist
(format nil "~*~A" 1 2) ; skip 1

(format nil "~R ~:*~A" 1) ; print one then print 1
(format nil "I saw ~r~:* el~[ves~;f~:;ves~]" 40)

(format t "~:i~A~%" 1)
