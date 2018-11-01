;; numbers: arbitrary sizes, exact ratios by default
;; can go to machine world if necessary
;; have complex numbers

10 20/2 #xa #B1010 #b1010 #O12 #o12 ; all are the same object
#20Ra ; base 20

;; floats
1.0 ; normal
1e0 1s0 1f0 1d0 1l0 ; normal, short, float, double, long
1E0 1S0 1F0 1D0 1L0 ; same, but capitalized

#c(1 2) ; 1 + 2i

;; casting: like types go to like types, unless complex with 0i
;; smaller types go to bigger types
;; real types go to complex types

;; truncation: return number to which we truncated and what to add to get back
(floor 1.2) ; towards -infinity
(ceiling 1.2) ; towards +infinity
(round 1.2) ; towards nearest integer
(truncate 1.2) ; towards 0

(mod -7 3) ; add 3s until positive?
(rem -7 3) ; add 3s until next would be positive?

;; 1+, 1- (they don't modify)
(1+ 2)
(1- 2)

;; = : numerical equality, ignores numeric type
(= 1 1.0 2/2 #C(1 0)) ; equal
(/= 1 2.0 3/2 #C(1 2)) ; different
(< 1.0 2.0 5/2) ; order
(<= 1.0 1.0 1.1)
(min 10 20 9 3 5)
(max 10 20 9 3 5)
(evenp 2)
(oddp 2)

(log (exp 1))
(log (expt 2 10) 2)
(sin (/ pi 6))
(cos (/ pi 3))
(tan (/ pi 4))

;; chars
#\a #\Space #\Newline #\Tab #\Return #\Backspace #\Rubout #\Linefeed #\Page

;; char= : case sensitive
;; char-equal: case insensitive
(char= #\  #\Space)
(char= #\a #\A)
(char-equal #\a #\A)

;; char/= char< char> char<= char>= for case sensitive
;; char-not-equal char-lessp char-greaterp char-not-greaterp char-not-lessp

;; strings
;; actually an array of chars
"array \"of\" chars"
(format t "array \"of\" chars~%")

;; comparisons: same as char but with 'string'
;; Only two strings allowed because they allow substring handling
(string= "hello" "hell")
(string= "hello" "hell" :end1 4)
