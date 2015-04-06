;; === Chapter 4: Syntax and Semantics === ;;
;; Lisp's syntax is obviously different from the 
;; Algol-family of languages . Algol-family of languages includes most modern
;; programming languages, like C/C++, VB, Java, Python, Perl.
;;
;; John McCarthy originally intended to make Lisp more Algol-like (specifically, Fortan), in what he
;; called M-expressions, but he never got around to it.
;;
;; Lisp has two black boxes -- one that translates text into Lisp objects, and another doing the semantics
;; of the language.
;; First is called the "reader", latter is "evaluator"
;;
;; Each black box defines a level of syntax. The reader defines s-expressions
;; S-expression sometimes refers to textual rep., and sometimes the object reslulted from reading the textual rep.
;; 
;; S-Expressions
;; Basic element of the s-expression are *lists* and *atoms*
;; lists have parens, atoms are everything else
;;
;; Comments are treated like whitespace
;;
;; atoms are things like: strings, numbers, names
;; Lisp supports ratios, floating points, integers, negative
;; Lisp also supports complex numbers

;; \ used as an escape character -- escape characters are evaluated with format, eg:
(format t "\"It was wonderful\", she said.")

;; Remember that only double quotes are used for strings. Single quote is used to turn something into data: 

(print (mapcar #'evenp '(1 2 3 4 5 6 7)))

;; hyphenated names common in Lisp. global are wrapped with *, constants are wrapped in +
;; low-level functions have names beginning with % and %%
;;
;; S-Expressions as Lisp forms
;; Any atom or nonlist/empty-list is a legal Lisp form, as well as any list that has a symbol as the first elem
;;
;; :this is an example of a keyword symbol
(defun my-func (&key a (b pi) (c 9))
  (* a b c))

(print (my-func :a 4))

;; Special operators
;; Not all operations are functions, like IF
;; if IF was a function, the evaluator would evaluate from left to right:
(setf x nil)

(if x
  (format t "yes")
  (format t "no"))
;; and both "yes" and "no" would be printed. but if is special, and follows different control paradigm.
;; IF is one of 25 special functions, including QUOTE (an alias for '), LET
;;
;; Macros
;; Macros take s-expressions as argument and returns a Lisp form, in turn evaluated in the place of the macro
;; Macro evoluation has 2 phases: elements of macro form are passed in unevaluated to the macro; then, the form is returned by the macro in an "expansion" form, and evaluted with normal Lisp eval rules
;;
;; Truth, Falsehood, Equality
;; NIL is the only false value -- all else is true. T is canonical true value, and used when you only need a non-NIL value.
;; But NIL is both an atom and a list; it reps falsehood and the empty list
;; all of these evaluate to the same thing: nil, 'nil, '(), (), (quote ())
;;
;; Common lisp has type-specific equality predicates like = (CHAR=)
;; equality operators: EQ, EQL, EQUAL and EQUALP
;;
;; EQ tests object identity like
(defvar i 99)
(eq 3 3) (eq i i) (eq 12 (* 4 3))
;; never us EQ to compare values that may be numberrs or characters. EQ is sensitive to changing with implementations
;;
;; EQL woks like EQ but is guarenteed to regard two objects of same class represeting the same number as equiv.
(eql 1 1) ;; returns true
(eql 1 1.0) ;; returns false -- 1 is an int, 1.0 is a float
;; Seibel recommends using EQL
;;
;; EQUAL and EQUALP work on all objects
;; EQUAL is looser, EQUALP is more discriminating. EQUALP ignores cases when comparing strings
(eql t (equalp "the" "The")) ;; returns T


;; Formatting Lisp Code
;; You should indent properly. Reflect structure of code so you dont have to count parens to see what goes with what
(defun print-list (list)
  (dolist (i list)
  	(format t "item: ~a~%" i)))
;; Most IDEs like SLIME and even VIM will do this for you
;;
;; Comments
;; 4 ;'s is for file header comment
;; 3 ;'s is for a paragraph
;; 2 ;'s for use within code:
(defun my-func2 ()
  ;; two semicolons go here
  (= 64 (* 8 8)))		; one semicolon here
