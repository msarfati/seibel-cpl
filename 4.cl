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

(mapcar #'evenp '(1 2 3 4 5 6 7))

;; hyphenated names common in Lisp. global are wrapped with *, constants are wrapped in +
;; low-level functions have names beginning with % and %%
