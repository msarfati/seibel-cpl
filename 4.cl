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
