#!/usr/bin/env clisp
;(princ '(hello world))

;; Format requires two args: 't' and the string. 't' indicates that it will be sent to STDOUT
;; Interpolation character is the ~ (and possibly others)
(format t "hola muendo!")

;; As in Ruby, the above returns "nil" when executed in repl. All expressions should return a value in Lisp, including side-effect ones like format that output to STDOUT. Format evaluates to itself, which is 'nil'
;; Functions

(defun hello-world ()
  (format t "Hola muendo!"))

;; Yes, in lisp, using - is legal. Can also use special symbols like <>!@#$%^&*
;; Standard Lisp style to use - One can see this in the Emacs methods
;;
;; Loading a function:
(load "hello-world.cl")
(hola)

;;  FASL (fast-load file) utilizes the compile-file function. Similar to above.
(load (compile-file "hello-world.cl"))
