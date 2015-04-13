;;;; # Functions
;;;
;;; Three most basic components of all Lisp programs:
;;; functions, variables, macros.
;;; Lisp isn't a pure functional language -- Common Lisp was designed to support a variety of styles. Scheme is the closest to pure functional, but still falls short. True pure functional languages include Haskell and ML.
;;;
;;; ## Defining New Functions
;;; Skel:

;;; (defun name (parameter*)
;;;  "Optional documentation string."
;;;  body-form*)

;;; Any symbol is usable as function name. Usually contain letters and hyphens, but other chars are allowed by convention.
;;; -> means convert, eg, string->widget
;;;
;;; use hyphens, not underscores or inner caps
;;;
;;; Parameter lists are also called *lambda lists*
;;;
;;; DOCUMENTATION function returns the doc string.:
;;; 	(documentation 'foo 'function)
(defun hola-muendo ()
  "hola-muendo returns a mystery formatted string!"
  (format t "hola muendo!"))
(documentation 'hola-muendo 'function)

(defun verbose-sum (x y)
  "Sums two numbers after printing."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

(defun verbose-sum2 (x y)
  "Displays the addends and their sum in a single format string."
  (format t "Summing ~d and ~d: ~d.~%" x y (+ y y))
  )

(format t "Documentation for verbose-sum2:~%")
(format t "~a~%" (documentation 'verbose-sum2 'function))

;;; :: Function Parameter Lists ::
;;; Parameter list, also known as the arguments
;;; Parameters may be optional or keyword passed
;;;
;;; : Optional Parameters: 
;;; &OPTIONAL followed by name of paramters:

(defun foo (a b &optional c d)
  (list a b c d))

;;; But this will return NIL for unspecified values C and D. If you want to ahve it return a default value:

(defun foo-two (a &optional (b 99) (c 'apple))
  (list a b c))

(print (foo-two 4 'yes 99))	; no keyword arguments, reads in sequentially.

;;; More flexibility:

(defun my-rectangle (width &optional (height width)) ; Height param would be the same as the width param
  (* height width))

;;; You can find out if the value to the optional argument was provided by caller or is the default value:

(defun my-foo-three (a &optional (b 99 b-supplied-p))
  (list a b b-supplied-p)) ; b-supplied-p will return a boolean value

;;; :: Rest Parameters ::
;;; Takes arbitrary number of arguments
(defun my-rest (&rest value)
  (list value))

(princ (my-rest 5 4 3 5 'this "some string" 'more-data))
;;;
;;; Review of Keyword arguments
(defun my-month (&key month (days 30))
  "Returns pretty format of month. Requires month and day-count"
  (format t "Month: ~a~%Number of Days: ~d~%" month days))

(my-month :month "Hexembruary" :days 45)

;;; Both &optional and &rest are positional, unlike &key

;;; Mixing different parameter types
;;; All four arg techniques -- &rest, &optional, &key -- and regular
;;; to be used.
;;; combining &OPTIONAL and &REST is good, but using any of these with
;;; &KEY can lead to unexpected behavior
;;;
;;; ## Function Return Values
;;; All functions covered hitherto evaluate to their last return value
;;; But, you can return early and quit out of a function using RETURN-FROM
;;; RETURN-FROM isn't tied to functions, it just returns from a block of code
;;; defined with the BLOCK operator
;;; But, DEFUN automatically wraps the whole function in a block, using
;;; name of the function
;;;
;;; RETURN-FROM can be used to prematurely exit a function, as in the case
;;; of a loop.
;;;
;;; ## Functions as Data (AKA higher-order functions)
;;; Eg: if you pass a function as an argumen to another func,
;;; you can write a general-purpose sorting func, while allowing
;;; caller to provide a function that's responsible for comparing two elems
;;; And this same algorithm can be used with many comparitors
;;;
;;; funcs are just another kind of object. DEFUN is really instantiating a function object with
;;; a name -- you can make nameless functions (LAMBDA)
